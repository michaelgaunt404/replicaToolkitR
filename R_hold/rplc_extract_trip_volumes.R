#' Extract and Aggregate Replica Trip Network Volumes
#'
#' @description
#' Subsets previously extracted Replica trip data and aggregates trip volumes
#' to network links. This function helps generate a summarized view of trip
#' activity along the network based on `activity_id` values from an existing
#' trip subset.
#'
#' @details
#' This function:
#' \itemize{
#'   \item Loads a previously saved trip subset (\code{table_trips_subset.qs}).
#'   \item Adds peak period and school-time flags.
#'   \item Groups the data by peak period (\code{flag_peak_am}) and queries
#'   BigQuery to unnest network link usage for each group of \code{activity_id}s.
#'   \item Handles large numbers of \code{activity_id}s by batching them with
#'   \code{split_long_string()}.
#'   \item Aggregates counts by link and writes both `.csv` and `.qs` outputs.
#' }
#'
#' Currently, only network volumes are extracted. The function is structured for
#' future expansion to include other trip-level attributes.
#'
#' @param input_process_folder Character. Path to processed Replica trip data
#' folder. Must contain a `table_trips_subset.qs` file.
#' @param customer_name Character. BigQuery project/customer name.
#' @param data_set_location Character. Dataset location identifier
#' (e.g., "HTR_maui").
#' @param data_set_day Character. Suffix for identifying trip tables
#' (e.g., "2024_Q2_cars").
#' @param file_write_prefix Character. Prefix for output files.
#' @param avoid_str Character. A value to filter out subsets based on
#' \code{flag_peak_am}. Default = "qqqq".
#'
#' @return A \code{data.table::data.table} with columns:
#' \itemize{
#'   \item \code{network_link_ids_unnested}: Replica network segment ID.
#'   \item \code{count}: Aggregated number of trips over each link.
#'   \item \code{flag_peak_am}: Peak period flag.
#'   \item \code{data_set_period}, \code{data_set_day}, \code{process_island}:
#'   Metadata fields.
#' }
#'
#' @importFrom data.table data.table rbindlist fwrite
#' @importFrom dplyr case_when filter group_by group_map
#' @importFrom purrr map
#' @importFrom qs qread qsave
#' @importFrom stringr str_detect str_trunc str_count str_glue
#' @importFrom bigrquery bq_project_query bq_table_download
#' @importFrom here here
#'
#' @seealso
#' \code{\link{rplc_postprocess_trips_ods}} for generating the trip subset.
#'
#' @note
#' Assumes existence of an internal function \code{split_long_string()} to batch
#' large \code{activity_id} vectors for SQL queries.
#'
#' @family rplc_
#'
#' @examples
#' \dontrun{
#' rplc_extract_trip_volumes(
#'   input_process_folder = "data/data_thru_bbox_...",
#'   customer_name = "my_project",
#'   data_set_location = "HTR_maui",
#'   data_set_day = "2024_Q2_cars",
#'   file_write_prefix = "volumes_output"
#' )
#' }
rplc_extract_trip_volumes <- function(
    input_process_folder,
    customer_name,
    data_set_location,
    data_set_day,
    file_write_prefix,
    avoid_str = "qqqq"
) {

  process_island <- input_process_folder %>%
    gsub("data/data_thru_bbox_.*", "\\1", .) %>%
    basename()

  message(stringr::str_glue("{gauntlet::strg_make_space_2()}Processing data for: {process_island}"))

  data_set_period <- stringr::str_trunc(input_process_folder, 12, "left", "") %>%
    gsub("(Q[0-9]).*", "\\1", .)

  message(stringr::str_glue("Processing data period: {data_set_period}"))

  run_files <- list.files(input_process_folder, full.names = TRUE)

  temp_file_trips_subset <- run_files[stringr::str_detect(run_files, "table_trips_subset.qs")]
  temp_log_file <- run_files[stringr::str_detect(run_files, "log_file")]

  trip_subset_raw <- temp_file_trips_subset %>%
    qs::qread() %>%
    data.table::data.table() %>%
    .[, `:=`(
      origin_tract = substr(origin_bgrp, 1, 11),
      destination_tract = substr(destination_bgrp, 1, 11),
      flag_peak_am = dplyr::case_when(
        start_local_hour >= 7 & start_local_hour < 9 ~ "am_peak",
        start_local_hour >= 16 & start_local_hour < 19 ~ "pm_peak",
        TRUE ~ "untracked"
      ),
      flag_school_time = dplyr::case_when(
        start_local_hour >= 8 & start_local_hour < 15 ~ "school_time",
        TRUE ~ "non_school_time"
      )
    )]

  trip_table <- stringr::str_glue("{customer_name}.{data_set_location}.{data_set_location}_{data_set_period}_{data_set_day}_trip")

  extracted_trip_links <- trip_subset_raw %>%
    dplyr::filter(!stringr::str_detect(flag_peak_am, avoid_str)) %>%
    dplyr::group_by(flag_peak_am_1 = flag_peak_am) %>%
    dplyr::group_map(~{
      temp_flag_peak_am <- unique(.x$flag_peak_am)

      message(stringr::str_glue("Perfoming analysis for {temp_flag_peak_am}"))

      index_activity_id <- .x$activity_id %>%
        paste0("'", ., "'") %>%
        paste0(collapse = ",")

      split_activity <- split_long_string(index_activity_id, max_len = 5e5)

      message(stringr::str_glue("Activity vector split into {length(split_activity)} batches"))

      extracted_link_vols <- split_activity %>%
        purrr::map(~{
          query <- stringr::str_glue(
            "select network_link_ids_unnested, count(*) as count
             from (
               select activity_id, network_link_ids_unnested
               from (
                 select *, ROW_NUMBER() OVER (PARTITION BY activity_id) AS index
                 from {trip_table}
                 where activity_id in ({.x})
               ) as foo1,
               unnest(network_link_ids) as network_link_ids_unnested
             ) as foo2
             group by network_link_ids_unnested"
          )

          table_agg_network_links <- bigrquery::bq_project_query(customer_name, query)
          temp_table <- bigrquery::bq_table_download(table_agg_network_links)

          temp_table
        }) %>%
        data.table::rbindlist() %>%
        .[, .(count = sum(count)), by = .(network_link_ids_unnested)] %>%
        .[, `:=`(
          flag_peak_am = temp_flag_peak_am,
          data_set_period = data_set_period,
          data_set_day = data_set_day,
          process_island = process_island
        )]

      extracted_link_vols
    }) %>%
    data.table::rbindlist()

  message("Data will be written out here:")
  message(input_process_folder)

  data.table::fwrite(
    extracted_trip_links,
    here::here(input_process_folder, stringr::str_glue("table_agg_network_vols_{file_write_prefix}.csv"))
  )

  qs::qsave(
    extracted_trip_links,
    here::here(input_process_folder, stringr::str_glue("table_agg_network_vols_{file_write_prefix}.qs"))
  )

  return(extracted_trip_links)
}

