#' Extract Replica Network Links by Activity ID Groupings
#'
#' @description
#' This function extracts network link-level counts from a Replica trip table,
#' filtered and grouped by a user-defined attribute (such as time of day or spatial tag).
#' It allows for flexible post-processing of a Replica trip dataset by re-tagging or grouping `activity_id`s
#' based on downstream analysis and then querying associated network links for each group.
#'
#' @details
#' The function expects a `raw_trip_table` that already contains a subset of trips from Replica (e.g., trips on Maui).
#' After performing post-hoc classification (e.g., tagging activities by time window or location),
#' users can extract a network profile per unique group in a specified column.
#' This function constructs dynamic SQL queries to query the Replica dataset (via BigQuery) and
#' aggregates the number of trips per network link (`network_link_ids_unnested`) for each group.
#'
#' The function performs the following:
#' - Validates or creates a grouping column (`col_to_split`).
#' - Filters out any groups containing a string (`avoid_str`) to skip.
#' - Constructs SQL queries per group of `activity_id`s.
#' - Executes BigQuery queries to retrieve and aggregate network links.
#' - Returns a combined table of network link usage per group, annotated with context and dataset metadata.
#'
#'
#' @param raw_trip_table A `data.frame` or `data.table` of Replica trips with `activity_id` and optional grouping columns.
#' @param col_to_split Character. Name of the column used to group activities (e.g., "time_group"). If `NULL`, all data is grouped as a single network.
#' @param avoid_str Character. String pattern to exclude groups from processing. Default is `"qqq"` to filter out nothing.
#' @param add_context Character. Descriptive string added as a context column to the result.
#' @param data_set_location Character. Location name (e.g., `"maui"`), used to build the BigQuery table name.
#' @param data_set_type Character. Type of day or dataset (e.g., `"typical"`).
#' @param data_set_period Character. Time period (e.g., `"2022_Q4"`).
#' @param customer_name Character. BigQuery project name (e.g., `"replica_customer"`).
#'
#' @return A `data.table::data.table` containing:
#' - `network_link_ids_unnested`: Link ID
#' - `count`: Aggregated number of trips
#' - `split_col_type`: Name of the grouping column
#' - `split_col_value`: Value of the group
#' - `data_set_location`, `data_set_period`, `data_set_day`, `context`: Metadata columns
#'
#' @importFrom dplyr filter group_by mutate ungroup
#' @importFrom stringr str_glue str_detect
#' @importFrom purrr map pmap
#' @importFrom data.table rbindlist :=
#' @importFrom bigrquery bq_project_query bq_table_download
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming you have already loaded and filtered Replica trips into raw_trip_table
#' trips_tagged = raw_trip_table %>%
#'   dplyr::mutate(time_group = dplyr::case_when(
#'     hour_of_day < 12 ~ "morning",
#'     TRUE ~ "afternoon"
#'   ))
#'
#' network_links = rplc_extract_networks_given_activity_id(
#'   raw_trip_table = trips_tagged,
#'   col_to_split = "time_group",
#'   avoid_str = "test",
#'   add_context = "Maui AM/PM comparison",
#'   data_set_location = "maui",
#'   data_set_type = "typical",
#'   data_set_period = "2022_Q4",
#'   customer_name = "replica_customer"
#' )
#' }
rplc_extract_networks_given_activity_id = function(
    raw_trip_table,
    col_to_split = NULL,
    avoid_str = "qqq",
    add_context,
    data_set_location,
    data_set_type,
    data_set_period,
    customer_name
) {

    trip_table = stringr::str_glue(
    "{customer_name}.{data_set_location}.{data_set_location}_{data_set_period}_{data_set_type}_trip"
  )

  if (!is.null(col_to_split)) {

    check_name_int = which(col_to_split == names(raw_trip_table))

    if (!is_empty(check_name_int)) {
      names(raw_trip_table)[check_name_int] = "temp_split_col"
    } else {
      stopifnot("Did not find splitting column" = FALSE)
    }

  } else {

    message("User did not supply a column to split network with..")
    message("Acquiring network for entire supplied data...")

    raw_trip_table = dplyr::mutate(raw_trip_table, temp_split_col = "full_data_network")

    col_to_split = "full_data_network"
  }

  extracted_trip_links = raw_trip_table %>%
    dplyr::filter(!stringr::str_detect(temp_split_col, avoid_str)) %>%
    dplyr::group_by(temp_split_col_1 = temp_split_col) %>%
    tidyr::nest() %>%
    dplyr::mutate(data_pro = purrr::map(
      .data$data,
      ~{

        # browser()
        temp_var = unique(.x$temp_split_col)

        message(stringr::str_glue("Perfoming analysis for {temp_var}"))

        index_activity_id = .x$activity_id %>%
          paste0("'", ., "'") %>%
          paste0(collapse = ",")

        split_activity = rplc_split_long_string(index_activity_id, max_len = 5e5)
        split_activity = rplc_split_long_string(index_activity_id, max_len = 50)

        message(stringr::str_glue("Activity vector split into {length(split_activity)} batches"))

        extracted_link_vols = list(
          split_activity,
          1:length(split_activity)
        ) %>%
          purrr::pmap(~{
            message(.y)

            # browser()

            query = stringr::str_glue(
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

            table_agg_network_links = bigrquery::bq_project_query(
              customer_name, query, quiet = TRUE)

            temp_table = bigrquery::bq_table_download(table_agg_network_links)

            return(temp_table)
          }) %>%
          data.table::rbindlist() %>%
          .[, .(count = sum(count)), by = .(network_link_ids_unnested)] %>%
          .[, `:=`(
            split_col_type = col_to_split,
            split_col_value = temp_var,
            data_set_location = data_set_location,
            data_set_period = data_set_period,
            data_set_day = data_set_type,
            context = add_context
          )]

        return(extracted_link_vols)
      }
    ))

  extracted_trip_links_pro = extracted_trip_links %>%
    dplyr::ungroup() %>%
    .[["data_pro"]] %>%
    data.table::rbindlist()

  return(extracted_trip_links_pro)
}
