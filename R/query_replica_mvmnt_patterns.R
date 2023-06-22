#' Query movement patterns for a replica network
#'
#' This function queries movement patterns  replica network using a specified \code{network_table},
#' \code{trip_table}, \code{mode_type}, \code{customer_name}, and \code{jitter_factor}. It uses
#' the \code{view_replica_network} function to retrieve the network data, applies a jitter factor to
#' display the network, prompts the user to select links for each movement, performs a table query, and
#' performs quality control and processing operations.
#'
#' @param network_table A character string specifying the name of the table containing network data
#' @param trip_table A character string specifying the name of the table containing trip data
#' @param mode_type A character string specifying the type of mode to query
#' @param customer_name A character string specifying the name of the customer
#' @param jitter_factor A numeric value specifying the amount of jitter to apply to the network display
#' @param mvmnt_df A dataframe describing the turning movements the query will acquire data for and the number of discrete links needed for each movement.
#'
#' @return A processed data frame of movement patterns
#'
#' @examples
#' query_replica_mvmnt_patterns(network_table = "network_data", trip_table = "trip_data",
#'                              mode_type = "PRIVATE_AUTO", customer_name = "ACME",
#'                              jitter_factor = 0.05)
#'
#' @importFrom dplyr %>%
#' @importFrom sf st_as_sf st_jitter selectFeatures pull filter setNames
#' @importFrom mapview mapedit
#' @importFrom glue str_glue
#' @importFrom hcl.colors hcl.colors
#' @importFrom tidyr separate flatten
#' @importFrom magrittr %>%
#' @importFrom purrr pmap reduce
#' @importFrom bigrquery bq_project_query bq_table_nrow bq_table_download
#' @importFrom stringr str_extract_all
#' @importFrom stats runif
#' @importFrom utils readRDS
#' @importFrom here here
#' @importFrom tibble as_tibble
#' @importFrom lubridate parse_date_time
query_replica_mvmnt_patterns <- function(network_table, trip_table
                                         ,mode_type, customer_name, jitter_factor
                                         ,mvmnt_df
                                         ,save_location) {
  mapviewOptions(homebutton = F)
  message("Starting query to get movement pattern data from Replica")

  mode_type_pro = paste0("'", mode_type, "'", collapse = ", ")

  {
    # network_table = "replica-customer.northwest.northwest_2021_Q4_network_segments"
    # trip_table = "replica-customer.northwest.northwest_2021_Q4_thursday_trip"
    # mode_type = c('PRIVATE_AUTO')
    # customer_name = "replica-customer"
    # jitter_factor = 0.003
    # save_location = "data/req_zz"
    # mvmnt_df = data.frame(mvmnt_desc = c("int_15th_to_onramp205","int_15th_to_99E"
    #                                        #,"into_99E_14th_main","int_main_14th_99E"
    #                                        )
    #                       ,ttl_seq = c(2, 2))
  }

  {
    check_dir_path(save_location)
    directory_path = make_dir_prfx_date(save_location, "mvmnt_data_")
    logger = log4r::logger("DEBUG"
                           ,appenders = log4r::file_appender(
                             here::here(directory_path, "log_file.txt")))
    gauntlet::log_and_info("Directories and log file created", logger)
    gauntlet::log_and_info(
      list(
        network_table = network_table
        ,trip_table = trip_table
        ,mode_type = mode_type
        ,customer_name = customer_name
        ,jitter_factor = jitter_factor
        ,user_provided_save_location = save_location
        ,directory_path = directory_path) %>%
        print_named_list() %>%
        paste0("\n", gauntlet::strg_make_space_2(), "Recording user inputs....\n", ., gauntlet::strg_make_space_2(last = T))
      ,logger
    )
  }

  index_network_name_strip = c("Drive", "Boulevard", 'Broadway', 'Center', 'Circle', 'Lane', 'Loop'
                               ,'Park', 'Parkway', 'Place', 'Route', 'Road', 'Square', 'Street', 'View', 'Way') %>%
    paste0(collapse = "|")

  table_network_data = view_replica_study_area_network(
    network_table = network_table
    ,customer_name = customer_name)

  info(logger, "User elected to acquire links... successful")
  info(logger, str_glue("table_network: {replica_temp_tbl_name(table_network_data$table_network)}"))

  table_network_data_sf =  table_network_data[[2]] %>%
    st_as_sf(wkt = "geometry", crs = 4326)

  table_network_data_simp =  table_network_data_sf %>%
    st_drop_geometry() %>%
    select(stableEdgeId, streetName) %>%
    mutate(steetName = str_remove(streetName, index_network_name_strip) %>%
             str_trim())

  check_save_net = robust_prompt_used("to save the network at this point")
  if (check_save_net){
    write_sf(table_network_data_sf, here::here(directory_path, "network_sf.gpkg"))
    message("Network saved..")
  } else {
    message("Table network has been saved to log file\nif you want to download the network at a later point in time..")
  }

  message(str_glue("{gauntlet::strg_make_space_2()}Starting link selection process..."))
  log4r::info(logger, "started link selection")
  message(str_glue("The network you queried will be displayed with a {jitter_factor} jitter factor..."))

  table_network_data_sf_jit = table_network_data_sf %>%
    st_jitter(factor = jitter_factor)

  mapview(table_network_data_sf_jit, zcol = "flags", burst = T)

  rejitter = TRUE
  while (rejitter) {
    rejitter = robust_prompt_used("change the jitter factor and reijitter")

    if (rejitter) {
      jitter_factor = prompt_jitter_factor()

      table_network_data_sf_jit = table_network_data_sf %>%
        st_jitter(factor = jitter_factor)

      mapview(table_network_data_sf_jit, zcol = "flags", burst = T) %>%  print()
    }
  }


  {
    link_selections = list(
      mvmnt_df$mvmnt_desc
      ,mvmnt_df$ttl_seq
    ) %>%
      pmap(~{
        print(str_glue("{make_space_2()}Select links for: {.x}"))
        tmp_list = list()

        for (i in 1:.y){
          print(str_glue("Links for {i} movement..."))
          tmp_list[[str_glue("seq_{i}")]] = table_network_data_sf_jit %>%
            mapedit::selectFeatures() %>%
            pull(stableEdgeId)
          Sys.sleep(2)
        }

        tmp_list_named = setNames(list(tmp_list), .x)

        return(tmp_list_named)
      })

    link_selections_df = link_selections %>%
      flatten() %>%
      flatten_named_list() %>%
      tidyr::separate(col = "name", into = c("intersection", "sequence"), sep = "\\.")

    write_csv(link_selections_df, here::here(directory_path, 'link_selections.csv'))

    link_selections_index_pro = paste0("'", sort(unique(link_selections_df$value)), "'", collapse = ", ")

    # table_network_data_sf %>%
    #   filter(stableEdgeId %in% unique(link_selections_df$value)) %>%
    #   st_as_sf(wkt = "geometry", crs = 4326) %>%
    #   mapview()

    review_map = unique(link_selections_df$intersection) %>%
      map(~{
        link_selections_df %>%
          filter(intersection == .x) %>%
          merge(table_network_data_sf, .
                ,by.x = "stableEdgeId", by.y = "value") %>%
          mapview(zcol = "sequence", layer.name = .x
                  # ,label = "label"
                  ,color = hcl.colors(5, palette = "viridis")
          )
      }) %>%
      reduce(`+`)

    review_map

    #need to have a check here if we want to reselect one of them by name

    message(str_glue("Completed link selection{gauntlet::strg_make_space_2(last  = F)}"))
  }

  #sec: table query
  {
    message(str_glue("{gauntlet::strg_make_space_2()}Starting data acquisition process....."))

    table = bigrquery::bq_project_query(
      customer_name
      ,stringr::str_glue("select * from
(select * except(network_link_ids)
,ROW_NUMBER ()
    OVER (PARTITION BY activity_id) AS link_ord
from `{trip_table}`, unnest(network_link_ids) as network_links
where 1=1
and mode in ('PRIVATE_AUTO')
)
where 1 = 1
and network_links in ({link_selections_index_pro});"))

table_pro = bigrquery::bq_project_query(
  customer_name
  ,stringr::str_glue("select
activity_id,mode, network_links,vehicle_type, link_ord
,ROW_NUMBER ()
    OVER (PARTITION BY activity_id) AS seq_ord
,count(*)
    OVER (PARTITION BY activity_id) AS act_link_count
from {replica_temp_tbl_name(table)}
order by activity_id, link_ord;"))

info(logger, str_glue("queired_mvmnt_trips: {replica_temp_tbl_name(table_pro)}"))

message(str_glue("Query resulted in {bigrquery::bq_table_nrow(table_pro)} through identified movement patterns"))

check_continue = robust_prompt_used("conntinue and download")
stopifnot("Aborted" = check_continue)

turning_links = bigrquery::bq_table_download(table_pro
                                             ,page_size = 1000,
                                             quiet = F)

  }

  #sec: perform QC and processing operation
  {
    message(str_glue("{gauntlet::strg_make_space_2()}Starting data V&V process....."))

    processed_mvmnt_links = unique(link_selections_df$intersection) %>%
      map_df(~{
        link_sub = link_selections_df %>%
          filter(intersection == x)

        index_max_seq = max(parse_number(link_sub$sequence))

        tl_sub = turning_links %>%
          filter(network_links %in% link_sub$value) %>%
          arrange(activity_id, seq_ord ) %>%
          group_by(activity_id) %>%
          mutate(seq_ord_rltv = row_number()) %>%
          ungroup() %>%
          group_by(activity_id) %>%
          filter(n() == index_max_seq) %>%
          ungroup()

        index_order_checks = c(1:index_max_seq) %>%
          c(1:2) %>%
          map(~{
            tl_sub %>%
              filter(seq_ord_rltv == .x) %>%
              filter(network_links %in% (link_sub %>%
                                           filter(parse_number(sequence) == .x) %>%
                                           pull(value))) %>%
              pull(activity_id)
          }, .progress = "Checking link sequences")

        index_activity = index_order_checks[[1]]

        for (i in 1:(index_max_seq-1)){
          index_activity = intersect(index_activity, index_order_checks[[i+1]])
        }

        tl_sub_pro = tl_sub %>%
          filter(activity_id %in% index_activity) %>%
          group_by(activity_id) %>%
          mutate(seq_ord = str_glue("seq_{row_number()}_link_id"))

        tmp_tl = tl_sub_pro %>%
          arrange(activity_id, seq_ord) %>%
          select(activity_id, mode, network_links, seq_ord)

        index_seq = unique(tmp_tl$seq_ord) %>% sort()

        tmp_tl_agg = tmp_tl %>%
          pivot_wider(names_from = "seq_ord", values_from = "network_links") %>%
          mutate(count = 1) %>%
          group_by(mode, across(starts_with("seq_"))) %>%
          summarise(count = sum(count)) %>%
          ungroup()

        tmp_tl_agg_comb_sf = tmp_tl_agg %>%
          merge_cols() %>%
          bind_cols_prefix() %>%
          merge(table_network_data[[2]] %>%
                  select(stableEdgeId, geometry), .
                ,by.x = "stableEdgeId", by.y = "link_id") %>%
          mutate(label = str_glue("<b>{mvmnt_desc_fl}</b><br>Full Link List: {mvmnt_desc}<br>Count: {count}<br>Street Name: {streetName}<br>Link Order: {order}")
                 ,order = as.factor(order)) %>%
          mutate(flag_grp = str_glue("{mvmnt_desc}_{count}")) %>%
          st_as_sf(wkt = "geometry", crs = 4326) %>%
          arrange(mvmnt_desc, count, order)
      }, .progress = "Perfroming intersection quality checks")
  }


  write_sf(processed_mvmnt_links, here::here(directory_location, 'processed_mvmnt_links.gpkg'))

  return(processed_mvmnt_links)
}