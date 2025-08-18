#' @title Easily query and download Replica trip and network data using the extent of a map layer.
#'
#' @description This function takes a bounding box and Google Big Query inputs and returns data back to you.
#' Function currently only works for data using block groups as it is hardcoded in some of the queries. Changes can be made to the query such that other polygon shapes can be used.
#'
#' This function creates a log file that records inputs, the query sent to Google, and IDs for each Google table that is created in the multi-step query. All of this information can be accessed at later points in time. Table IDs in the log file can be used for auditing purposes or copied and pasted for use in the Google Big Query console GUI.
#'
#' Reprinted: 20231107
#'
#' @param bb_network_layer a string of the relative path to the bounding box object detailing the extent of the road network. This will be used to query trips with. This should be made using sf::st_bbox().
#' @param bb_sa_layer a string of the relative path to the bounding box object detailing the extent of the study area. This will be used to perform aggregations. This should be made using sf::st_bbox(). This input is generally larger or identical to [bb_network_layer].
#' @param query_links a vector of character strings detailing which type of network links to use in the query. These will be used to depict any network link volume graphics and to filter trips. For the latter, any trip that uses these roads within the study area will be included in any reporting.
#' @param mode_type a vector of character strings detailing which mode to query, e.g., 'PASSENGER_CAR', 'PRIVATE_AUTO', 'COMMERCIAL', etc.
#' @param data_set_location a string indicating the region or location table to use, e.g., "northwest."
#' @param data_set_period a string in year_quarter format, e.g., "2023_Q1."
#' @param data_set_day a string that should be either "thursday" or "saturday."
#' @param customer_name a character string indicating the Google account that will be billed.
#' @param prefix_origin a string defining the prefix for Latin long coordinates for each record for the origin (e.g., "origin").
#' @param prefix_dest a string defining the prefix for Latin long coordinates for each record for the destination (e.g., "destination").
#' @param input_folder_write a character string indicating a directory where you want log files and data to be saved in CSV format. A new folder containing function outputs will be created there, named using the convention 'data_[sys.datetime].'
#' @param query_network a boolean (T/F) indicating whether the network should be downloaded at function runtime. The default is 'T,' but it can be beneficial to wait and see how many links will be downloaded before downloading the network. The network can also be downloaded later as log files record big query table IDs.
#' @param max_record an integer indicating the maximum number of records to be written out for each data acquired by Google. The default is 1000. Use Inf if you do not want to limit the download. This is useful if you want to check the data before downloading large amounts of data. Google tables are still made in full so they can be manually acquired without rerunning this function (see log files).
#'
#' @return This function does not return any objects in R. It creates a log file and data in the folder destination in CSV format.
#' @export
#'
#' @examples
#'
#' #none
query_network_trip_using_bbox = function(
    bb_network_layer
    ,bb_sa_layer
    ,query_links = c('highway','corridor','road'
                     ,'motorway','motorway_link'
                     ,'trunk','trunk_link'
                     ,'primary','primary_link'
                     ,'secondary','secondary_link'
                     ,'tertiary','tertiary_link')
    ,mode_type = c('PASSENGER_CAR','PRIVATE_AUTO'
                   ,'COMMERCIAL','CARPOOL'
                   ,'WALKING','BIKING','PUBLIC_TRANSIT'
                   ,'ON_DEMAND_AUTO','OTHER_TRAVEL_MODE')
    ,data_set_location
    ,data_set_period
    ,data_set_day
    ,customer_name
    ,prefix_origin = "origin"
    ,prefix_dest = "destination"
    # ,folder
    ,input_folder_write
    ,input_folder_suffix = NULL
    ,max_record = 1000
    ,input_highway_regrex = NULL){

  #commented out inputs
  {
    # input_folder_write = "req_test/data"
    # data_set_location = "northwest"
    # data_set_period = "2021_Q4"
    # data_set_day = "thursday"
    # customer_name = "replica-customer"
    # mode_type = c('BIKING')
    # query_links = c("primary", "secondary", "tertiary", "footway")
    # prefix_origin = "origin"v
    # prefix_dest = "destination"
    # bb_sa_layer = mapedit::drawFeatures() %>% st_transform(4326)
    # bb_network_layer = mapedit::drawFeatures() %>% st_transform(4326)

  }


  #sec: Initialization Block====================================================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  {
    # Notify initialization
    message(stringr::str_glue("{gauntlet::strg_make_space_2()}{gauntlet::strg_make_space_2()}REPLICATOOLKITR: Initializing function run............"))

    # ── Create references to tables in BigQuery ──
    network_table <- str_glue("{customer_name}.{data_set_location}.{data_set_location}_{data_set_period}_network_segments")
    trip_table    <- str_glue("{customer_name}.{data_set_location}.{data_set_location}_{data_set_period}_{data_set_day}_trip")
    person_table  <- str_glue("{customer_name}.{data_set_location}.{data_set_location}_{data_set_period}_population")

    # ── Validate access to required tables ──
    rplc_checkValidTableConnections(
      prefix_origin = prefix_origin,
      prefix_dest = prefix_dest,
      network_table = network_table,
      trip_table = trip_table
    )

    # ── Get current timestamp for folder naming ──
    query_start <- gauntlet::strg_clean_datetime()

    # ── Construct the folder path where results/logs will be written ──
    folder <- if (is.null(input_folder_suffix)) {
      here::here(input_folder_write, str_glue("data_thru_bbox_{query_start}"))
    } else {
      here::here(input_folder_write, str_glue("data_thru_bbox_{query_start}_{input_folder_suffix}"))
    }

    # ── Prevent overwriting an existing folder ──
    stopifnot("Folder location exists already, stop and prevent overwriting files" = !dir.exists(folder))
    dir.create(folder)

    # ── Initialize log file and logger ──
    log_file <- here::here(folder, "log_file.txt")  # fixed missing quotes around log_file.txt
    logger <- log4r::logger("DEBUG", appenders = log4r::file_appender(log_file))
    log4r::info(logger, "Query started")
    message(str_glue('Query started at {query_start}\nFile path to log file:\n{log_file}'))

    # ── Construct SQL 'WHERE' clause components from vectors ──
    mode_type_pro <- paste0("'", mode_type, "'", collapse = ", ")
    links_pro <- paste0("'", query_links, "'", collapse = ", ")
    links_where_statement <- stringr::str_glue("where highway in ({links_pro})")

    # ── Final message indicating successful setup ──
    message(str_glue("Initial set up complete\nNo fatal errors detected{gauntlet::strg_make_space_2(last = F)}"))

    # ── Optional: Uncomment this block for detailed query logging ──
    # log4r::info(logger, stringr::str_glue(
    #   "{gauntlet::strg_make_space_2()}\nLogging Query Inputs\n" %>%
    #   "Path to network boundary file: {bb_network_layer}\n" %>%
    #   "Path to study area boundary file: {bb_sa_layer}\n" %>%
    #   "Customer Name: {customer_name}\n" %>%
    #   "Schema Table: {trip_table}\n" %>%
    #   "Links Provided:{strg_make_space_2('-', n = 10)}\n" %>%
    #   "{paste0(stringr::str_glue('{sort(query_links)}'), collapse = '\n')}" %>%
    #   "{strg_make_space_2('-', n = 10)}"
    # ))
  }


  #sec: Spatial Tables==========================================================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #load_process_boundary_object
  {
    message("Loading and processing boundary objects")

    list_wkt_objects = list(
      list(bb_network_layer, bb_sa_layer)
      ,list("network_layer", "sa_layer")) %>%
      pmap(rplc_layer_extent_loadUnionWkt)

  }

  gauntlet::log_and_info("Starting Google query now...", logger)

  #create network table and perfrom checks
  {
    table_network = sql_createNetworkTable(
      customer_name = customer_name
      ,network_table = network_table
      ,links_pro = links_pro
      ,wkt_object = list_wkt_objects[[1]]
      ,highway_regrex = input_highway_regrex
    )

    highway_counts = sql_createNetworkLinkCountTable(
      customer_name = customer_name
      ,table_network = table_network
    )

    check_and_log_queired_links(
      counts_object = highway_counts
      ,query_links = links_pro
      ,logger_object = logger
    )

  }

  ##create polygon table for block groups given study area
  {
    table_sa_poly_index = sql_createStudyAreaSubset(
      customer_name = customer_name
      ,wkt_geometry = list_wkt_objects[[2]]
    )

    log4r::info(logger, stringr::str_glue("table_sa_poly_index: {replica_temp_tbl_name(table_sa_poly_index)}"))
  }

  #sec: Trip Tables=============================================================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ###CREATE: trip subset====
  #returns only trips that have a network link in subset of links
  #performs big cross=reference/filtering operation
  {
    table_trip_network_match = sql_createTripNetworkMatchTable(
      customer_name = customer_name
      ,trip_table = trip_table
      ,mode_type_pro = mode_type_pro
      ,table_network = table_network)

    log4r::info(logger,stringr::str_glue("table_trip_network_match: {replica_temp_tbl_name(table_trip_network_match)}"))
  }

  ###CREATE: thruzone trips====
  {
    table_trips_thru_zone = sql_createTripsThruZoneTable(
      customer_name = customer_name
      ,trip_table = trip_table
      ,mode_type_pro = mode_type_pro
      ,table_sa_poly_index = table_sa_poly_index)

    log4r::info(logger,stringr::str_glue("table_trips_thru_zone: {replica_temp_tbl_name(table_trips_thru_zone)}"))

    message(stringr::str_glue("{strg_make_space_2()}Initial queries complete, starting aggregation queries now...."))

    table_trips_subset = bigrquery::bq_project_query(
      customer_name
      ,stringr::str_glue('select * except(network_link_ids, transit_route_ids, geometry)
from {replica_temp_tbl_name(table_trips_thru_zone)};'))
  }


  ###CREATE: OD tables====
  {
    table_simple_origin_destination = sql_createODBlockgroupTable(
      customer_name = customer_name
      ,table_trips_thru_zone = table_trips_thru_zone)

    log4r::info(logger, stringr::str_glue("table_simple_origin_destination: {replica_temp_tbl_name(table_simple_origin_destination)}"))
  }


  #sec: Network Volume Tables===================================================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ###CREATE: Volume tables---
  {
    # message(stringr::str_glue("{strg_make_space_2()}\nOrigin and Destination aggreations commencing...."))


    #subset network count
    {
      table_agg_by_link_subset = sql_createAggNetworkLinksTable(
        customer_name = customer_name
        ,table_trips_thru_zone = table_trips_thru_zone
        ,table_network = table_network)

      log4r::info(logger,stringr::str_glue("table_agg_by_link_subset: {replica_temp_tbl_name(table_agg_by_link_subset)}"))

      summary_table_link_counts = sql_createAggByLinkSumSubsetTable(
        customer_name = customer_name
        ,table_agg_by_link_subset = table_agg_by_link_subset
      )


      #message here to reduce the size of the network!
      gauntlet::log_and_info(
        str_glue("{strg_make_space_2()}\nUser supplied inputs resulted in {gauntlet::strg_pretty_num(sum(summary_table_link_counts$count))} records in link aggregation table....\nSee the following table:{strg_make_space_2('-', 30)}\n{paste0(capture.output(summary_table_link_counts), collapse = '\n')}{strg_make_space_2('-', 30)}\nBy default, links with less than 5 counts on them are removed\n---this would result in downloading {summary_table_link_counts[[3, 6]]} records....\n---An ideal MAXIMUM number of records is ~500,000{gauntlet::strg_make_space_2('-')}")
        ,logger)
      message(stringr::str_glue("If your selection has resulted in too many records, you can............
         1) Decrease the study area layer resulting in less originating polys
         2) Decrease the size of the network layer supplied to the function
         3) Reduce the number of link types queired by the function by changing query_links input
         4) (BEST OPTION) Increase trip volume limit"))

      check_threshold_TF = F
      while (check_threshold_TF == F) {
        check_threshold = readline("Would you like to increase link volume limit to minimize data download? (Y/N)? ")
        # check_threshold = "zz_backdoor"
        check_threshold_TF = (check_threshold %in% c("y", "n", "Y", "N", "zz_backdoor"))
        if (!check_threshold_TF){
          message("Not a valid input... try again")
        }
      }

      if (check_threshold %in% c("n", "N")) {
        log4r::info(logger, "You did not elect to increase link volume threshold, minimum of 5 trips per link will be used..." )

        table_agg_by_link_subset_limited = bigrquery::bq_project_query(
          customer_name
          ,stringr::str_glue("select *
                    from {replica_temp_tbl_name(table_agg_by_link_subset)}
                    where count >= 5"))

        log4r::info(logger,stringr::str_glue("table_agg_by_link_subset_limited: {replica_temp_tbl_name(table_agg_by_link_subset_limited)}"))
      } else if (check_threshold == "zz_backdoor") {
        message(stringr::str_glue("Backdoor access granted. No link count threshold will be applied..."))
        log4r::warn(logger, "Backdoor access granted. No link count threshold will be applied..." )

        table_agg_by_link_subset_limited = bigrquery::bq_project_query(
          customer_name,
          stringr::str_glue("select *
                    from {replica_temp_tbl_name(table_agg_by_link_subset)}")
        )

        log4r::info(logger, stringr::str_glue("table_agg_by_link_subset_limited: {replica_temp_tbl_name(table_agg_by_link_subset_limited)}"))
      } else {

        check_threshold_count_TF = F
        while (check_threshold_count_TF == F) {
          check_threshold_count = readline("Please pick new threshold (integer): ")
          check_threshold_count = as.numeric(check_threshold_count)
          check_threshold_count_TF = (check_threshold_count >= 5)

          if (!check_threshold_count_TF){
            message("Not a valid input, value must be 5 or greater... try again")
          } else {
            message(stringr::str_glue("Thank you, a threshold of {check_threshold_count} will be used..."))
            log4r::warn(logger, "You elected to increase link volume threshold, you supplied a threshold of {check_threshold_count} to be applied" )
          }
        }

        table_agg_by_link_subset_limited = bigrquery::bq_project_query(
          customer_name
          ,stringr::str_glue("select *
                    from {replica_temp_tbl_name(table_agg_by_link_subset)}
                    where count >= {check_threshold_count}"))

        log4r::info(logger,stringr::str_glue("table_agg_by_link_subset_limited: {replica_temp_tbl_name(table_agg_by_link_subset_limited)}"))
      }
    }

  }

  log4r::info(logger,stringr::str_glue("table_agg_by_link_subset: {replica_temp_tbl_name(table_agg_by_link_subset)}"))
  log4r::info(logger,stringr::str_glue("table_agg_by_link_subset_limited: {replica_temp_tbl_name(table_agg_by_link_subset_limited)}"))
  message(stringr::str_glue("Link aggreations complete....{strg_make_space_2()}"))



  #data_download


  #sec: data_extraction====================================================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  list(
    list(table_network, "replica_network")
    ,list(table_sa_poly_index, "replica_study_area_polys")
    ,list(table_simple_origin_destination, "replica_trip_origin_destination")
    ,list(table_agg_by_link_subset_limited, "table_agg_network_vols")
    ,list(table_trips_subset, "table_trips_subset")
  ) %>%
    map(~{
      # browser()

      temp_table = .x[[1]]
      temp_name = .x[[2]]

      check_to_download_bqtable(
        bq_table = temp_table
        ,file_name = temp_name
        ,table_name = temp_name
        ,max_record = max_record
        ,folder = folder
        ,return_obj = F
        ,write_format = c("both")
      )
    })



  # table_trips_thru_zone

  check_aux_tables = gauntlet::robust_prompt_used("Create/download demo and trip chain tables?")

  if(check_aux_tables){
    table_pop_demo  = bigrquery::bq_project_query(
      customer_name
      ,stringr::str_glue('select *
from {person_table}
where 1 = 1
and person_id in (
  select DISTINCT person_id from {replica_temp_tbl_name(table_trips_subset)}
);'))

    temp_table = table_pop_demo
    temp_name = "table_pop_demo"
    check_to_download_bqtable(
      bq_table = temp_table
      ,file_name = temp_name
      ,table_name = temp_name
      ,max_record = max_record
      ,folder = folder
      ,return_obj = F
      ,write_format = c("both")
    )

    table_trips_subset_all_persons_trips = bigrquery::bq_project_query(
      customer_name
      ,stringr::str_glue('select * except (network_link_ids, transit_route_ids)
from {trip_table}
where 1 = 1
and person_id in (
  select DISTINCT person_id from {replica_temp_tbl_name(table_trips_subset)}
);'))

    temp_table = table_trips_subset_all_persons_trips
    temp_name = "table_trips_subset_all_persons_trips"
    check_to_download_bqtable(
      bq_table = temp_table
      ,file_name = temp_name
      ,table_name = temp_name
      ,max_record = max_record
      ,folder = folder
      ,return_obj = F
      ,write_format = c("both")
    )
  }

  message(str_glue("Reminder - data saved to this folder: {basename(folder)}"))

  # #perform checks
  # {
  #   message("Performing data checks now.....")
  #
  #   link_merge_check = check_links_download(
  #     location = input_folder_write
  #     ,folder = stringr::str_glue("data_{query_start}")
  #   )
  #
  #   here(folder, "replica_network_links_without_trip_volumes.csv") %>%
  #     write.csv(link_merge_check
  #               ,file = ., row.names = F)
  #
  # }

}
