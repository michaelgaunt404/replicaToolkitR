#' Query movement patterns for a replica network
#'
#' This function queries movement patterns  replica network using a specified \code{network_table},
#' \code{trip_table}, \code{mode_type}, \code{customer_name}, and \code{jitter_factor}. It uses
#' the \code{view_replica_network} function to retrieve the network data, applies a jitter factor to
#' display the network, prompts the user to select links for each movement, performs a table query, and
#' performs quality control and processing operations.
#'
#'
#' @param data_set_location a string indicating the region or location table to use, e.g., "northwest."
#' @param data_set_period a string in year_quarter format, e.g., "2023_Q1."
#' @param data_set_day a string that should be either "thursday" or "saturday."
#' @param query_links a vector of character strings detailing which type of network links to use when defining movement patterns. Trips that use these links as defined by each movement pattern will be queried.
#' @param query_links_net a vector of character strings detailing which type of network links to use in the query. These will be used to depict any network link volume graphics and to filter trips. For the latter, any trip that uses these roads within the study area will be included in any reporting.
#' @param mode_type a vector of character strings detailing which mode to query, e.g., 'PASSENGER_CAR', 'PRIVATE_AUTO', 'COMMERCIAL', etc.
#' @param bb_sa_layer a string of the relative path to the bounding box object detailing the extent of the study area. This will be used to perform aggregations. This should be made using sf::st_bbox(). This input is generally larger or identical to [bb_network_layer].
#' @param bb_network_layer a string of the relative path to the bounding box object detailing the extent of the road network. This will be used to query trips with. This should be made using sf::st_bbox().
#' @param prefix_origin a string defining the prefix for Latin long coordinates for each record for the origin (e.g., "origin").
#' @param prefix_dest a string defining the prefix for Latin long coordinates for each record for the destination (e.g., "destination").
#' @param max_record an integer indicating the maximum number of records to be written out for each data acquired by Google. The default is 1000. Use Inf if you do not want to limit the download. This is useful if you want to check the data before downloading large amounts of data. Google tables are still made in full so they can be manually acquired without rerunning this function (see log files).
#' @param customer_name a character string indicating the Google account that will be billed.
#' @param save_location a character string indicating a directory where you want log files and data to be saved in CSV format. A new folder containing function outputs will be created there, named using the convention 'data_[sys.datetime].'
#' @param jitter_factor A numeric value specifying the amount of jitter to apply to the network display
#' @param mvmnt_df A dataframe describing the turning movements the query will acquire data for and the number of discrete links needed for each movement.
#'
#' @return A processed data frame of movement patterns
#' @export
#' @examples
#'
#' query_replica_mvmnt_patterns(network_table = "network_data", trip_table = "trip_data",
#'                              mode_type = "PRIVATE_AUTO", customer_name = "ACME",
#'                              jitter_factor = 0.05)
#'
#' @importFrom sf st_as_sf st_jitter
#' @importFrom mapedit selectMap
#' @importFrom stringr str_glue
#' @importFrom grDevices hcl.colors
#' @importFrom tidyr separate
#' @importFrom magrittr %>%
#' @importFrom purrr pmap reduce flatten
#' @importFrom bigrquery bq_project_query bq_table_nrow bq_table_download
#' @importFrom stringr str_extract_all str_glue
#' @importFrom here here
#' @importFrom tibble as_tibble
query_replica_mvmnt_patterns <- function(
    data_set_location = "south_atlantic"
    ,data_set_period = "2023_Q2"
    ,data_set_day = "thursday"
    ,query_links = c("primary", "secondary")
    ,query_links_net = c("highway", "corridor", "road", "motorway", "motorway_link", "trunk", "primary", "secondary")
    ,mode_type = c('COMMERCIAL', 'PRIVATE_AUTO')
    ,bb_sa_layer
    ,bb_network_layer
    ,prefix_origin = "origin"
    ,prefix_dest = "destination"
    ,max_record = Inf
    ,customer_name = "replica-customer"
    ,save_location = "data/req_zz/tampa_mvmnt_custom"
    ,jitter_factor = 0.003
    ,mvmnt_df) {
  mapviewOptions(homebutton = F)
  message("Starting query to get movement pattern data from Replica")

  network_table = str_glue("{customer_name}.{data_set_location}.{data_set_location}_{data_set_period}_network_segments")
  trip_table = str_glue("{customer_name}.{data_set_location}.{data_set_location}_{data_set_period}_{data_set_day}_trip")

  mode_type_pro = paste0("'", mode_type, "'", collapse = ", ")
  links_pro = paste0("'", query_links, "'", collapse = ", ")
  links_pro_net = paste0("'", query_links_net, "'", collapse = ", ")
  replica_test_connection(network_table, trip_table, customer_name)

  #load_process_boundary_object
  {
    message("Loading and processing boundary objects")

    list_wkt_objects = list(
      list(bb_network_layer, bb_sa_layer)
      ,list('network_layer', 'sa_layer')) %>%
      pmap(rplc_layer_extent_loadUnionWkt)
  }

  #NOTE: example data inputs
  {
    # data_set_location = "south_atlantic"
    # data_set_period = "2023_Q2"
    # data_set_day = "thursday"
    # query_links = c("primary" "secondary")
    # query_links_net = c("highway", "corridor", "road", "motorway", "motorway_link", "trunk", "primary", "secondary")
    # mode_type = c('COMMERCIAL', 'PRIVATE_AUTO')
    # bb_sa_layer
    # bb_network_layer
    # prefix_origin = "origin"
    # prefix_dest = "destination"
    # max_record = Inf
    # customer_name = "replica-customer"
    # save_location = "data/req_zz/tampa_mvmnt_custom_1"
    # jitter_factor = 0.003
    # mvmnt_df
  }

  {
    gauntlet::check_dir_path(save_location)
    directory_path = gauntlet::make_dir_prfx_date(save_location, "mvmnt_data_")
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
    ,customer_name = customer_name
    ,links_pro = links_pro)

  info(logger, "User elected to acquire links... successful")
  info(logger, str_glue("table_network: {replica_temp_tbl_name(table_network_data$table_network)}"))

  table_network_data_sf =  table_network_data[[2]] %>%
    sf::st_as_sf(wkt = "geometry", crs = 4326) %>%
    gauntletMap::st_calculate_heading_dataframe("startLat", "startLon", "endLat", "endLon") %>%
    mutate(label = str_glue("{streetName}<br>Type: {highway}<br>Bearing: {bearing}"))

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
    filter(!is.na(streetName)) %>% #this needs to be a selection
    st_jitter(factor = jitter_factor)

  mapview(table_network_data_sf_jit, zcol = "flags", burst = T) %>% print()

  rejitter = TRUE
  while (rejitter) {
    rejitter = robust_prompt_used("change the jitter factor and rejitter")

    if (rejitter) {
      message(str_glue("Previous jitter used was {jitter_factor}"))

      jitter_factor = prompt_jitter_factor()

      table_network_data_sf_jit = table_network_data_sf %>%
        st_jitter(factor = jitter_factor)

      mapview(table_network_data_sf_jit, zcol = "flags", burst = T) %>%  print()
    }
  }

  {
    #make leaflet object here
    map_default_selection = table_network_data_sf_jit %>%
      filter(str_detect(flags, "ALLOWS_CAR")) %>%
      viz_network_link_bearing_map()

    link_selections = list(
      mvmnt_df$mvmnt_desc
      ,mvmnt_df$ttl_seq
      ,mvmnt_df$bearing
    ) %>%
      pmap(function(x, y, z) {
        message(str_glue("{strg_make_space_2()}Select links for: {x}"))
        tmp_list = list()


        if (is.na(z)){
          message("Bearing value NA\n---Using default network with no bearing filtering applied....")
          #just iterate but using the pre-made map
          for (i in 1:y){
            message(str_glue("Select all links for {i} ({i}/{y}) movement pattern..."))
            tmp_list[[str_glue("seq_{i}")]] =
              mapedit::selectMap(map_default_selection
                                 ,styleFalse = list(weight = 5, opacity = 0.7)
                                 ,styleTrue = list(weight = 10, opacity = 1)) %>%
              select(id) %>%
              rename(stableEdgeId = id) %>%
              unique()

            Sys.sleep(2)
          }
        } else {
          # apply the bearing filter and then make map and then select
          message("Bearing value provided\n---Bearing filtering will be applied....")

          map_bearing_selection = table_network_data_sf_jit %>%
            filter(str_detect(flags, "ALLOWS_CAR")) %>%
            filter(eval(parse(text = z))) %>%
            viz_network_link_bearing_map()

          for (i in 1:y){
            message(str_glue("Select all links for {i} ({i}/{y}) movement pattern..."))

            tmp_list[[str_glue("seq_{i}")]] = mapedit::selectMap(map_bearing_selection
                                                                 ,styleFalse = list(weight = 5, opacity = 0.7)
                                                                 ,styleTrue = list(weight = 10, opacity = 1)) %>%
              select(id) %>%
              rename(stableEdgeId = id) %>%
              unique()

            Sys.sleep(2)
          }
        }

        tmp_list_named = setNames(list(tmp_list), x)

        return(tmp_list_named)
      })

    link_selections_df = link_selections %>%
      flatten() %>%
      flatten_named_list() %>%
      tidyr::separate(col = "name", into = c("mvmnt", "sequence"), sep = "\\.") %>%
      group_by(mvmnt, sequence) %>%
      mutate(index_sel_seq = row_number()) %>%
      ungroup()

    #NOTE: should put something here that allows me to skip to this location if I supply this object
    data.table::fwrite(link_selections_df, here::here(directory_path, 'link_selections_df.csv'))

    link_selections_index_pro = paste0("'", sort(unique(link_selections_df$value)), "'", collapse = ", ")

    tmp_review = link_selections_df %>%
      select(mvmnt, sequence) %>%
      unique()

    review_map = list(
      tmp_review$mvmnt
      ,tmp_review$sequence
    ) %>%
      pmap(function(x, y) {
        link_selections_df %>%
          filter(mvmnt == x) %>%
          filter(sequence == y) %>%
          mutate(index_sel_seq = as.integer(index_sel_seq)) %>%
          merge(table_network_data_sf, .
                ,by.x = "stableEdgeId", by.y = "value") %>%
          mapview(zcol = "index_sel_seq", layer.name = str_glue("{x} - {y}")
                  # ,label = "label"
                  ,lwd = 4
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

    #note: this query gets all trips that use any of the links
    #trips that do not meet explicit turning link sequences will be removed
    table_trips_that_use_links = createTipsByLinkIndex(
      customer_name = customer_name
      ,trip_table = trip_table
      ,mode_type_pro = mode_type_pro
      ,link_selections_index_pro = link_selections_index_pro)

    #note: this query processes the data in the above table
    #spec: it creates link order attributes
    table_pro = createTipsByLinkIndexProcessed(
      customer_name = customer_name
      ,table_trips_that_use_links = table_trips_that_use_links
    )

    info(logger, str_glue("queired_mvmnt_trips: {replica_temp_tbl_name(table_pro)}"))

    message(str_glue("Query resulted in {bigrquery::bq_table_nrow(table_pro)} trip/links identified via movement patterns"))

    check_continue = robust_prompt_used("continue and download")
    stopifnot("Aborted" = check_continue)

    turning_links = bigrquery::bq_table_download(
      table_pro
      ,page_size = 1000,
      quiet = F)
  }

  #sec: perform QC and processing operation
  {
    message(str_glue("{gauntlet::strg_make_space_2()}Starting data V&V process for mvmnts\n--- Trips will be mapped to turning movement patterns...."))

    processed_link_ods = unique(link_selections_df$mvmnt) %>%
      map(function(x) {

        message(str_glue("Tagging trips for {x}"))

        link_sub = link_selections_df %>%
          filter(mvmnt == x) %>%
          group_by(mvmnt, sequence) %>%
          mutate(max_sel_seq = n())  %>%
          ungroup()

        mvmnt = x
        index_max_seq = max(parse_number(link_sub$sequence))
        index_max_sel_seq = max(link_sub$index_sel_seq)

        tl_sub = unique(link_sub$sequence) %>%
          map_df(function(x) {
            tmp_link_sub = link_sub %>%
              filter(sequence == x)

            turning_links %>%
              filter(network_links %in% tmp_link_sub$value) %>%
              group_by(activity_id) %>%
              mutate(seq_ord_rltv = row_number()) %>%
              ungroup() %>%
              arrange(activity_id, seq_ord_rltv) %>%
              merge(., tmp_link_sub, all = T
                    ,by.x = c("network_links")
                    ,by.y = c("value")) %>%
              arrange(activity_id, sequence, seq_ord_rltv) %>%
              filter(seq_ord_rltv == index_sel_seq) %>%
              group_by(activity_id, sequence) %>%
              filter(n() >= max(max_sel_seq)) %>%
              ungroup() %>%
              mutate(mvmnt = mvmnt
                     ,mvmnt_seq = x)
          })

        #note:::::might have issue here since prefix presumed to be start/end
        temp = c("start", "end") %>%
          map_df(~{
            tl_sub %>%
              st_as_sf(
                coords = c(
                  paste0(.x, "_lng")
                  ,paste0(.x, "_lat")
                ), crs = 4326
              ) %>%
              mutate(data_set = paste0(mvmnt, "_", .x))
          }) %>%
          select(activity_id, mode, vehicle_type
                 ,end_lng, end_lat, start_lng, start_lat, data_set) %>%
          unique()

        return(tl_sub)
        # list(
        #   temp
        #   # ,agg_od
        #   ))

      }, .progress = "Perfroming mvmnt quality checks")

    processed_link_ods_pro = processed_link_ods %>%
      reduce(bind_rows)

    #noteCHECK -- need to make sure that every activity links to ALL sequences that it should
    #and activity can link to multiple sequences if there are nested movmeent patterns
    processed_link_ods_pro %>%  count(activity_id, sequence) %>% count(activity_id) %>%  pull(n) %>% max

    pulled_activity_id = processed_link_ods_pro %>%
      pull(activity_id) %>%
      unique()

    pulled_activity_id_pro = paste0("'", pulled_activity_id, "'", collapse = ", ")
    # pulled_activity_id_pro_mrg = paste0("'", processed_link_ods_pro_upload$activity_id, "'", collapse = ", ")
    # pulled_mvmnt_pro_mrg = paste0("'", processed_link_ods_pro_upload$mvmnt, "'", collapse = ", ")
    # pulled_mvmnt_seq_pro_mrg = paste0("'", processed_link_ods_pro_upload$mvmnt_seq, "'", collapse = ", ")

    tmp_mvmnt_merge = processed_link_ods_pro %>%
      select(mvmnt, mvmnt_seq) %>%
      unique()

    #note: this creates a crosswalk table for activities and movement locations
    message(str_glue("{gauntlet::strg_make_space_2()}Creating crosswalk table for movements/sequences and activities...."))

    table_activity_mvmnt_seq_list =
      list(
        tmp_mvmnt_merge$mvmnt
        ,tmp_mvmnt_merge$mvmnt_seq
      ) %>%
      pmap(function(x, y) {

        gauntlet::log_and_info(str_glue('Processing: {x} and {y}'), logger)

        pulled_activity_id_pro_mrg = processed_link_ods_pro %>%
          filter(mvmnt == x
                 ,mvmnt_seq == y) %>%
          pull(activity_id) %>%
          unique() %>%
          # head() %>%
          paste0("'", ., "'", collapse = ", ")


        #This uses supplied custom polygons to spatially filter trips
        temp_table_poly_subset = bigrquery::bq_project_query(
          customer_name
          ,stringr::str_glue("select distinct activity_id as activity_id_duplicate
          ,'{x}' as mvmnt
          ,'{y}' as mvmnt_seq
          from (
select *
from `{replica_temp_tbl_name(table_pro)}`
where activity_id in ({pulled_activity_id_pro_mrg}));"))

        gauntlet::log_and_info(str_glue('There are {bigrquery::bq_table_nrow(temp_table_poly_subset)} rows in subset...'), logger)

        return(temp_table_poly_subset)
      })

    table_activity_mvmnt_seq_list_comb = bigrquery::bq_project_query(
      customer_name
      ,stringr::str_glue('{paste0("select * from `", unlist(map(table_activity_mvmnt_seq_list, replica_temp_tbl_name)), "`",  collapse = " union all ")};')) #need space at end

    log4r::info(logger,stringr::str_glue("table_activity_mvmnt_seq_list_comb: {replica_temp_tbl_name(table_activity_mvmnt_seq_list_comb)}"))


  }

  #standard tables
  {
    gauntlet::log_and_info("Starting Google query now...", logger)

    ##create network table and perform checks
    {
      table_network = createNetworkTable(
        customer_name = customer_name
        ,network_table = network_table
        ,links_pro = links_pro_net
        ,wkt_object = list_wkt_objects[[1]]
      )

      highway_counts = createNetworkLinkCountTable(
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
      table_sa_poly_index = createStudyAreaSubset(
        customer_name = customer_name
        ,wkt_geometry = list_wkt_objects[[2]]
      )

      log4r::info(logger, stringr::str_glue("table_sa_poly_index: {replica_temp_tbl_name(table_sa_poly_index)}"))
    }
  }


  #make code that follows traditional code but just with specific
  #activity ids from this pulled_activity_id_pro
  table_full_list_trips = sql_createTripsThruZoneTable_mvmntPat(
    customer_name = customer_name
    ,trip_table = trip_table
    ,activity_id_pro = pulled_activity_id_pro
    ,table_sa_poly_index = table_sa_poly_index
    ,table_activity_mvmnt_seq_list_comb = table_activity_mvmnt_seq_list_comb)

  ###CREATE: OD tables---
  {
    table_simple_origin_destination = createODBlockgroupTable(
      customer_name = customer_name
      ,table_trips_thru_zone = table_full_list_trips
      ,mvmnt_query = T
    )

    log4r::info(logger, stringr::str_glue("table_simple_origin_destination:
                                        {replica_temp_tbl_name(table_simple_origin_destination)}"))
  }


  ###CREATE: Volume tables---
  {
    # message(stringr::str_glue("{make_space()}\nOrigin and Destination aggreations commencing...."))


    #subset network count
    {
      table_agg_by_link_subset = createAggNetworkLinksTable(
        customer_name = customer_name
        ,table_trips_thru_zone = table_full_list_trips
        ,table_network = table_network
        ,mvmnt_query = T)

      log4r::info(logger,stringr::str_glue("table_agg_by_link_subset: {replica_temp_tbl_name(table_agg_by_link_subset)}"))


      summary_table_link_counts = createAggByLinkSumSubsetTable(
        customer_name = customer_name
        ,table_agg_by_link_subset = table_agg_by_link_subset
      )


      #message here to reduce the size of the network!
      gauntlet::log_and_info(
        str_glue("{make_space()}\nUser supplied inputs resulted in {gauntlet::strg_pretty_num(sum(summary_table_link_counts$count))} records in link aggregation table....\nSee the following table:{make_space('-', 30)}\n{paste0(capture.output(summary_table_link_counts), collapse = '\n')}{make_space('-', 30)}\nBy default, links with less than 5 counts on them are removed\n---this would result in downloading {summary_table_link_counts[[3, 6]]} records....\n---An ideal MAXIMUM number of records is ~500,000{gauntlet::make_space('-')}")
        ,logger)
      message(stringr::str_glue("If your selection has resulted in too many records, you can............
         1) Decrease the study area layer resulting in less originating polys
         2) Decrease the size of the network layer supplied to the function
         3) Reduce the number of link types queired by the function by changing query_links input
         4) (BEST OPTION) Increase trip volume limit"))

      check_threshold_TF = F
      while (check_threshold_TF == F) {
        check_threshold  = readline("Would you like to increase link volume limit to minimize data download? (Y/N)? ")
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

  #data_download
  {
    tryCatch({

      message("Starting data download now.....")

      if (gauntlet::robust_prompt_used("download the network links")){
        here(directory_path, "replica_queried_network.csv") %>%
          write.csv(
            bigrquery::bq_table_download(table_network, n_max = max_record)
            , file = ., row.names = F)
      } else {
        gauntlet::log_and_warn("User did not elect to download the network at function runtime.....")
      }

      message("Starting data download now..... replica_sa_poly_index")
      here(directory_path, "replica_sa_poly_index.csv") %>%
        write.csv(
          bigrquery::bq_table_download(table_sa_poly_index, n_max = max_record)
          , file = .)

      #aggregated network link volumes
      message("Starting data download now..... table_agg_by_link_subset_limited")
      here(directory_path, "table_agg_by_link_subset_limited.csv") %>%
        write.csv(
          bigrquery::bq_table_download(table_agg_by_link_subset_limited, n_max = max_record)
          , file = ., row.names = F)

      #writing out simple OD matix based on block groups
      message("Starting data download now..... replica_trip_origin_destination")
      here(directory_path, "replica_trip_origin_destination.csv") %>%
        write.csv(
          bigrquery::bq_table_download(table_simple_origin_destination, n_max = max_record)
          , file = ., row.names = F)

      #raw trip mvmnt/seq table
      message("Starting data download now..... replica_trip_mvmnt_seq_table")
      here(directory_path, "replica_trip_mvmnt_seq_table.csv") %>%
        write.csv(
          processed_link_ods_pro
          , file = ., row.names = F)


      message("All data downloaded.....")

    }, error = function(e) {
      message(paste("Error occured in data download:\n",
                    e$message))
      return(NA)
    })

  }

  #perform checks
  # {
  #   message("Performing data checks now.....")
  #
  #   link_merge_check = check_links_download(
  #     location = file_destination
  #     ,folder = directory_path
  #   )
  #   here(directory_path, "replica_network_links_without_trip_volumes.csv") %>%
  #     write.csv(link_merge_check
  #               ,file = ., row.names = F)
  # }

}
