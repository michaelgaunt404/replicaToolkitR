query_replica_mvmnt_patterns_3 = function(
    # data_set_location = "south_atlantic"
    # ,data_set_period = "2023_Q2"
    # ,data_set_day = "thursday"
    # ,query_links = c("primary", "secondary")
    # ,query_links_net = c("highway", "corridor", "road", "motorway", "motorway_link", "trunk", "primary", "secondary")
    # ,mode_type = c('COMMERCIAL', 'PRIVATE_AUTO')
    # ,bb_sa_layer
    # ,bb_network_layer
    # ,prefix_origin = "origin"
    # ,prefix_dest = "destination"
    # ,max_record = Inf
    # ,customer_name = "replica-customer"
    # ,save_location = "data/req_zz/tampa_mvmnt_custom"
    # ,jitter_factor = 0.003
    # ,mvmnt_df
    ) {

  mapviewOptions(homebutton = F)
  message("Starting query to get movement pattern data from Replica")

  network_table = str_glue("{customer_name}.{data_set_location}.{data_set_location}_{data_set_period}_network_segments")
  trip_table = str_glue("{customer_name}.{data_set_location}.{data_set_location}_{data_set_period}_{data_set_day}_trip")

  mode_type_pro = paste0("'", mode_type, "'", collapse = ", ")
  links_pro = paste0("'", network_links, "'", collapse = ", ")
  links_pro_net = paste0("'", query_links_net, "'", collapse = ", ")
  replica_test_connection(network_table, trip_table, customer_name)

  #sec: load_process_boundary_object============================================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  {
    message("Loading and processing boundary objects")

    list_wkt_objects = list(
      list(bb_network_layer, bb_sa_layer)
      ,list('network_layer', 'sa_layer')) %>%
      pmap(rplc_layer_extent_loadUnionWkt)
  }

  #sec: example_data_inputs============================================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

  #sec: log_set-up============================================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  {
    # save_location_relative = str_remove(save_location, here::here())
    gauntlet::check_dir_path(directory_path)
    # directory_path = gauntlet::make_dir_prfx_date(save_location, "mvmnt_data_")
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
        # ,jitter_factor = jitter_factor
        ,user_provided_save_location = save_location
        ,directory_path = directory_path) %>%
        print_named_list() %>%
        paste0("\n", gauntlet::strg_make_space_2(), "Recording user inputs....\n", ., gauntlet::strg_make_space_2(last = T))
      ,logger
    )
  }


  #sec: table_query============================================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #NOTE: this section still peerfroms functions that old function did
  #perfroms some additioanl processing needed for the mvmnt links

  {
    temp_mvmnt_trip_subset = rplc_make_mvmnt_trip_subsets(
      link_selections_df, customer_name
      ,trip_table, mode_type_pro, logger)

    table_pro = temp_mvmnt_trip_subset$table_pro
    turning_links = temp_mvmnt_trip_subset$turning_links
  }

  #sec: perform QC and processing operation=========================================================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #note: dont exatly knows what this does
  #----- seems to check that trisp are getting flagged for each mvmnt pattern
  #----- --but for the correct gate checks
  #main outputs are: processed_link_ods_pro and pulled_activity_id_pro
  #probably really easy to turn into function
  {
    message(str_glue("{gauntlet::strg_make_space_2()}Starting data V&V process for mvmnts\n--- Trips will be mapped to turning movement patterns...."))

    processed_link_ods = unique(link_selections_df$mvmnt) %>%
      map(function(x) {

        # browser()
        message(str_glue("Tagging trips for {x}"))

        link_sub = link_selections_df %>%
          filter(mvmnt == x) %>%
          group_by(mvmnt, sequence) %>%
          mutate(max_sel_seq = n())  %>%
          ungroup()

        mvmnt = x
        index_max_seq = max((link_sub$sequence))
        index_max_sel_seq = max(link_sub$sequence)

        tl_sub = unique(link_sub$sequence) %>%
          map_df(function(x) {
            # browser()
            tmp_link_sub = link_sub %>%
              filter(sequence == x)

            turning_links %>%
              filter(network_links %in% tmp_link_sub$stableEdgeId) %>%
              group_by(activity_id) %>%
              mutate(seq_ord_rltv = row_number()) %>%
              ungroup() %>%
              arrange(activity_id, seq_ord_rltv) %>%
              merge(., tmp_link_sub, all = T
                    ,by.x = c("network_links")
                    ,by.y = c("stableEdgeId")) %>%
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
    #note: i think this is an agg table that counts freq of dup trips-mvmnt
    # processed_link_ods_pro %>%
    #   count(activity_id, sequence) %>%
    #   count(activity_id) %>%
    #   count(n)

    pulled_activity_id = processed_link_ods_pro %>%
      pull(activity_id) %>%
      unique()

    pulled_activity_id_pro = paste0("'", pulled_activity_id, "'", collapse = ", ")
    # pulled_activity_id_pro_mrg = paste0("'", processed_link_ods_pro_upload$activity_id, "'", collapse = ", ")
    # pulled_mvmnt_pro_mrg = paste0("'", processed_link_ods_pro_upload$mvmnt, "'", collapse = ", ")
    # pulled_mvmnt_seq_pro_mrg = paste0("'", processed_link_ods_pro_upload$mvmnt_seq, "'", collapse = ", ")

  }

  #sec: assign_mvmnts_to_trips=========================================================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #note: required extensive refactor - mg20241011
  #----- substantial issue with queiry strings being too long and hitting limit
  #----- implemente dfix that breaks strings into chunks then maps over shuunks

  table_activity_mvmnt_seq_list_comb = rplc_assign_mvmntSeq_to_trips(
    logger, customer_name, table_pro
    ,processed_link_ods_pro
  ) %>%
    .[["table_activity_mvmnt_seq_list_comb"]]


  #sec: standard tables=========================================================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  gauntlet::log_and_info("Starting Google query now...", logger)


  ##sub: base tables=========================================================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ###sub: network table=========================================================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##create network table and perform checks
  {
    temp_table_object = rplc_make_network_tables(
      logger, wkt_object = list_wkt_objects[[1]]
      ,links_pro, customer_name, network_table)

    table_network = temp_table_object$table_network
    highway_counts = temp_table_object$highway_counts
  }

  ###sub: poly table=========================================================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##create polygon table for block groups given study area
  {
    table_sa_poly_index = createStudyAreaSubset(
      customer_name = customer_name
      ,wkt_geometry = list_wkt_objects[[2]]
    )

    log4r::info(logger, stringr::str_glue("table_sa_poly_index: {replica_temp_tbl_name(table_sa_poly_index)}"))
  }

  ###sub: poly_subset=========================================================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  {
    table_sa_poly_index = createStudyAreaSubset(
      customer_name = customer_name
      ,wkt_geometry = list_wkt_objects[[2]]
    )

    log4r::info(logger, stringr::str_glue("table_sa_poly_index: {replica_temp_tbl_name(table_sa_poly_index)}"))
  }

  ##sub: combined_trip_table=========================================================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ###CREATE: combined_trip_table---
  #make code that follows traditional code but just with specific
  #activity ids from this pulled_activity_id_pro
  #--this one is also expensive - i think it requires the raw trip table
  #----potentially able to be fixed f
  table_full_list_trips = sql_createTripsThruZoneTable_mvmntPat_2(
    customer_name = customer_name
    ,trip_table = trip_table
    ,activity_id_pro = pulled_activity_id_pro
    ,table_sa_poly_index = table_sa_poly_index
    ,table_activity_mvmnt_seq_list_comb = table_activity_mvmnt_seq_list_comb)

  #makes cleaned list of trip table
  #can export raw to get metrics for trips given movement
  query <- stringr::str_glue("select *  except(network_link_ids, geometry, transit_route_ids) from  {replica_temp_tbl_name(table_full_list_trips)};")
  table_full_list_trips_clean <- bigrquery::bq_project_query(customer_name, query)
  log4r::info(logger, stringr::str_glue("table_full_list_trips_clean: {replica_temp_tbl_name(table_full_list_trips_clean)}"))

  ###CREATE: OD tables---
  table_simple_origin_destination = createODBlockgroupTable(
    customer_name = customer_name
    ,table_trips_thru_zone = table_full_list_trips
    ,mvmnt_query = T
  )

  log4r::info(logger, stringr::str_glue("table_simple_origin_destination: {replica_temp_tbl_name(table_simple_origin_destination)}"))


  ##sub: network volumes=========================================================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ###CREATE: Volume tables---

  ###sub: make_network_volums=========================================================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

  ###sub: network_reduction=========================================================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #message here to reduce the size of the network!
  {
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




  ##sub: data_download=========================================================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #data_download
  {
    tryCatch({

      message(str_glue("Starting data download now..... Getting {gsub(here::here(), '\\1', directory_path)}"))

      if (gauntlet::robust_prompt_used("download the network links")){
        data_1 = rplc_save_out_table(table_network, directory_path, "replica_queried_network.csv")
      } else {
        gauntlet::log_and_warn("User did not elect to download the network at function runtime.....")
        data_1 = data.frame(file = "replica_queried_network.csv")
      }

      data_2 = rplc_save_out_table(table_sa_poly_index, directory_path, "replica_sa_poly_index.csv")
      data_3 = rplc_save_out_table(table_agg_by_link_subset_limited, directory_path, "table_agg_by_link_subset_limited.csv")
      data_4 = rplc_save_out_table(table_simple_origin_destination, directory_path, "table_simple_origin_destination.csv")
      data_5 = rplc_save_out_table(
        table = table_full_list_trips_clean
        ,save_location = directory_path
        , file_name = "table_full_list_trips_clean.csv")

      process_data_summary = bind_rows(
        data_1, data_2, data_3, data_4, data_5
      )

      message("All data downloaded.....")

    }, error = function(e) {
      message(paste("Error occured in data download:\n",
                    e$message))
      return(NA)
    })

  }

  #perform checks
  {
    message("Performing data checks now.....")

    # link_merge_check = check_links_download(
    #   location = file_destination
    #   ,folder = directory_path
    # )

    # here(directory_path, "replica_network_links_without_trip_volumes.csv") %>%
    #   write.csv(link_merge_check
    #             ,file = ., row.names = F)

  }

  #sec: post_processing=========================================================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #converts items to spatial layers
  {
    folder = basename(directory_path)
    location = str_remove(directory_path, folder)

    acquired_sa_polys = get_tigris_polys_from_replica_index(
      location = location
      ,folder = folder
      ,states = "WA"
      ,auto_save = T)

    network = make_network_link_layer(
      location = location
      ,folder = folder
      ,auto_save = T)

    replica_queried_network_cntds = make_network_centroid_layer(
      location = location
      ,folder = folder
      ,auto_save = T)
  }

}
