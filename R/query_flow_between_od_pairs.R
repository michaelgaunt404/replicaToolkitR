query_flow_between_od_pairs = function(
    poly_gen_att
    ,bb_network_layer
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
    ,input_folder_write
    # ,query_network = T
    ,max_record = 1000
){

  #mk: standard_init========~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  {
    message(stringr::str_glue("{gauntlet::strg_make_space_2()}{gauntlet::strg_make_space_2()}REPLICATOOLKITR: Initializing function run..."))
    query_start = gauntlet::strg_clean_datetime(strip = T)
    network_table = str_glue("{customer_name}.{data_set_location}.{data_set_location}_{data_set_period}_network_segments")
    trip_table = str_glue("{customer_name}.{data_set_location}.{data_set_location}_{data_set_period}_{data_set_day}_trip")
    mode_type_pro = paste0("'", mode_type, "'", collapse = ", ")
    links_pro = paste0("'", query_links, "'", collapse = ", ")
    links_where_statement = stringr::str_glue("where highway in ({links_pro})")

    rplc_checkValidTableConnections(
      prefix_origin = prefix_origin, prefix_dest = prefix_dest
      ,network_table = network_table, trip_table = trip_table)

    #create_run_storage_location
    #note: don't need to do this is everything is stored in a single qs object
    folder = here::here(input_folder_write, stringr::str_glue("data_od_flow_{query_start}"))
    stopifnot("Folder location exists already, stop and prevent overwriting files" = (dir.exists(folder) != T))
    dir.create(folder)

    #currently_commented_out
    #note: how i save the contents of file out has implications if I have a log file I think
    #potentially make a temp file - then readlines and then save out with qs
    #i think tho you need a hard copy since that will be there even if there is a failed run
    log_file = here::here(folder, "log_file.txt")
    logger = log4r::logger("DEBUG", appenders = log4r::file_appender(log_file))
    log4r::info(logger, "Query started")
    message(stringr::str_glue('Query started at {query_start}\nFile path to log file:\n{log_file}'))
    #i do think this is redundant - this all can be written in a qs object
    # log4r::info(logger,
    #             stringr::str_glue("{make_space()}\nLogging Query Inputs\nPath to network boundary file: {bb_network_layer}\nPath to study area boundary file: {bb_network_layer}\nCutsomer Name: {customer_name}\nSchema Table: {trip_table}\nLinks Provided:{make_space('-', n = 10)}\n{paste0(stringr::str_glue('{sort(query_links)}'),collapse = '\n')}{make_space('-', n = 10)}"))

    message(str_glue("Initial set up complete\nNo fatal errors detected{gauntlet::strg_make_space_2(last = F)}"))
  }

  #mk: wkt instances of code~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  {
    message("Loading and processing boundary objects")
    list_wkt_objects = list(
      list(bb_network_layer, poly_gen_att)
      ,list("network_layer", "poly_gen_att")
    ) %>%
      pmap(rplc_layer_extent_loadUnionWkt)

    network_wkt = list_wkt_objects[[1]]
    poly_wkt = list_wkt_objects[[2]]
  }


  #create: network_subset~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #note: creates subset of network using study area extent
  #----- returns all roads in study area and then filters only for requested link types
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
      ,table_network = table_network)

    check_and_log_queired_links(
      counts_object = highway_counts
      ,query_links = links_pro
      ,logger_object = logger)
  }


  #create: trip subset~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #note: this makes a table subset for trips that either end or begin in custom polygons
  #---- gets you Int-ext, Ext-int, and Int-int
  #---- ---- does not get ext-ext as does not use bbox
  #---- filter option at end bottom (%_contains = T) keeps only Int-int
  #---- ---- can be changed
  #---- issue might arise if lat/lng suffix are not used but hard coded here
  table_trip_subset = bigrquery::bq_project_query(
    customer_name
    ,stringr::str_glue("select *
      from (
  select *
  ,ST_CONTAINS(
  ST_GEOGFROMTEXT('{poly_wkt}')
  ,ST_GEOGPOINT({prefix_origin}_lng, {prefix_origin}_lat)) as flag_origin_in_sa
  ,ST_CONTAINS(
  ST_GEOGFROMTEXT('{poly_wkt}')
  ,ST_GEOGPOINT({prefix_dest}_lng, {prefix_dest}_lat)) as flag_dest_in_sa
  from `{trip_table}`
  where 1=1
  and mode in ({mode_type_pro})
  )
  where 1=1
  and (flag_origin_in_sa = TRUE
  or flag_dest_in_sa = TRUE);"))

  #mk: query_expand_grid_pairs~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #TODO: this needs to be more robust, issues with this as an input
  #note: mg20240613 I dont really remember what this means
  query_grid = expand.grid(
    generator = poly_gen_att %>% filter(str_detect(group, "generator")) %>% pull(Name)
    ,terminal = poly_gen_att %>% filter(str_detect(group, "terminal")) %>% pull(Name)) %>%
    mutate(across(c(everything()), as.character)) %>%
    unique() %>%
    filter(generator != terminal)



  #TODO: you can make the TRUE inputs at the end of sql to FALSE to get
  #---only emanating or leaving trips
  custom_poly_list =
    list(
      query_grid$generator
      ,query_grid$terminal
    ) %>%
    pmap(~{

      # browser()

      # temp_origin = query_grid$generator[1]
      # temp_destination = query_grid$terminal[1]

      temp_origin = .x
      temp_destination = .y

      #TODO:this needs a try catch
      #especially if query limits are hit
      #need to print and write out tables to script
      message(str_glue("{gauntlet::strg_make_space_2()}Querying {temp_origin} to {temp_destination}"))

      poly_gen_att_unique = poly_gen_att %>% select(Name) %>%  unique()
      origin_wkt = wellknown::sf_convert(poly_gen_att_unique %>%  filter(Name == temp_origin))
      destination_wkt = wellknown::sf_convert(poly_gen_att_unique %>%  filter(Name == temp_destination))


      # --from `{replica_temp_tbl_name(table_trip_subset)}`
# --from `{replica_temp_tbl_name(table_trip_subset)}`

      table = bigrquery::bq_project_query(
        customer_name
        ,stringr::str_glue(
          "select *
        ,'{temp_origin}' as poly_name_origin
        ,'{temp_destination}' as poly_name_destination from (
select *
,ST_CONTAINS(
ST_GEOGFROMTEXT('{origin_wkt}')
,ST_GEOGPOINT(start_lng, start_lat)) as flag_contains_origin
,ST_CONTAINS(
ST_GEOGFROMTEXT('{destination_wkt}')
,ST_GEOGPOINT(end_lng, end_lat)) as flag_contains_destination
from {trip_table}
where 1=1
and mode in ({mode_type_pro})
)
where 1=1
and flag_contains_origin = TRUE
and flag_contains_destination = TRUE
        ;"))

  count = bigrquery::bq_table_nrow(table)

  message(str_glue("Resulted in {count} records"))


  return(table)

    })

#mk: single_trip_table~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# note: consider this the trip subset table, it is literally all the trips that qualify
table_custom_poly_list_data = bigrquery::bq_project_query(
  customer_name
  ,stringr::str_glue('{paste0("select * from `", unlist(map(custom_poly_list, replica_temp_tbl_name)), "`",  collapse = " union all ")};')
)

table_custom_poly_list_data_noLinks = bigrquery::bq_project_query(
  customer_name
  ,stringr::str_glue('select * except(network_link_ids, transit_route_ids) from {replica_temp_tbl_name(table_custom_poly_list_data)};')
)

table_custom_poly_list_data_noLinks_all_persons = bigrquery::bq_project_query(
  customer_name
  ,stringr::str_glue('select * except (network_link_ids, transit_route_ids)
from {trip_table}
where 1 = 1
and person_id in (
  select DISTINCT person_id from {replica_temp_tbl_name(table_custom_poly_list_data_noLinks)}
);')
)

###CREATE: Volume tables---
#subset network count
{
  table_agg_by_link_subset = sql_createAggNetworkLinksTable_flow_btwn_od(
    customer_name = customer_name
    ,table_trips_thru_zone = table_custom_poly_list_data
    ,table_network = table_network)

  log4r::info(logger, stringr::str_glue("table_agg_by_link_subset: {replica_temp_tbl_name(table_agg_by_link_subset)}"))

  summary_table_link_counts = sql_createAggByLinkSumSubsetTable(
    customer_name = customer_name
    ,table_agg_by_link_subset = table_agg_by_link_subset
  )


  #message here to reduce the size of the network!
  gauntlet::log_and_info(
    str_glue("{gauntlet::strg_make_space_2()}User supplied inputs resulted in {gauntlet::strg_pretty_num(sum(summary_table_link_counts$count))} records in link aggregation table....\nSee the following table:{paste0(capture.output(summary_table_link_counts), collapse = '\n')}\n{gauntlet::strg_make_space_2()}By default, links with less than 5 counts on them are removed\n---this would result in downloading {summary_table_link_counts[[3, 6]]} records....\n---An ideal MAXIMUM number of records is ~500,000{gauntlet::strg_make_space_2(last = F)}")
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

#sec: data_extraction====================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
list(
  list(table_network, "replica_network")
  ,list(table_custom_poly_list_data_noLinks, "table_trips_subset")
  ,list(table_custom_poly_list_data_noLinks_all_persons, "table_trips_subset_all_person_trips")
  ,list(table_agg_by_link_subset_limited, "table_agg_network_vols")
) %>%
  map(~{
    # browser()
    temp_table = .x[[1]]
    temp_name = .x[[2]]

    rplc_check_to_download_bqtable(
      bq_table = temp_table
      ,file_name = temp_name
      ,table_name = temp_name
      ,max_record = max_record
      ,folder = folder
      ,return_obj = F
      ,write_format = c("both")
    )
  })
}
