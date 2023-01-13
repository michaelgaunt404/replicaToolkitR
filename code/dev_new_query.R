

library(tidyverse)
library(gauntlet)
library(bigrquery)
library(log4r)
library(here)
library(sf)

query_network_trip_using_bbox = function(
  bb_network_layer
  ,bb_sa_layer
  ,query_links = c('highway','corridor','road'
                   ,'motorway','motorway_link'
                   ,'trunk','trunk_link'
                   ,'primary','primary_link'
                   ,'secondary','secondary_link'
                   ,'tertiary','tertiary_link')
  ,customer_name
  ,trip_table
  ,network_table
  ,file_destination
  ,query_network = T
  ,max_record = 1000){

  #commented out inputs
  {
    # bb_network_layer = "data/req_dev/study_area_network.shp"
    # bb_sa_layer = "data/req_dev/study_area_polys_union.shp"
    # network_table = "replica-customer.northwest.northwest_2021_Q4_network_segments"
    # trip_table = "replica-customer.northwest.northwest_2021_Q4_thursday_trip"
    # customer_name = "replica-customer"
    # file_destination = "data/req_dev"
    # query_links = c('motorway','motorway_link'
    #                 ,'primary','primary_link')
  }

  #init_error_logging_and_setup
  {
    query_start = gauntlet::clean_datetime()
    folder = here::here(file_destination, str_glue("data_{query_start}"))
    stopifnot("Folder location exists already, stop and prevent overwriting files" = (dir.exists(folder) != T))
    dir.create(folder)
    log_file = here::here(file_destination, str_glue("data_{query_start}/log_file.txt"))
    logger = log4r::logger("DEBUG", appenders = file_appender(log_file))
    log4r::info(logger, "Query started")
    message(str_glue('Query started at {query_start}\nFile path to log file:\n{log_file}'))

    log4r::info(logger, qs_log_inputs())

    if (!any(is.na(query_links))){
      message("No NAs detected in links input.... Good")
      links_pro = paste0("'", query_links, "'", collapse = ", ")
      links_where_statement = str_glue("where highway in ({links_pro})")
    } else {
      message("NAs detected, all highway links will be queried.")
      warn(logger, "NAs detected, all highway links will be queried.")
      links_where_statement = ""
    }

    message("Initial set up complete....")
  }

  #load_process_boundary_object
  {
    message("Loading and processing boundary objects")
    list_wkt_objects = list(bb_network_layer
                            ,bb_sa_layer) %>%
      map(~{

        temp_object = sf::read_sf(here::here(.x))

        if (nrow(temp_object) != 1){
          fatal(logger, "More than one polygon in a supplied boundary object detected")
          stopifnot("One of the bounding layers you provided has more than one polygon, please fix this...." = F)
        }

        if (st_crs(temp_object)$input == "EPSG:4326"){
          log_and_warn('CRS for one of the bounding layers you provided is was not set to EPSG:4326, it was converted for you....', logger)
          temp_object = sf::st_transform(temp_object, 4326)
        }
        log_and_info("Bounding box checks pass.... GOOD", logger)
        temp_wkt = wellknown::sf_convert(temp_object)
      })
  }

  ##query google----
  {
    log_and_info("Starting Google query now", logger)

    ###CREATE: network_table and get count----
    #gets links only for certain area defined by user
    #defines list of roads that we want to visualize
    #--could have a problem where a trip in study area is not on roads
    #--but if list is sufficiently large then I think its okay
    {
      message(str_glue("{make_space()}\nCreating network table now...."))

      table_network = bq_project_query(
        customer_name
        ,qs_table_network()
      )

      temp_query = str_glue("SELECT highway, count(*) as count
                      from {replica_temp_tbl_name(table_network)}
                      group by highway
                      order by count;")

      table_network_link_count = bq_project_query(
        customer_name
        ,temp_query
      )

      project_dataset = str_glue("{table_network_link_count$project}.{table_network_link_count$dataset}")

      info(logger, str_glue("Replica project and dataset are: {project_dataset}"))

      highway_counts = bq_table_download(table_network_link_count)

      if ((nrow(highway_counts) == 0)){
        mes_fatal = "Number of returned links was zero\n...change bounding box\n...stopping...."
        fatal(logger, mes_fatal)
        stopifnot("Number of returned links was zero\n...change bounding box\n...stopping...." = F)
      } else {
        str_glue("{make_space()}\nNon-empty data returned, Good\nQuery continued...\nIn total {sum(highway_counts$count)} links\n{str_glue('-{highway_counts$count} ({100*dgt2(highway_counts$count/sum({highway_counts$count}))})% {highway_counts$highway}') %>%
                paste0(collapse = '\n')}") %>%
          log_and_info(., logger)

        index_empty_highways = query_links[-which(highway_counts$highway %in% query_links)]

        if (length(index_empty_highways) == 0){
          log_and_info(str_glue("All requested link types returned with some number of links\nnone empty.... GOOD"),logger)
        } else {
          log_and_warn(
            str_glue("{make_space()}\nThe following link types were requested but not found in bounding box
                 {str_glue('{}') %>% paste0(collapse = '\n')}"),logger)
        }
      }

      info(logger,str_glue("table_network: {replica_temp_tbl_name(table_network)}"))

      check_continuation = readline(prompt = "Would you like to coninute function exectuion (T/F)? ") == "T"

      if (!check_continuation){
        warn(logger, "You have elected to terminate function run..." )
        stopifnot("You have elected to terminate function run..." = check_continuation)
      }

    }

    ###CREATE: poly index and get count----
    #this will only work if they want block groups
    #screw it- itll work for now
    #--ACTUALLY NOT - just defining study area
    #real problem is in aggregation lower down
    #you could define which layer you want to pull from brps or ztaz or whatever
    #----> would need another input but this is a persitnent problem
    {
      message(str_glue("{make_space()}\nCreating study area subset now...."))

      table_sa_poly_index = bq_project_query(
        "replica-customer"
        ,qs_table_sa_poly_index())

      info(logger, str_glue("table_sa_poly_index: {replica_temp_tbl_name(table_sa_poly_index)}"))
    }

    ###CREATE: trip_prefilter for mode----
    #prefilters trips so that we only get commercial vehicles
    #just makes queries run a little bit faster
    #TODO: this actually needs an input for mode
    {
      message(str_glue("{make_space()}\nCreating trip prefilter subset table now...."))

      table_trip_subset = bq_project_query(
        customer_name
        ,qs_table_trip_subset())

      info(logger, str_glue("table_trip_subset: {replica_temp_tbl_name(table_trip_subset)}"))
    }

    ###CREATE: trip subset occurring on select links----
    #returns only trips that have a network link in subset of links
    #performs big cross=reference/filtering operation
    {
      message(str_glue("{make_space()}\nFiltering trips that only use queried network now...."))

      table_trip_network_match = bq_project_query(
        customer_name
        ,qs_table_trip_network_match())

      info(logger,str_glue("table_trip_network_match: {replica_temp_tbl_name(table_trip_network_match)}"))
    }

    ###CREATE: thruzone trips----
    #returns all trips that go through zone
    #all other sections aggregate this in some way
    {
      message(str_glue("{make_space()}\nCreating trips through zone table now...."))

      table_trips_thru_zone = bq_project_query(
        customer_name
        ,qs_table_trips_thru_zone())

      info(logger,str_glue("table_trips_thru_zone: {replica_temp_tbl_name(table_trips_thru_zone)}"))

      message(str_glue("{make_space()}\nInitial queries complete, starting aggregation queries now...."))
    }

    #od_data
    {
      message(str_glue("{make_space()}\nOrigin and Destination aggreations commencing...."))

      table_simple_origin_destination = bq_project_query(
        customer_name
        ,qs_table_simple_origin_destination())

      info(logger,str_glue("table_simple_origin_destination: {replica_temp_tbl_name(table_simple_origin_destination)}"))

      table_ordered_trip_links = bq_project_query(
        customer_name
        ,qs_table_ordered_trip_links())

      info(logger,str_glue("table_ordered_trip_links: {replica_temp_tbl_name(table_ordered_trip_links)}"))

      table_trip_first_link = bq_project_query(
        customer_name
        ,qs_table_trip_first_link())

      table_trip_first_link_pro = bq_project_query(
        customer_name
        ,qs_table_trip_first_link_pro())

      info(logger,str_glue("table_trip_first_link_pro: {replica_temp_tbl_name(table_trip_first_link_pro)}"))

      message(str_glue("Origin and Destination aggreations complete....{make_space()}"))
    }


    #network_links
    {
      message(str_glue("{make_space()}\nLink aggreations commencing...."))

      table_agg_by_link = bq_project_query(
        customer_name
        ,qs_table_agg_by_link())

      info(logger,str_glue("table_agg_by_link: {replica_temp_tbl_name(table_agg_by_link)}"))

      #large network count
      {
        # table_agg_by_link_sum = bq_project_query(
        #   customer_name
        #   ,qs_table_agg_by_link_sum(table_agg_by_link))
        #
        # summary_table_link_counts = bq_table_download(table_agg_by_link_sum) %>%
        #   mutate(flag_link = fct_relevel(flag_link,
        #                                  c("1 count", "2 count", "3 count"
        #                                    ,"4 count", '5 count', "6-10 count", "11 or greater"))) %>%
        #   arrange(flag_link) %>%
        #   mutate(percent = 100*dgt3(count/sum(count))
        #          ,count_cum = cumsum(count)
        #          ,percent_cum = 100*dgt3(count_cum/sum(count))) %>%
        #   arrange(desc(flag_link)) %>%
        #   mutate(count_rm = cumsum(count)
        #          ,percent_rm = cumsum(percent))
        #
        # log_and_info(mes_network_size(), logger)
        # message(mes_network_size_option())

        # check_network_links_TF = F
        # while (check_network_links_TF == F) {
        #   check_network_links  = readline("Would you like to continue query (with the option to change threshold)? (Y/N) ")
        #   check_network_links_TF = (check_network_links %in% c("y", "n", "Y", "N"))
        #   if (!check_network_links_TF){
        #     message("Not a valid input... try again")
        #   }
        # }
        #
        # if (check_network_links %in% c("n", "N")) {
        #   warn(logger, "You have elected to terminate function run..." )
        #   stopifnot("You have elected to terminate function run..." = F)
        # }

      }

      #subset network count
      {
        table_agg_by_link_subset = bq_project_query(
          customer_name
          ,qs_table_agg_by_link_subset())

        table_agg_by_link_sum_subset = bq_project_query(
          customer_name
          ,qs_table_agg_by_link_sum(table_agg_by_link_subset))

        summary_table_link_counts = bq_table_download(table_agg_by_link_sum_subset) %>%
          mutate(flag_link = fct_relevel(flag_link,
                                         c("1 count", "2 count", "3 count"
                                           ,"4 count", '5 count', "6-10 count", "11 or greater"))) %>%
          arrange(flag_link) %>%
          mutate(percent = 100*dgt3(count/sum(count))
                 ,count_cum = cumsum(count)
                 ,percent_cum = 100*dgt3(count_cum/sum(count))) %>%
          arrange(desc(flag_link)) %>%
          mutate(count_rm = cumsum(count)
                 ,percent_rm = cumsum(percent))

        #message here to reduce the size of the network!
        log_and_info(mes_network_size(), logger)
        message(mes_network_size_option())

        #input here needed for whether to continue or not
        #TODO:here
      }

      info(logger,str_glue("table_agg_by_link_subset: {replica_temp_tbl_name(table_agg_by_link_subset)}"))

      message(str_glue("Link aggreations complete....{make_space()}"))
    }

  }

  #data_download
  {
    here(folder, "replica_sa_poly_index.csv") %>%
      write.csv(bq_table_download(table_sa_poly_index, n_max = max_record), .)

    here(folder, "replica_trip_origin_links.csv") %>%
      write.csv(bq_table_download(table_trip_first_link_pro, n_max = max_record), .)

    #this one is good
    here(folder, "replica_trip_agg_by_link_subset.csv") %>%
      write.csv(bq_table_download(table_agg_by_link_subset, n_max = max_record), .)

    here(folder, "replica_trip_origin_destination.csv") %>%
      write.csv(bq_table_download(table_simple_origin_destination, n_max = max_record), .)

    if (query_network){
      here(folder, "replica_queried_network.csv") %>%
        write.csv(bq_table_download(table_network, n_max = max_record), .)
    } else {
      log_and_warn("User did not elect to download the network at function runtime.")
    }

  }
}
