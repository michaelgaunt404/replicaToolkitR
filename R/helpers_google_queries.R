#' Extract project name, dataset, and tablename from google big query object.
#'
#' When BQuery objects (tables) are made using bq_project_query() they are given temporary dataset and table names.
#' This function extracts all of a table's identifying elements and glues them into a singular string.
#' This string can be used in subsequent queires.
#'
#' Function is helpful when making mulitstepped queries where temporary tables are created and subsequently interacted with.
#'
#' @param object
#'
#' @return a character string
#' @export
#'
#' @examples
#'
#' temp = list(username = "replica_fake_name"
#',dataset = "replica_temp_dataset"
#',table = "replica_temp_table")
#'
#'replica_temp_tbl_name(temp)
replica_temp_tbl_name = function(object) {
  str_glue("{object[[1]]}.{object[[2]]}.{object[[3]]}")
}


#' @title Easily query and download Replica trip and network data using the extent of a map layer.
#'
#' @description This function takes a bounding box and google big query inputs and returns data back to you.
#' Function currently only works for data using block groups as it is hardcoded in some of the queries. Changes can be made to the query such that other polygon shapes can be used.
#'
#' This function creates a log file that records inputs, the query sent to Google, and IDs for each Google table that is created in the multi-step query. All of this information can be accessed at later points in time. Table IDs in log file can be used for auditing purposes or copy and pasted and used in the Google Big Query console GUI.
#'
#'
#' @param bb_network_layer bounding box object detailing extent of road network that will be used to query trips with. This should be made using sf::st_bbox().
#' @param bb_sa_layer bounding box object detailing extent of the study area that will be used to perform aggregations. This should be made using sf::st_bbox(). This input is gernerally larger or identical to [bb_network_layer].
#' @param query_links vector of character strings detailing which type of network links to use in the query. These will be used 1) to depict any network link volume graphics with 2) to filter trips with. For the latter, any trip that uses these road within the study area will be including in any reporting.
#' @param customer_name character string indicating google account that will be billed
#' @param trip_table character string indicating which trip table should be queired - location, year, quarter, day, etc should match network_table. Please include entire table name as well '[customer_name].[trip_table]'
#' @param network_table character string indicating which trip table should be queired - location, year, quarter, day, etc should match trip_table. Please include entire table name as well '[customer_name].[network_table]'
#' @param file_destination character string indicating a directory where you want log files and data to be saved to. A new folder containg function outputs will be made there named using the convention 'data_[sys.datetime]'
#' @param query_network boolean (T/F) indicating if the network should be downloaded at function runtime. Default is 'T'. It can be beneficial to wait and see how many links will be downloaded before netowrk is downloaded, it can also be downloaded latter as log files record big query table IDs.
#' @param max_record integer indicating max number of records will be written out for each data acquired by google - default is 1000, use Inf if you do not want to limit download. Useful if you want to check the data out first before downloading large amounts of data. Google tables are still made in full so they can be manually acquired without rerunning this function - see log files.
#'
#' @return does not return any objects in R. Function creates a log file and data to folder destination in CSV format.
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
    # query_links = c("highway", "corridor", "road", "motorway", "motorway_link", "trunk",
    #                 "trunk_link", "primary", "primary_link", "secondary", "secondary_link")
  }

  #init_error_logging_and_setup
  {
    query_start = gauntlet::clean_datetime()
    folder = here::here(file_destination, stringr::str_glue("data_{query_start}"))
    stopifnot("Folder location exists already, stop and prevent overwriting files" = (dir.exists(folder) != T))
    dir.create(folder)
    log_file = here::here(file_destination, stringr::str_glue("data_{query_start}/log_file.txt"))
    logger = log4r::logger("DEBUG", appenders = log4r::file_appender(log_file))
    log4r::info(logger, "Query started")
    message(stringr::str_glue('Query started at {query_start}\nFile path to log file:\n{log_file}'))

    log4r::info(logger,
                stringr::str_glue("{make_space()}\nLogging Query Inputs\nPath to network boundary file: {bb_network_layer}\nPath to study area boundary file: {bb_sa_layer}\nCutsomer Name: {customer_name}\nSchema Table: {trip_table}\nLinks Provided:{make_space('-', n = 10)}\n{paste0(stringr::str_glue('{sort(query_links)}'),collapse = '\n')}{make_space('-', n = 10)}"))

    if (!any(is.na(query_links))){
      message("No NAs detected in links input.... Good")
      links_pro = paste0("'", query_links, "'", collapse = ", ")
      links_where_statement = stringr::str_glue("where highway in ({links_pro})")
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
          log4r::fatal(logger, "More than one polygon in a supplied boundary object detected")
          stopifnot("One of the bounding layers you provided has more than one polygon, please fix this...." = F)
        }

        if (st_crs(temp_object)$input == "EPSG:4326"){
          gauntlet::log_and_warn('CRS for one of the bounding layers you provided is was not set to EPSG:4326, it was converted for you....', logger)
          temp_object = sf::st_transform(temp_object, 4326)
        }
        gauntlet::log_and_info("Bounding box checks pass.... GOOD", logger)
        temp_wkt = wellknown::sf_convert(temp_object)
      })
  }

  ##query google----
  {
    gauntlet::log_and_info("Starting Google query now", logger)

    ###CREATE: network_table and get count----
    #gets links only for certain area defined by user
    #defines list of roads that we want to visualize
    #--could have a problem where a trip in study area is not on roads
    #--but if list is sufficiently large then I think its okay
    {
      message(stringr::str_glue("{make_space()}\nCreating network table now...."))

      table_network = bigrquery::bq_project_query(
        customer_name
        # ,qs_table_network()
        ,stringr::str_glue("select * from (
select *,
ST_INTERSECTS(
ST_GEOGFROMTEXT('{list_wkt_objects[[1]]}')
,geometry) as flag_contains
from `{network_table}`
where highway in ({links_pro})
)
where flag_contains = TRUE")
      )

      temp_query = stringr::str_glue("SELECT highway, count(*) as count
                      from {replica_temp_tbl_name(table_network)}
                      group by highway
                      order by count;")

      table_network_link_count = bigrquery::bq_project_query(
        customer_name
        ,temp_query
      )

      project_dataset = stringr::str_glue("{table_network_link_count$project}.{table_network_link_count$dataset}")

      log4r::info(logger, stringr::str_glue("Replica project and dataset are: {project_dataset}"))

      highway_counts = bigrquery::bq_table_download(table_network_link_count)

      if ((nrow(highway_counts) == 0)){
        mes_fatal = "Number of returned links was zero\n...change bounding box\n...stopping...."
        log4r::fatal(logger, mes_fatal)
        stopifnot("Number of returned links was zero\n...change bounding box\n...stopping...." = F)
      } else {
        str_glue("{make_space()}\nNon-empty data returned, Good\nQuery continued...\nIn total {sum(highway_counts$count)} links\n{str_glue('-{highway_counts$count} ({100*gauntlet::dgt2(highway_counts$count/sum({highway_counts$count}))})% {highway_counts$highway}') %>%
                paste0(collapse = '\n')}") %>%
          gauntlet::log_and_info(., logger)

        index_empty_highways = query_links[-which(highway_counts$highway %in% query_links)]

        if (length(index_empty_highways) == 0){
          gauntlet::log_and_info(stringr::str_glue("All requested link types returned with some number of links\nnone empty.... GOOD"),logger)
        } else {
          gauntlet::log_and_warn(
            str_glue("{make_space()}\nThe following link types were requested but not found in bounding box
                 {str_glue('{}') %>% paste0(collapse = '\n')}"),logger)
        }
      }

      log4r::info(logger,stringr::str_glue("table_network: {replica_temp_tbl_name(table_network)}"))

      check_network_links_TF = F
      while (check_network_links_TF == F) {
        check_network_links  = readline("Would you like to continue the function exectuion? (Y/N) ")
        check_network_links_TF = (check_network_links %in% c("y", "n", "Y", "N"))
        if (!check_network_links_TF){
          message("Not a valid input... try again")
        }
      }

      if (check_network_links %in% c("n", "N")) {
        log4r::warn(logger, "You have elected to terminate function run..." )
        stopifnot("You have elected to terminate function run..." = F)
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
      message(stringr::str_glue("{make_space()}\nCreating study area subset now...."))

      table_sa_poly_index = bigrquery::bq_project_query(
        "replica-customer"
        # ,qs_table_sa_poly_index()
        ,stringr::str_glue("select * from (
select *,
ST_INTERSECTS(
ST_GEOGFROMTEXT('{list_wkt_objects[[2]]}')
,surface_point) as flag_contains
from `Geos.bgrp`
)
where flag_contains = TRUE")
      )

log4r::info(logger, stringr::str_glue("table_sa_poly_index: {replica_temp_tbl_name(table_sa_poly_index)}"))
    }

    ###CREATE: trip_prefilter for mode----
    #prefilters trips so that we only get commercial vehicles
    #just makes queries run a little bit faster
    #TODO: this actually needs an input for mode
    {
      message(stringr::str_glue("{make_space()}\nCreating trip prefilter subset table now...."))

      table_trip_subset = bigrquery::bq_project_query(
        customer_name
        # ,qs_table_trip_subset()
        ,stringr::str_glue("select distinct activity_id, network_link_ids
from
(select *
from `{trip_table}`
where mode = 'COMMERCIAL'
), unnest(network_link_ids) as network_link_ids
;")
      )

      log4r::info(logger, stringr::str_glue("table_trip_subset: {replica_temp_tbl_name(table_trip_subset)}"))
    }

    ###CREATE: trip subset occurring on select links----
    #returns only trips that have a network link in subset of links
    #performs big cross=reference/filtering operation
    {
      message(stringr::str_glue("{make_space()}\nFiltering trips that only use queried network now...."))

      table_trip_network_match = bigrquery::bq_project_query(
        customer_name
        # ,qs_table_trip_network_match()
        ,stringr::str_glue("select distinct activity_id
from {replica_temp_tbl_name(table_trip_subset)}
where
1 = 1
and network_link_ids in (select stableEdgeId from {replica_temp_tbl_name(table_network)});")
      )

      log4r::info(logger,stringr::str_glue("table_trip_network_match: {replica_temp_tbl_name(table_trip_network_match)}"))
    }

    ###CREATE: thruzone trips----
    #returns all trips that go through zone
    #all other sections aggregate this in some way
    {
      message(stringr::str_glue("{make_space()}\nCreating trips through zone table now...."))

      table_trips_thru_zone = bigrquery::bq_project_query(
        customer_name
        # ,qs_table_trips_thru_zone()
        ,stringr::str_glue("select *
,case
when origin_bgrp in (select raw_id from {replica_temp_tbl_name(table_sa_poly_index)}) then origin_bgrp
else 'out of study area'
END as origin_poly
,case
when destination_bgrp in (select raw_id from {replica_temp_tbl_name(table_sa_poly_index)}) then destination_bgrp
else 'out of study area'
END as destination_poly
,case
when origin_bgrp in (select raw_id from {replica_temp_tbl_name(table_sa_poly_index)}) then 'internal'
else 'external'
END as flag_sa_origin
,case
when destination_bgrp in (select raw_id from {replica_temp_tbl_name(table_sa_poly_index)}) then 'internal'
else 'external'
END as flag_sa_destination
from (select *
from `{trip_table}`
where 1 = 1
and activity_id in (select activity_id from {replica_temp_tbl_name(table_trip_network_match)}))")
        )

      log4r::info(logger,stringr::str_glue("table_trips_thru_zone: {replica_temp_tbl_name(table_trips_thru_zone)}"))

      message(stringr::str_glue("{make_space()}\nInitial queries complete, starting aggregation queries now...."))
    }

    #od_data
    {
      message(stringr::str_glue("{make_space()}\nOrigin and Destination aggreations commencing...."))

      table_simple_origin_destination = bigrquery::bq_project_query(
        customer_name
        # ,qs_table_simple_origin_destination()
        ,stringr::str_glue("select mode
      ,vehicle_type
      ,origin_poly, flag_sa_origin
      ,destination_poly, flag_sa_destination
      ,count(*) as count
      from {replica_temp_tbl_name(table_trips_thru_zone)}
      group by mode, vehicle_type
           ,origin_poly, flag_sa_origin
      ,destination_poly, flag_sa_destination;"))

      log4r::info(logger,stringr::str_glue("table_simple_origin_destination: {replica_temp_tbl_name(table_simple_origin_destination)}"))

      table_ordered_trip_links = bigrquery::bq_project_query(
        customer_name
        # ,qs_table_ordered_trip_links()
        ,stringr::str_glue("select
    activity_id, mode, vehicle_type
    ,origin_bgrp, origin_poly, flag_sa_origin
    ,destination_bgrp, destination_poly, flag_sa_destination
    ,network_link_ids_unnested
    ,ROW_NUMBER ()
    OVER (PARTITION BY activity_id) AS index
    from {replica_temp_tbl_name(table_trips_thru_zone)}
                          ,unnest(network_link_ids) as network_link_ids_unnested;"))

      log4r::info(logger,stringr::str_glue("table_ordered_trip_links: {replica_temp_tbl_name(table_ordered_trip_links)}"))

      table_trip_first_link = bigrquery::bq_project_query(
        customer_name
        # ,qs_table_trip_first_link()
        ,stringr::str_glue("select
  mode, vehicle_type
  ,origin_bgrp, origin_poly, flag_sa_origin, network_link_ids_unnested
  , count(*) as count
  from {replica_temp_tbl_name(table_ordered_trip_links)}
  where 1 = 1
  and index = 1
  group by mode, vehicle_type,origin_bgrp, origin_poly, flag_sa_origin, network_link_ids_unnested;"))

      table_trip_first_link_pro = bigrquery::bq_project_query(
        customer_name
        # ,qs_table_trip_first_link_pro()
        ,stringr::str_glue("select
table_left.*
,table_right.*
from {replica_temp_tbl_name(table_trip_first_link)} table_left
left join (select stableEdgeId,startLat,startLon,endLat,endLon
from {network_table}
where stableEdgeId in (select network_link_ids_unnested from {replica_temp_tbl_name(table_trip_first_link)})) table_right
on (table_left.network_link_ids_unnested = table_right.stableEdgeId);"))

      log4r::info(logger,stringr::str_glue("table_trip_first_link_pro: {replica_temp_tbl_name(table_trip_first_link_pro)}"))

      message(stringr::str_glue("Origin and Destination aggreations complete....{make_space()}"))
    }

    #network_links
    {
      message(stringr::str_glue("{make_space()}\nLink aggreations commencing...."))

      table_agg_by_link = bigrquery::bq_project_query(
        customer_name
        # ,qs_table_agg_by_link()
        ,  str_glue("select
mode, vehicle_type
,origin_poly, flag_sa_origin
,flag_sa_destination
,network_link_ids_unnested
,count(*) as count
from {replica_temp_tbl_name(table_ordered_trip_links)}
group by
mode, vehicle_type
,origin_poly, flag_sa_origin
,flag_sa_destination
,network_link_ids_unnested"))

      log4r::info(logger,stringr::str_glue("table_agg_by_link: {replica_temp_tbl_name(table_agg_by_link)}"))

      #large network count
      {
        # table_agg_by_link_sum = bigrquery::bq_project_query(
        #   customer_name
        #   ,qs_table_agg_by_link_sum(table_agg_by_link))
        #
        # summary_table_link_counts = bigrquery::bq_table_download(table_agg_by_link_sum) %>%
        #   mutate(flag_link = fct_relevel(flag_link,
        #                                  c("1 count", "2 count", "3 count"
        #                                    ,"4 count", '5 count', "6-10 count", "11 or greater"))) %>%
        #   arrange(flag_link) %>%
        #   mutate(percent = 100*gauntlet::dgt3(count/sum(count))
        #          ,count_cum = cumsum(count)
        #          ,percent_cum = 100*gauntlet::dgt3(count_cum/sum(count))) %>%
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
        table_agg_by_link_subset = bigrquery::bq_project_query(
          customer_name
          # ,qs_table_agg_by_link_subset()
          , stringr::str_glue("select *
from {replica_temp_tbl_name(table_agg_by_link)}
where network_link_ids_unnested in
         (select distinct stableEdgeId from {replica_temp_tbl_name(table_network)})"))

        table_agg_by_link_sum_subset = bigrquery::bq_project_query(
          customer_name
          ,qs_table_agg_by_link_sum(table_agg_by_link_subset))

        summary_table_link_counts = bigrquery::bq_table_download(table_agg_by_link_sum_subset) %>%
          mutate(flag_link = forcats::fct_relevel(flag_link,
                                         c("1 count", "2 count", "3 count"
                                           ,"4 count", '5 count', "6-10 count", "11 or greater"))) %>%
          arrange(flag_link) %>%
          mutate(percent = 100*gauntlet::dgt3(count/sum(count))
                 ,count_cum = cumsum(count)
                 ,percent_cum = 100*gauntlet::dgt3(count_cum/sum(count))) %>%
          arrange(desc(flag_link)) %>%
          mutate(count_rm = cumsum(count)
                 ,percent_rm = cumsum(percent))

        #message here to reduce the size of the network!
        gauntlet::log_and_info(
          str_glue("{make_space()}\nUser supplied inputs resulted in {gauntlet::pretty_num(sum(summary_table_link_counts$count))} records in link aggregation table....\nSee the following table:{make_space('-', 30)}\n{paste0(capture.output(summary_table_link_counts), collapse = '\n')}{make_space('-', 30)}\nBy default, links with less than 5 counts on them are removed\n---this would result in downloading {summary_table_link_counts[[3, 6]]} records....\n---An ideal number of records is ~500,000")
          , logger)
        message(stringr::str_glue("If your selection has resulted in too many records, you can............
         1) Decrease the study area layer resulting in less originating polys
         2) Decrease the size of the network layer supplied to the function
         3) Reduce the number of link types queired by the function by changing query_links input"))

        #input here needed for whether to continue or not
        #TODO:here
        #or if they should have the option to change the threshold

        table_agg_by_link_subset_limited = bigrquery::bq_project_query(
          customer_name
          ,stringr::str_glue("select *
                    from {replica_temp_tbl_name(table_agg_by_link_subset)}
                    where count >= 5"))
        }

      log4r::info(logger,stringr::str_glue("table_agg_by_link_subset: {replica_temp_tbl_name(table_agg_by_link_subset)}"))
      log4r::info(logger,stringr::str_glue("table_agg_by_link_subset_limited: {replica_temp_tbl_name(table_agg_by_link_subset_limited)}"))

      message(stringr::str_glue("Link aggreations complete....{make_space()}"))

      check_network_dl_TF = F
      while (check_network_dl_TF == F) {
        check_network_dl  = readline(stringr::str_glue("Would you like to download the network links ({summary_table_link_counts[[3, 6]]} records) at this time? (Y/N) "))
        check_network_dl_TF = (check_network_links %in% c("y", "n", "Y", "N"))
        if (!check_network_dl_TF){
          message("Not a valid input... try again")
        }
      }

    }

  }

  #data_download
  {
    message("Starting data download now.....")

    here(folder, "replica_sa_poly_index.csv") %>%
      write.csv(
        bigrquery::bq_table_download(table_sa_poly_index, n_max = max_record)
        , file = ., row.names = F)

    here(folder, "replica_trip_origin_links.csv") %>%
      write.csv(
        bigrquery::bq_table_download(table_trip_first_link_pro, n_max = max_record)
        , file = ., row.names = F)

    #this one is good
    here(folder, "table_agg_by_link_subset_limited.csv") %>%
      write.csv(
        bigrquery::bq_table_download(table_agg_by_link_subset_limited, n_max = max_record)
        , file = ., row.names = F)

    here(folder, "replica_trip_origin_destination.csv") %>%
      write.csv(
        bigrquery::bq_table_download(table_simple_origin_destination, n_max = max_record)
        , file = ., row.names = F)

    if (check_network_dl %in% c("y", "Y")){
      here(folder, "replica_queried_network.csv") %>%
        write.csv(
          bigrquery::bq_table_download(table_network, n_max = max_record)
          , file = ., row.names = F)
    } else {
      log_and_warn("User did not elect to download the network at function runtime.")
    }

  }

}




