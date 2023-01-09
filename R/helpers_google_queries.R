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
#' Function currently only works for data using blockgroups as it is hardcoded in some of the queries. Changes can be made to the query such that other polygon shapes can be used.
#' This function creates a log file that records inputs, the query sent to google, and IDs for each google table that is created in the multi-step query. All of this information can be accessed at later points in time. Tablle IDs in log file can be used for auditing pruposes or copy and pasted and used in the Google Big Query console GUI.
#'
#' @param bounding_box_points bounding box object detailing extent of road network that you whish to query trips with. This should be made using sf::st_bbox().
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
  bounding_box_points
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

    #init_error_logging_and_setup
  {
    query_start = clean_datetime()
    folder = here(file_destination
                  ,str_glue("data_{query_start}"))
    stopifnot("Folder location exists already, stop and prevent overwriting files" = (dir.exists(folder) != T))
    dir.create(folder)
    log_file = here(file_destination
                    ,str_glue("data_{query_start}/log_file.txt"))
    logger = logger("DEBUG", appenders = file_appender(log_file))

    info(logger, "Query started")
    message(str_glue('Query started at {query_start}\nFile path to log file {log_file}'))

    info(logger
         ,str_glue("{make_space()}\nLogging Query Inputs
         Bounding Box\n{paste0(str_glue('{names(bounding_box_points)} _ {bounding_box_points}'), collapse = '\n')}
         Links Provided\n{paste0(str_glue('{sort(query_links)}'),collapse = '\n')}
         Cutsomer Name: {customer_name}
         Schema Table: {trip_table}"))

    bounding_box_message = str_glue("(startLat > {bounding_box_points[[2]]} AND startLat < {bounding_box_points[[4]]}) AND
                                (endLat > {bounding_box_points[[2]]} AND endLat < {bounding_box_points[[4]]}) AND
                                (startLon > {bounding_box_points[[1]]} AND startLon < {bounding_box_points[[3]]}) AND
                                (endLon > {bounding_box_points[[1]]} AND endLon < {bounding_box_points[[3]]})")

    if (!any(is.na(query_links))){
      message("No NAs detected in links input.... Good")
      links_pro = paste0("'", query_links, "'", collapse = ", ")
      links_where_statement = str_glue("where highway in ({links_pro})")
    } else {
      message("NAs detected, all highway links will be queried.")
      warn(logger, "NAs detected, all highway links will be queried.")
      links_where_statement = ""
    }

    message("Passed links location")
  }

  #query strings
  #pre-make query strings for later
  {
    query_string_count = str_glue(
      "SELECT highway, count(*) as count
from
`{network_table}`
{links_where_statement}
{ifelse(links_where_statement == '', 'where', 'and')}
{bounding_box_message}
group by highway
order by count;")

    query_string_google_cloud = str_glue(
      "SELECT *
from
`{network_table}`
{links_where_statement}
{ifelse(links_where_statement == '', 'where', 'and')}
{bounding_box_message}
;")

    info(logger
         ,str_glue("{make_space()}\nQuery used:\n{query_string_google_cloud}"))
  }

  # network_count = dbGetQuery(con, query_string_count)
  {
    init_query_and_count = bq_project_query(
      customer_name
      ,query_string_count
    )

    project_dataset = str_glue("{init_query_and_count$project}.{init_query_and_count$dataset}")

    info(logger, str_glue("Replica project and dataset are: {project_dataset}"))

    highway_counts = bq_table_download(init_query_and_count)

    if ((nrow(highway_counts) == 0)){
      mes_fatal = "Number of returned links was zero\n... change bounding box\n...stopping...."
      fatal(logger, mes_fatal)
      stopifnot(mes_fatal = (nrow(highway_counts) != 0))
    } else {
      str_glue("{make_space()}\nNon-empty data returned, Good\nQuery continued...\nIn total {sum(highway_counts$count)} links\n{str_glue('-{highway_counts$count} ({100*dgt2(highway_counts$count/sum({highway_counts$count}))})% {highway_counts$highway}') %>%
                paste0(collapse = '\n')}") %>%
        log_and_info(., logger)

      log_and_warn(
        str_glue("{make_space()}\nThe following link types were requested but not found in bounding box
                 {str_glue('{query_links[-which(highway_counts$highway %in% query_links)]}') %>% paste0(collapse = '\n')}")
        ,logger)
    }
  }




  #query google
  {
    log_and_info("Starting Google query now", logger)
    #query network
    #gets links only for certain area defined by user
    #defines list of roads that we want to visualize
    #--could have a problem where a trip in study area is not on roads
    #--but if list is sufficiently large then I think its okay
    {
      message(str_glue("{make_space()}\nCreating network table now...."))

      table_network = bq_project_query(
        customer_name
        ,query_string_google_cloud
      )

      info(logger,str_glue("table_network: {replica_temp_tbl_name(table_network)}"))
    }

    #prefilters trips so that we only get commercial vehicles
    #just makes queries run a little bit faster
    {
      message(str_glue("{make_space()}\nCreating trip subset table now...."))

      temp_query = str_glue("select distinct activity_id, network_link_ids
from
(select *
from `{trip_table}`
where mode = 'COMMERCIAL'
), unnest(network_link_ids) as network_link_ids
;")

      table_trip_subset = bq_project_query(
        customer_name
        ,temp_query)

      info(logger, str_glue("table_trip_subset: {replica_temp_tbl_name(table_trip_subset)}"))
    }

    #returns only trips that have a network link in subset of links
    #perfroms big cross=reference/filtering operation
    {
      message(str_glue("{make_space()}\nFiltering trips that only use queried network now...."))

      temp_query = str_glue("select distinct activity_id
from {replica_temp_tbl_name(table_trip_subset)}
where
1 = 1
and network_link_ids in (select stableEdgeId from {replica_temp_tbl_name(table_network)});")

      table_trip_network_match = bq_project_query(
        customer_name
        ,temp_query)

      info(logger,str_glue("table_trip_network_match: {replica_temp_tbl_name(table_trip_network_match)}"))
    }

    #returns all trips that go through zone
    #all other sections aggregate this in some way
    {
      message(str_glue("{make_space()}\nCreating trips through zone table now...."))

      temp_query = str_glue("select *
from `{trip_table}`
where 1 = 1
and activity_id in (select activity_id from {replica_temp_tbl_name(table_trip_network_match)});")

      table_trips_thru_zone = bq_project_query(
        customer_name
        ,temp_query)

      info(logger,str_glue("table_trips_thru_zone: {replica_temp_tbl_name(table_trips_thru_zone)}"))

      message(str_glue("{make_space()}\nInitial queries complete, starting aggregation queries now...."))
    }

    #od_data
    {
      temp_query = str_glue("select
    activity_id
    ,mode
    ,vehicle_type
    ,origin_bgrp
    ,destination_bgrp
    ,network_link_ids_unnested
    ,ROW_NUMBER ()
    OVER (PARTITION BY activity_id) AS index
    from {replica_temp_tbl_name(table_trips_thru_zone)}
                          ,unnest(network_link_ids) as network_link_ids_unnested;")

      table_ordered_trip_links = bq_project_query(
        customer_name
        ,temp_query)

      info(logger,str_glue("table_ordered_trip_links: {replica_temp_tbl_name(table_ordered_trip_links)}"))

      temp_query = str_glue("select mode, vehicle_type, origin_bgrp, network_link_ids_unnested, count(*) as count
from {replica_temp_tbl_name(table_ordered_trip_links)}
where 1 = 1
and index = 1
group by mode, vehicle_type, origin_bgrp, network_link_ids_unnested;")

      table_trip_first_link = bq_project_query(
        customer_name
        ,temp_query)

      temp_query = str_glue("select
table_left.*
,table_right.*
from {replica_temp_tbl_name(table_trip_first_link)} table_left
left join (select stableEdgeId,startLat,startLon,endLat,endLon
from {network_table}
where stableEdgeId in (select network_link_ids_unnested from {replica_temp_tbl_name(table_trip_first_link)})) table_right
on (table_left.network_link_ids_unnested = table_right.stableEdgeId);")

      table_trip_first_link_pro = bq_project_query(
        customer_name
        ,temp_query)

      info(logger,str_glue("table_trip_first_link_pro: {replica_temp_tbl_name(table_trip_first_link_pro)}"))

      temp_query = str_glue("select mode
      ,vehicle_type
      ,origin_bgrp
      ,destination_bgrp
      ,count(*) as count
      from (select distinct activity_id, mode, vehicle_type, origin_bgrp, destination_bgrp
            from {replica_temp_tbl_name(table_ordered_trip_links)})
      group by mode, vehicle_type, origin_bgrp, destination_bgrp;")

      table_simple_origin_destination = bq_project_query(
        customer_name
        ,temp_query)

      message(str_glue("{make_space()}\nrigin and Destination based aggreations complete...."))
    }

    #network_links
    {
      temp_query = str_glue("select vehicle_type, network_link_ids_unnested, count(*) as count
from
(select vehicle_type, network_link_ids_unnested
from {replica_temp_tbl_name(table_trips_thru_zone)}, unnest(network_link_ids) as network_link_ids_unnested
)
group by vehicle_type, network_link_ids_unnested;")

table_agg_by_link = bq_project_query(
  customer_name
  ,temp_query)

info(logger,str_glue("table_agg_by_link: {replica_temp_tbl_name(table_agg_by_link)}"))

temp_query = str_glue("select *
from {replica_temp_tbl_name(table_agg_by_link)}
where network_link_ids_unnested in (select distinct stableEdgeId from {replica_temp_tbl_name(table_network)})")

table_agg_by_link_subset = bq_project_query(
  customer_name
  ,temp_query)

info(logger,str_glue("table_agg_by_link_subset: {replica_temp_tbl_name(table_agg_by_link_subset)}"))

message(str_glue("{make_space()}\nNetwork link based aggreations complete...."))
    }

  }

  #data_download
  {
    here(folder, "replica_trip_origin_links.csv") %>%
      write.csv(bq_table_download(table_trip_first_link_pro, n_max = max_record), .)

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



