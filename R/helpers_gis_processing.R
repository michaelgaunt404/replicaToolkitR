#' Convert tabular network file to gis layer.
#'
#' @description The network acquired from google is saved as a shapefile.
#' This function helps converts file from CSV to GIS layer and provides an option to save GIS object as a ".gpkg".
#'
#' @param location character string pointing to top level location where data acquired from Google was saved to.
#' @param folder character string of name where data was automatically saved to from google data download.
#' @param auto_save Boolean (T/F - default F) indicating if you want the GIS layer to be saved. Default just creates an object without saving.
#' @param network_object tabular link data. Default is NULL or input left empty - function will use location and folder inputs to load object and then convert.
#'
#' @return GIS layer of network with CRS 4326
#' @export
#'
#' @examples
#' data('replica_queried_network')
#'
#' head(replica_queried_network)
#'
#' make_network_link_layer(
#'   network_object = replica_queried_network
#'   ,auto_save = F
#')
make_network_link_layer = function(location, folder, auto_save = F, network_object = NULL){

  if (is.null(network_object)){
    message("Spatial links were made using CSV from file and location...")
    replica_queried_network = here::here(location, folder, "replica_queried_network.csv") %>%
      data.table::fread()
    replica_queried_network = sf::st_as_sf(replica_queried_network, wkt = "geometry", crs = 4326)
  } else {
    message("Spatial links were made using supplied tabular network object...")
    replica_queried_network = sf::st_as_sf(network_object, wkt = "geometry", crs = 4326)
  }

  stopifnot("Network object had more than one geometry feature in it, please review and fix...." = (length(unique(st_geometry_type(replica_queried_network))) == 1))
  stopifnot("ERROR: Nerwork object must be LINESTRING, please review and fix...." = (unique(st_geometry_type(replica_queried_network))[[1]] %in% c("LINESTRING")))

  replica_queried_network = replica_queried_network %>%
    mutate(flag_highway = case_when(
      (str_detect(streetName, "^US") |
         str_detect(streetName, "^I [[:digit:]]+") |
         str_detect(streetName, "^SR [[:digit:]]+") |
         str_detect(streetName, "State Route") |
         str_detect(streetName, str_glue("^({paste0(pull(tigris::states(), STUSPS), collapse = '|')}) [[:digit:]]+"))
      )~"Flagged Highway"
      ,T~"Not a Highway")) %>%
    mutate(stableEdgeId = str_trunc(stableEdgeId , 14, "right", "")) %>%
    select(stableEdgeId, streetName, distance, osmid, highway, flag_contains, flag_highway)


  if (auto_save) {
    sf::write_sf(replica_queried_network, here::here(location, folder, "replica_queried_network_links.gpkg"))
  }

  return(replica_queried_network)
}

#' Convert GIS network links to points by using link centroids.
#'
#' @description HTML packages used in creating interactive, client-side visualizations are not compatible with links.
#' This function converts GIS links to points by extracting the centroids.
#'
#' @param location character string pointing to top level location where data acquired from google was saved to.
#' @param folder character string of name where data was automatically saved to from Google data download.
#' @param auto_save boolean (T/F - default F) indicating if you want the GIS layer to be saved. Default just creates an object without saving.
#' @param network_object network object containing links. Default is NULL or input left empty - function will use location and folder inputs to load object and then convert.
#'
#' @return GIS layer of network with CRS 4326
#' @export
#'
#' @examples
#' data('replica_queried_network_links')
#'
#' head(replica_queried_network_links)
#'
#' make_network_centroid_layer(
#'   location = "data/req_dev"
#'   ,folder = "data_20230106_154119"
#'   ,network_object = replica_queried_network_links
#'   ,auto_save = F
#' )
#'
#'
make_network_centroid_layer = function(location, folder, auto_save = F, network_object = NULL){

  # network_object = replica_queried_network_links

  if (is.null(network_object)){
    message("Centroids were made using spatial links from file and location...")
    replica_queried_network = here::here(location, folder, "replica_queried_network_links.gpkg") %>%
      sf::read_sf()
    replica_queried_network_cntds = gauntlet::st_true_midpoint(replica_queried_network) %>%
      select(!c(merge_id))
  } else {
    message("Centroids were made using supplied spatial links network object...")
    replica_queried_network_cntds = gauntlet::st_true_midpoint(network_object) %>%
      select(!c(merge_id))
  }

  stopifnot("Supplied network object had more than one geometry feature in it, please review and fix...." = (length(unique(st_geometry_type(replica_queried_network_cntds))) == 1))
  stopifnot("Supplied network object must be either POINT or LINESTRING, please review and fix...." = (unique(st_geometry_type(replica_queried_network_cntds))[[1]] %in% c("POINT")))

  if (auto_save) {
    sf::write_sf(replica_queried_network_cntds, here::here(location, folder, "replica_queried_network_cntds.gpkg"))
  }

  return(replica_queried_network_cntds)
}


#' Convert tabular first link file to gis layer.
#'
#' @description The network acquired from google is saved as a shapefile.
#' This function helps converts file from CSV to GIS layer and provides an option to save GIS object as a ".gpkg".
#'
#' @param location character string pointing to top level location where data acquired from google was saved to.
#' @param folder character string of name where data was automatically saved to from Google data download.
#' @param auto_save boolean (T/F - default F) indicating if you want the GIS layer to be saved. Default just creates an object without saving.
#' @param first_links_object network object containing links. Default is NULL or input left empty - function will use location and folder inputs to load object and then convert.
#'
#' @return GIS layer of network with CRS 4326
#' @export
#'
#' @examples
#' data('replica_queried_network_links')
#'
#' head(replica_queried_network_links)
#'
#' make_network_centroid_layer(
#'   location = "data/req_dev"
#'   ,folder = "data_20230106_154119"
#'   ,network_object = replica_queried_network_links
#'   ,auto_save = F
#' )
make_trip_origin_point_layer = function(location, folder, auto_save = F
                                        ,first_links_object = NULL){

  # location = "data/req_dev"
  # folder = 'data_20230117_092037'

  if (is.null(first_links_object)){
    message("Trip origin link points made using file and location...")
    replica_trip_origin_links = here::here(location, folder, "replica_trip_origin_links.csv") %>%
      data.table::fread()
    replica_trip_origin_links = sf::st_as_sf(replica_trip_origin_links, coords = c('startLon', 'startLat'), crs = 4326)
  } else {
    message("Trip origin link points made using suplied network object...")
    replica_trip_origin_links = sf::st_as_sf(first_links_object, coords = c('startLon', 'startLat'), crs = 4326)
  }

  stopifnot("Supplied network object had more than one geometry feature in it, please review and fix...." = (length(unique(st_geometry_type(replica_trip_origin_links))) == 1))
  stopifnot("Supplied network object must be either POINT or LINESTRING, please review and fix...." = (unique(st_geometry_type(replica_trip_origin_links))[[1]] %in% c("POINT")))

  replica_trip_origin_links = replica_trip_origin_links %>%
    select(!c(origin_bgrp, network_link_ids_unnested))

  if (auto_save) {
    sf::write_sf(replica_trip_origin_links, here::here(location, folder, "replica_trip_origin_links.gpkg"))
  }

  return(replica_trip_origin_links)
}

#' Get polygons using Replica study area poly index.
#'
#' @param location character string pointing to top level location where data acquired from google was saved to.
#' @param folder character string of name where data was automatically saved to from google data download.
#' @param auto_save boolean (T/F - default F) indicating if you want the GIS layer to be saved. Default just creates an object without saving.
#' @param network_object network object containing links. Default is NULL or input left empty - function will use location and folder inputs to load object and then convert.
#' @param states character string or vector character string of state two letter abbreviations indicating which states to get polys for.
#' @param year integer indicating which year census polys should be acquired. Replica uses 2010 and this functions default is 2010, you should not change this.
#'
#' @return a spatial object containing polygons for study area polygons acquired from Replica.
#' @export
#'
#' @examples
#' #none
get_tigris_polys_from_replica_index = function(
  location, folder, auto_save = F, network_object = NULL
  ,states, year = 2010){

  # location = "data/req_dev"
  # folder = "data_20230111_150315"
  # auto_save = F
  # network_object = NULL
  # states = "WA"
  # year = 2010

  #load_replica_poly_index
  if (is.null(network_object)){
    message("Study area index loaded using file and location...")
    replica_queried_network = here::here(location, folder, "replica_sa_poly_index.csv") %>%
      data.table::fread() %>%
      select(raw_id)
  } else {
    message("Study area index made using suplied network object...")
    replica_queried_network = network_object %>%
      select(raw_id)
  }

  block_groups = tigris::block_groups(state = states, year = year) %>%
    sf::st_transform(4326)

  block_groups_sub = block_groups %>%
    mutate(flag_blkgrps = 1) %>%
    merge(replica_queried_network %>%
            mutate(flag_index = 1
                   ,raw_id = as.character(raw_id))
          , by.x = "GEOID10", by.y = "raw_id", all = T) %>%
    mutate(across(c(flag_blkgrps, flag_index),~tidyr::replace_na(.x, 0))) %>%
    mutate(flag_poly_merge = case_when(
      (flag_blkgrps == 1 & flag_index == 0)~"Tirgris but not Replica"
      ,(flag_blkgrps == 1 & flag_index == 1)~"Both Tirgris and Replica"
      ,(flag_blkgrps == 0 & flag_index == 1)~"Replica but not Tigris"
      ,T~"Error please check.."
    ))

  check_match = block_groups_sub %>%
    sf::st_drop_geometry() %>%
    filter(flag_index == 1) %>%
    count(flag_blkgrps, flag_index) %>%
    mutate(percent = paste0(100*(n/sum(n)), "%")) %>%
    capture.output() %>%
    paste0(collapse = "\n")

  stringr::str_glue("{make_space('-')}\n1 - 1 combinations indicate good matches") %>%  message()
  stringr::str_glue("Anything else indicates bad poly matches") %>%  message()
  stringr::str_glue("The following query resulted in the following matches...\n{check_match}") %>%  message()

  block_groups_sub = block_groups_sub %>%
    filter(flag_poly_merge == "Both Tirgris and Replica") %>%
    select(!c(flag_blkgrps, flag_index, flag_poly_merge))

  stringr::str_glue("Replica query acquired {nrow(replica_queried_network)} polygons\nPolygon query returning {nrow(block_groups_sub)} polygons\n{100*nrow(block_groups_sub)/nrow(replica_queried_network)}% match{make_space('-')}") %>%  message()

  if (auto_save) {
    sf::write_sf(block_groups_sub, here::here(location, folder, "acquired_sa_polys.gpkg"))
  }

  return(block_groups_sub)
}

#' Quickly make network link aggregate objects.
#'
#' @description This function makes an RDS list object containing network link layers that have been aggreated three different pre-set ways.
#' The data contained in `table_agg_by_link_subset_limited` is aggregated by:
#' + network_link and flag_sa_destination
#' + network_link_ids_unnested and vehicle_type
#' + network_link_ids_unnested, origin_poly, and vehicle_type
#'
#' @param location character string pointing to top level location where data acquired from google was saved to.
#' @param folder character string of name where data was automatically saved to from google data download.
#' @param auto_save Boolean (T/F - default F) indicating if you want the GIS layer to be saved. Default just creates an object without saving.
#' @param agg_count_object data frame object containing raw link volumes.
#' @param network_object spatial object of either the network as represented with poly-lines or with network link mid-points.
#' @return a data frame and/or saved RDS file
#' @export
#'
#' @examples
#'
#' data("table_agg_by_link_subset_limited")
#' data("replica_queried_network_cntds")
#'
#' aggregate_network_links(
#'   agg_count_object = table_agg_by_link_subset_limited
#'   ,network_object = replica_queried_network_cntds
#' )
aggregate_network_links = function(location, folder, auto_save = F
                                   ,agg_count_object = NULL
                                   ,network_object = NULL){
  #TODO:make this compatible with other polygon types
  #TODO:review different aggregations - they don't make the most sense

  {
    # location = "data/req_dev"
    # folder = "data_20230125_162034"
    # agg_count_object = NULL
    # auto_save = F
    # network_object = replica_queried_network_cntds
  }

  if (is.null(agg_count_object)){
    message("Aggregations will be made using file and location...")
    network_links = here::here(location, folder, "table_agg_by_link_subset_limited.csv") %>%
      data.table::fread() %>%
      mutate(flag_trip_type = str_glue("{flag_sa_origin}-{flag_sa_destination}")
             ,network_link_ids_unnested = str_trunc(network_link_ids_unnested, 14, "right", ""))
  } else {
    message("Aggregations will be made using supplied network object...")
    network_links = agg_count_object %>%
      mutate(flag_trip_type = str_glue("{flag_sa_origin}-{flag_sa_destination}")
             ,network_link_ids_unnested = str_trunc(network_link_ids_unnested, 14, "right", ""))
  }

  stopifnot("You need to supply an object to merge tabular data with, either network centroids or polylines...." = !is.null(network_object))
  stopifnot("Supplied network object had more than one geometry feature in it, please review and fix...." = (length(unique(st_geometry_type(network_object))) == 1))
  stopifnot("Supplied network object must be either POINT or LINESTRING, please review and fix...." = (unique(st_geometry_type(network_object))[[1]] %in% c("POINT", "LINESTRING")))

  #pre-process data
  {
    if (unique(st_geometry_type(network_object))[[1]] == "POINT"){
      index_extract = "POINT"
    } else {
      index_extract = "LINESTRING"
    }
  }


  #perform aggregations
  {
    message(stringr::str_glue("{make_space()}\nStarting aggreagtion by link and study area destination flag...."))

    #ANL
    {
      message(str_glue("{make_space()}\nStarting aggreagtion by link...."))

      agg_link = network_links %>%
        count_percent_zscore(
          grp_c = c('network_link_ids_unnested')
          ,grp_p = c()
          ,col = count, rnd = 2) %>%
        mutate(count_nrm_prank_ttl = gauntlet::dgt2(percent_rank(count))
               ,count_nrm_mmax_ttl = gauntlet::dgt2(normalize_min_max(count)))

      agg_link_mrg = merge(
        network_object, agg_link
        ,by.x = "stableEdgeId", by.y = "network_link_ids_unnested", all = T)

      agg_link_mrg_pro = agg_link_mrg %>%
        filter(!is.na(count)) %>%
        mutate(label = str_glue(
          "Link Name: {streetName} ({highway})
          <br>TTl Link Volume: {count} - {100*count_nrm_mmax_ttl}% (Min-Max norm.)"))

      message("Aggregation complete....")
    }

    #ANLT
    {
      message(str_glue("{make_space()}\nStarting aggreagtion by link and vehicle type...."))

      agg_link_vehicle_type = network_links %>%
        count_percent_zscore(
          grp_c = c('network_link_ids_unnested', 'vehicle_type')
          ,grp_p = c('network_link_ids_unnested')
          ,col = count, rnd = 2) %>%
        group_by(network_link_ids_unnested) %>%
        mutate(ttl_count_link = sum(count)) %>%
        ungroup() %>%
        mutate(count_nrm_prank_ttl = gauntlet::dgt2(percent_rank(ttl_count_link))
               ,count_nrm_mmax_ttl = gauntlet::dgt2(normalize_min_max(ttl_count_link))) %>%
        group_by(vehicle_type) %>%
        mutate(count_nrm_prank = dgt2(percent_rank(count))
               ,count_nrm_mmax = dgt2(normalize_min_max(count)))

      agg_link_vehicle_type_mrg = merge(
        network_object, agg_link_vehicle_type
        ,by.x = "stableEdgeId", by.y = "network_link_ids_unnested", all = T)

      agg_link_vehicle_type_mrg_pro = agg_link_vehicle_type_mrg %>%
        filter(!is.na(vehicle_type)) %>%
        mutate(label = str_glue(
          "Link Name: {streetName} ({highway})
          <br>TTl Link Volume: {ttl_count_link} - {100*count_nrm_mmax_ttl}% (Min-Max norm.)
          <hr>
          Metrics Adj for Veh. Type: {vehicle_type}
          <br>Link Volume: {count}
          <br>Link Volume (Min-Max norm.): {100*count_nrm_mmax}%"))

      message("Aggregation complete....")
    }

    #ANLTT
    {
      agg_link_flag = network_links %>%
        count_percent_zscore(
          grp_c = c('network_link_ids_unnested', 'flag_trip_type')
          ,grp_p = c('network_link_ids_unnested')
          ,col = count, rnd = 2) %>%
        group_by(network_link_ids_unnested) %>%
        mutate(ttl_count_link = sum(count)) %>%
        ungroup() %>%
        mutate(count_nrm_prank_ttl = gauntlet::dgt2(percent_rank(ttl_count_link))
               ,count_nrm_mmax_ttl = gauntlet::dgt2(normalize_min_max(ttl_count_link))) %>%
        group_by(flag_trip_type) %>%
        mutate(count_nrm_prank = gauntlet::dgt2(percent_rank(count))
               ,count_nrm_mmax = gauntlet::dgt2(normalize_min_max(count))) %>%
        ungroup()

      agg_link_flag_mrg = merge(
        network_object, agg_link_flag
        ,by.x = "stableEdgeId", by.y = "network_link_ids_unnested", all = T)

      agg_link_flag_mrg_pro = agg_link_flag_mrg %>%
        filter(!is.na(flag_trip_type )) %>%
        mutate(label = str_glue(
          "Link Name: {streetName} ({highway})
          <br>TTl Link Volume: {ttl_count_link} - {100*count_nrm_mmax_ttl}% (Min-Max norm.)
          <hr>
          Metrics Adj for Trip Type: {flag_trip_type}
          <br>Link Volume: {count}
          <br>Link Volume (Min-Max norm.): {100*count_nrm_mmax}%"))

      message("Aggregation complete....")
    }

    #ANLTO
    {
      message(str_glue("{make_space()}\nStarting aggreagtion by link, vehicle type, and originating poly....{gauntlet::make_space('-')}"))
      message("INFO:\nBy default external-external and external-internal trips are removed before aggregation")
      message("This is because origin based visualizations and calcultions display locations WITHIN/INTERNAL TO the user provided study area...")
      message(str_glue("Prefiltering makes processing faster and limits size of data object{gauntlet::make_space('-')}"))

      agg_link_vehicle_type_origin = network_links %>%
        filter(!is.na(vehicle_type)) %>%
        filter(origin_poly != "out of study area") %>%
        count_percent_zscore(
          grp_c = c('network_link_ids_unnested', 'vehicle_type', 'origin_poly')
          ,grp_p = c('network_link_ids_unnested', 'origin_poly')
          ,col = count, rnd = 2) %>%
        group_by(network_link_ids_unnested) %>%
        mutate(ttl_count_link = sum(count)) %>%
        ungroup() %>%
        mutate(count_nrm_prank_ttl = gauntlet::dgt2(percent_rank(ttl_count_link))
               ,count_nrm_mmax_ttl = gauntlet::dgt2(normalize_min_max(ttl_count_link))) %>%
        group_by(origin_poly) %>%
        mutate(ttl_count_orgin = sum(count)) %>%
        ungroup() %>%
        group_by(origin_poly, vehicle_type) %>%
        mutate(count_nrm_prank = dgt2(percent_rank(count))
               ,count_nrm_mmax = dgt2(normalize_min_max(count))) %>%
        ungroup() %>%
        group_by(origin_poly, vehicle_type) %>%
        mutate(ttl_count_orgin_type = sum(count)) %>%
        ungroup()

      agg_link_vehicle_type_origin_mrg = merge(
        network_object, agg_link_vehicle_type_origin
        ,by.x = "stableEdgeId", by.y = "network_link_ids_unnested", all = T)

      nrow(filter(agg_link_flag_mrg, is.na(flag_trip_type)))
      (nrow(filter(agg_link_flag_mrg, is.na(stableEdgeId))) == 0)

      agg_link_vehicle_type_origin_mrg_pro = agg_link_vehicle_type_origin_mrg %>%
        mutate(label = str_glue(
          "Origin: {origin_poly} - {ttl_count_orgin} (total origin trips)
          <br>Link name: {streetName} ({highway})
          <br>Link Volume: {ttl_count_link}
          <hr>
          Metrics Adj for Origin and Vehicle Type: {vehicle_type}
          <br>Link Volume: {count} - {100*count_nrm_mmax}% (Min-Max norm.)
          <br>TTl Trips from Origin by Veh. Type: {ttl_count_orgin_type} ({100*gauntlet::dgt2(ttl_count_orgin_type/ttl_count_orgin)})"))

      message("Aggregation complete....")}
  }

  #save out
  {
    list_objects = list(agg_link = agg_link_mrg_pro
                        ,agg_link_flag = agg_link_flag_mrg_pro
                        ,agg_link_vehicle_type = agg_link_vehicle_type_mrg_pro
                        ,agg_link_vehicle_type_origin = agg_link_vehicle_type_origin_mrg_pro)

    if (auto_save) {
      message("You elected to automatically save the returned list object!")
      file = here::here(location, folder, "aggregated_network_links.rds")
      message(str_glue("It is saved at this location:\n{file}"))
      saveRDS(list_objects, file)
    } else {
      message("You did not elect to automatically save the returned list object!")
    }
  }

  return(list_objects)

}


#' Convert tabular network link count objects to SF polyline objects.
#'
#' @description This functions takes the three tabular network link counts data frames and converts them to individual SF objects contained within a list.
#' It can be supplied with either the list object directly or supplied location and folder strings to point to the folder the list is in.
#'
#'
#' @param location character string pointing to top level location where data acquired from google was saved to.
#' @param folder character string of name where data was automatically saved to from google data download.
#' @param auto_save boolean (T/F - default F) indicating if you want the GIS layer to be saved. Default just creates an object without saving.
#' @param aggregate_object list object containing the network link counts aggregated at different levels.
#' @param network_link_object network object containing links. Default is NULL or input left empty - function will use location and folder inputs to load object and then convert.
#'
#' @return a data frame and/or saved RDS file
#' @export
#'
#' @examples
#' data('aggregated_network_links')
#' data('replica_queried_network_links')
#'
#' make_agg_network_shapefile_links(
#'   aggregate_object = aggregated_network_links
#'   ,network_link_object = replica_queried_network_links
#'   ,auto_save = F
#' )
make_agg_network_shapefile_links = function(location, folder, auto_save = F
                                            ,aggregate_object = NULL, network_link_object = NULL){

  # location = "data/req_dev"
  # folder = 'data_20230125_162034'

  if (is.null(aggregate_object)){
    message("Aggregate network links objects will be made using aggregate tables located at folder and location...")
    aggregated_network_links = here::here(location, folder, "aggregated_network_links.rds") %>%
      readr::read_rds()
    replica_trip_origin_links = aggregated_network_links
  } else {
    message("Aggregate network links objects will be made using supplied aggregate table...")
    aggregated_network_links = aggregate_object
  }

  if (is.null(network_link_object)){
    network_link_object = here::here(location, folder, "replica_queried_network_links.gpkg") %>%
      sf::read_sf()

  } else {
    message("network_link_object will be made using supplied network link object...")
    network_object = network_link_object
  }

  message(str_glue("Making shapefiles by merging data centroids with aggregated count data{make_space('-')}"))
  message("This may take awhile depending on the size of the network and count data")

  temp_object = c("agg_link_flag", "agg_link_vehicle_type", "agg_link_vehicle_type_origin") %>%
    map(~{

      message(str_glue("Making {.x}...."))

      network_object_mrgd = network_link_object %>%
        mutate(stableEdgeId_trunc = str_trunc(stableEdgeId, 14, "right", "")) %>%
        merge(aggregated_network_links[[.x]] %>%
                mutate(network_link_ids_unnested_trunc = str_trunc(network_link_ids_unnested , 14, "right", ""))
              ,by.x = "stableEdgeId_trunc", by.y = "network_link_ids_unnested_trunc", all = T)

      # summary_output = network_object_mrgd %>%
      #   st_drop_geometry() %>%
      #   count(stable_mis = !is.na(stableEdgeId)
      #         ,network_mis = !is.na(network_link_ids_unnested)) %>%
      #   mutate(flag = case_when(
      #     (stable_mis == T & network_mis == T)~"Successful data matches"
      #     ,(stable_mis == F & network_mis == T)~"Agg. table link present but no match in network object"
      #     ,(stable_mis == T & network_mis == F)~"Network object link present but no match in agg. table"
      #     ,T~"ERROR")) %>%
      #   select(flag, count = n)

      # message(str_glue("{paste0(capture.output(summary_output), collapse = '\n')}"))

    })

  #save out
  {
    list_objects = list(agg_link_flag = temp_object[[1]]
                        ,agg_link_vehicle_type = temp_object[[2]]
                        ,agg_link_vehicle_type_origin = temp_object[[3]])

    if (auto_save) {
      message("You elected to automatically save the returned list object!")
      file = here::here(location, folder, "network_link_aggregation_list.rds")
      message(str_glue("It is saved at this location:\n{file}"))
      saveRDS(list_objects, file)
    } else {
      message("You did not elect to automatically save the returned list object!")
    }
  }

  return(list_objects)

}


#' Convert tabular network link count objects to SF point objects.
#'
#' @description This functions takes the three tabular network link counts data frames and converts them to individual SF objects contained within a list.
#' It can be supplied with either the list object directly or supplied location and folder strings to point to the folder the list is in.
#'
#' Note: This should be used if you intend to visualize these objects with crosstalk.
#'
#'
#' @param location character string pointing to top level location where data acquired from google was saved to.
#' @param folder character string of name where data was automatically saved to from google data download.
#' @param auto_save boolean (T/F - default F) indicating if you want the GIS layer to be saved. Default just creates an object without saving.
#' @param aggregate_object list object containing the network link counts aggregated at different levels.
#' @param network_centroid_object network object containing links. Default is NULL or input left empty - function will use location and folder inputs to load object and then convert.
#'
#' @return a data frame and/or saved RDS file
#' @export
#'
#' @examples
#' data('aggregated_network_links')
#' data('replica_queried_network_cntds')
#'
#' make_agg_network_shapefile_centroids(
#'   aggregate_object = aggregated_network_links
#'   ,network_centroid_object = replica_queried_network_links
#'   ,auto_save = F
#' )
make_agg_network_shapefile_centroids = function(location, folder, auto_save = F
                                                ,aggregate_object = NULL
                                                ,network_centroid_object = NULL){
  #TODO: write out somewhere which links do not have counts

  # location = "data/req_dev"
  # folder = 'data_20230125_162034'
  # aggregate_object = aggregated_network_links
  # network_centroid_object = replica_queried_network_cntds

  if (is.null(aggregate_object)){
    message("Aggregate network links objects will be made using aggregate tables located at folder and location...")
    aggregated_network_links = here::here(location, folder, "aggregated_network_links.rds") %>%
      readr::read_rds()
    replica_trip_origin_links = aggregated_network_links
  } else {
    message("Aggregate network links objects will be made using supplied aggregate table...")
    aggregated_network_links = aggregate_object
  }

  if (is.null(network_centroid_object)){
    message("You elected to use folder and location pointers to make spaital object...")

    # message("In addition, you elected to repersent network links as polylines....\nIf you wish to depict network as links as points using their centroids change 'use_centroids = T'")
    network_centroid_object = here::here(location, folder, "replica_queried_network_cntds.gpkg") %>%
      sf::read_sf()

  } else {
    message("Shapefile will be made using supplied network link object...")
    network_centroid_object = network_centroid_object
  }

  message(str_glue("Making shapefiles by merging data centroids with aggregated count data{make_space('-')}"))
  message("This may take awhile depending on the size of the network and count data")

  temp_object = c("agg_link_flag", "agg_link_vehicle_type", "agg_link_vehicle_type_origin") %>%
    map(~{

      message(str_glue("Making {.x}...."))

      yolo = network_centroid_object %>%
        mutate(stableEdgeId_trunc = str_trunc(stableEdgeId, 14, "right", "")) %>%
        merge(aggregated_network_links[[.x]] %>%
                mutate(network_link_ids_unnested_trunc = str_trunc(network_link_ids_unnested , 14, "right", ""))
              ,by.x = "stableEdgeId_trunc", by.y = "network_link_ids_unnested_trunc", all = T) %>%
        filter(!is.na(network_link_ids_unnested)) %>%
        unique() %>%
        st_collection_extract("POINT")

    })

  #save out
  {
    list_objects = list(agg_link_flag = temp_object[[1]]
                        ,agg_link_vehicle_type = temp_object[[2]]
                        ,agg_link_vehicle_type_origin = temp_object[[3]])

    if (auto_save) {
      message("You elected to automatically save the returned list object!")
      file = here::here(location, folder, "network_centroid_aggregation_list.rds")
      message(str_glue("It is saved at this location:\n{file}"))
      saveRDS(list_objects, file)
    } else {
      message("You did not elect to automatically save the returned list object!")
    }
    }

  return(list_objects)

}


