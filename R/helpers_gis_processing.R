#' Convert tabular network file to gis layer.
#'
#' @description The network acquired from google is saved as a shapefile.
#' This function helps converts file from CSV to GIS layer and provides an option to save GIS object as a ".gpkg".
#'
#' @param location character string pointing to top level location where data acquired from google was saved to.
#' @param folder character string of name where data was automatically saved to from google data download.
#' @param auto_save boolean (T/F - default F) inicating if you want the GIS layer to be saved. Default just creates an object without saving.
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

  if (is.null(network_object)){
    message("Centroids were made using spatial links from file and location...")
    replica_queried_network = here::here(location, folder, "replica_queried_network_links.gpkg") %>%
      sf::read_sf()
    replica_queried_network_cntds = gauntlet::st_true_midpoint(replica_queried_network)
  } else {
    message("Centroids were made using supplied spatial links network object...")
    replica_queried_network_cntds = gauntlet::st_true_midpoint(network_object)
  }

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
make_trip_origin_point_layer = function(location, folder, auto_save = F, first_links_object = NULL){

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
    mutate(across(c(flag_blkgrps, flag_index),~replace_na(.x, 0))) %>%
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
#' @param auto_save boolean (T/F - default F) indicating if you want the GIS layer to be saved. Default just creates an object without saving.
#' @param network_object network object containing links. Default is NULL or input left empty - function will use location and folder inputs to load object and then convert.
#' @return a data frame and/or saved RDS file
#' @export
#'
#' @examples
#'
#' #none
aggregate_network_links = function(location, folder, auto_save = F
                                   ,network_object = NULL){
  #TODO:make this compatible with other polygon types
  #TODO: review different aggregations - they dont make the most sense


  # location = "data/req_dev"
  # folder = "data_20230117_092037"
  # network_object = NULL
  # auto_save = F

  if (is.null(network_object)){
    message("Aggregations will be made using file and location...")
    network_links = here::here(location, folder, "table_agg_by_link_subset_limited.csv") %>%
      data.table::fread()
  } else {
    message("Aggregations will be made using supplied network object...")
    network_links = network_object
  }

  #perfrom aggregations
  {
    message(str_glue("{make_space()}\nStarting aggreagtion by link and study area destination flag...."))

    agg_link_flag = network_links %>%
      count_percent_zscore(
        grp_c = c('network_link_ids_unnested', 'flag_sa_destination')
        ,grp_p = c('network_link_ids_unnested')
        ,col = count, rnd = 2)

    message("Aggregation complete....")

    message(str_glue("{make_space()}\nStarting aggreagtion by link and vehicle type...."))

    agg_link_vehicle_type = network_links %>%
      count_percent_zscore(
        grp_c = c('network_link_ids_unnested', 'vehicle_type')
        ,grp_p = c('network_link_ids_unnested')
        ,col = count, rnd = 2) %>%
      .[,`:=`(count_nrm_prank = dgt2(percent_rank(count))
              ,count_nrm_mmax = dgt2(normalize_min_max(count)))
        ,by = .(vehicle_type)] %>%
      .[,`:=`(ttl_count_link = sum(count)), by = .(network_link_ids_unnested)] %>%
      .[,`:=`(ttl_count_link_nrm_mmax =  dgt2(normalize_min_max(ttl_count_link)))] %>%
      data.frame() %>%
      mutate(label = str_glue(
        "Link No.: {network_link_ids_unnested}
    <br>Total Link Volume: {ttl_count_link}
    <br>Volume Min-Max norm.: {100*ttl_count_link_nrm_mmax}%
    <hr>
    Metrics Adj for Vehicle Type
    <br>Link Volume: {count} ({100*dgt2(percent)}% of total)
    <br>Volume Min-Max norm.: {100*count_nrm_mmax}%")) %>%
      data.table()

    message("Aggregation complete....")

    message(str_glue("{make_space()}\nStarting aggreagtion by link, vehicle type, and originating poly...."))

    agg_link_vehicle_type_origin = network_links %>%
      count_percent_zscore(
        grp_c = c('origin_poly', 'network_link_ids_unnested', 'vehicle_type')
        ,grp_p = c('origin_poly', 'network_link_ids_unnested')
        ,col = count, rnd = 2) %>%
      .[order(origin_poly, network_link_ids_unnested)] %>%
      .[,`:=`(count_nrm_prank = dgt2(percent_rank(count))
              ,count_nrm_mmax = dgt2(normalize_min_max(count)))
        ,by = .(origin_poly, vehicle_type)] %>%
      .[,`:=`(ttl_count_orgin = sum(count)), by = .(origin_poly)] %>%
      .[,`:=`(ttl_count_orgin_type = sum(count)), by = .(origin_poly, vehicle_type)] %>%
      # .[,`:=`(ttl_count_orgin_type_other = ttl_count_orgin-ttl_count_orgin_type
      #         ,ttl_count_orgin_type_per = ttl_count_orgin_type/ttl_count_orgin)] %>%
      data.frame() %>%
      mutate(label = str_glue(
        "Origin: {origin_poly}
    <br>Total Trips from Origin: {ttl_count_orgin}
    <br>By Vehicle Type: {vehicle_type} ttl_count_orgin_type (100*{ttl_count_orgin_type/ttl_count_orgin}%)
    <hr>
    Link Metrics (for vehicle type and origin):
    <br>Link Volume: {count}
    <br>Volume Min-Max norm.: {100*count_nrm_mmax}%")) %>%
      data.table()

    message("Aggregation complete....")
  }

  #save out
  {
    list_objects = list(agg_link_flag = agg_link_flag
                        ,agg_link_vehicle_type = agg_link_vehicle_type
                        ,agg_link_vehicle_type_origin = agg_link_vehicle_type_origin)

    if (auto_save) {
      message("You elected to automatically save the returned list object!")
      file = here::here(location, folder, "aggregated_network_links.rds")
      message(str_glue("It is saved at this location:\n{file}"))
      saveRDS(list_objects,file)
    } else {
      message("You did not elect to automatically save the returned list object!")
    }
  }

  return(list_objects)

}



