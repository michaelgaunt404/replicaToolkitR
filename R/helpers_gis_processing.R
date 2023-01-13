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
#'make_network_layer(
#'  network_object = replica_queried_network
#'  ,auto_save = F
#')
make_network_link_layer = function(location, folder, auto_save = F, network_object = NULL){
  if (is.null(network_object)){
    message("Centroids made using file and location...")
    replica_queried_network = here::here(location, folder, "replica_queried_network.csv") %>%
      data.table::fread()
    replica_queried_network = sf::st_as_sf(replica_queried_network, wkt = "geometry", crs = 4326)
  } else {
    message("Centroids made using suplied network object...")
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
make_network_centroid_layer = function(location, folder, auto_save = F, network_object = NULL){

  if (is.null(network_object)){
    message("Centroids made using file and location...")
    replica_queried_network = here::here(location, folder, "replica_queried_network_links.gpkg") %>%
      sf::read_sf()
    replica_queried_network_cntds = gauntlet::st_true_midpoint(replica_queried_network)
  } else {
    message("Centroids made using suplied network object...")
    replica_queried_network_cntds = gauntlet::st_true_midpoint(network_object)
  }

  if (auto_save) {
    sf::write_sf(replica_queried_network_cntds, here::here(location, folder, "replica_queried_network_cntds.gpkg"))
  }

  return(replica_queried_network_cntds)
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
    st_transform(4326)

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
    st_drop_geometry() %>%
    filter(flag_index == 1) %>%
    count(flag_blkgrps, flag_index) %>%
    mutate(percent = paste0(100*(n/sum(n)), "%")) %>%
    capture.output() %>%
    paste0(collapse = "\n")

  str_glue("{make_space('-')}\n1 - 1 combinations indicate good matches") %>%  message()
  str_glue("Anything else indicates bad poly matches") %>%  message()
  str_glue("The following query resulted in the following matches...\n{check_match}") %>%  message()

  block_groups_sub = block_groups_sub %>%
    filter(flag_poly_merge == "Both Tirgris and Replica") %>%
    select(!c(flag_blkgrps, flag_index, flag_poly_merge))

  str_glue("Replica query acquired {nrow(replica_queried_network)} polygons\nPolygon query returning {nrow(block_groups_sub)} polygons\n{100*nrow(block_groups_sub)/nrow(replica_queried_network)}% match{make_space('-')}") %>%  message()

  if (auto_save) {
    sf::write_sf(block_groups_sub, here::here(location, folder, "acquired_sa_polys.gpkg"))
  }

  return(block_groups_sub)
}
