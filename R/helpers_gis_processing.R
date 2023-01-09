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
#' This function converts GIS links to points by extracing the centroids.
#'
#' @param location character string pointing to top level location where data acquired from google was saved to.
#' @param folder character string of name where data was automatically saved to from google data download.
#' @param auto_save boolean (T/F - default F) inicating if you want the GIS layer to be saved. Default just creates an object without saving.
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
