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
#' \dontrun{
#' data('replica_queried_network_links')
#'
#' head(replica_queried_network_links)
#'
#' postprocess_make_network_centroid_layer(
#'   location = "data/req_dev"
#'   ,folder = "data_20230106_154119"
#'   ,network_object = replica_queried_network_links
#'   ,auto_save = F
#' )
#' }
postprocess_make_network_centroid_layer = function(location, folder, auto_save = F, network_object = NULL){

  # network_object = replica_queried_network_links

  if (is.null(network_object)){
    message("Centroids were made using spatial links from file and location...")
    replica_queried_network = here::here(location, folder, "replica_queried_network_links.gpkg") %>%
      sf::read_sf()
    replica_queried_network_cntds = gauntletMap::st_true_midpoint(replica_queried_network) %>%
      select(!c(merge_id))
  } else {
    message("Centroids were made using supplied spatial links network object...")
    replica_queried_network_cntds = gauntletMap::st_true_midpoint(network_object) %>%
      select(!c(merge_id))
  }

  stopifnot("Supplied network object had more than one geometry feature in it, please review and fix...." = (length(unique(st_geometry_type(replica_queried_network_cntds))) == 1))
  stopifnot("Supplied network object must be either POINT or LINESTRING, please review and fix...." = (unique(st_geometry_type(replica_queried_network_cntds))[[1]] %in% c("POINT")))

  if (auto_save) {
    message("Writing network to gpkg...")
    sf::write_sf(replica_queried_network_cntds, here::here(location, folder, "replica_queried_network_cntds.gpkg"))
    message("Writing network to qs object...")
    qs::qsave(replica_queried_network_cntds, here::here(location, folder, "replica_queried_network_cntds.qs"))
  }

  return(replica_queried_network_cntds)
}
