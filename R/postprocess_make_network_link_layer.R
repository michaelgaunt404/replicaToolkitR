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
#' \dontrun{
#' data('replica_queried_network')
#'
#' head(replica_queried_network)
#'
#' postprocess_make_network_link_layer(
#'   network_object = replica_queried_network
#'   ,auto_save = F
#' )
#' }
postprocess_make_network_link_layer =  function(location, folder, auto_save = F, network_object = NULL){

  if (is.null(network_object)){
    message("Spatial links were made using CSV from file and location...")
    replica_queried_network = here::here(location, folder) %>%
      list.files(full.names = T, pattern = "csv") %>%
      .[str_detect(., "replica_network|replica_queried_network")] %>%
      data.table::fread()
    replica_queried_network = sf::st_as_sf(replica_queried_network, wkt = "geometry", crs = 4326)
  } else {
    message("Spatial links were made using supplied tabular network object...")
    replica_queried_network = sf::st_as_sf(network_object, wkt = "geometry", crs = 4326)
  }

  stopifnot("Network object had more than one geometry feature in it, please review and fix...." = (length(unique(st_geometry_type(replica_queried_network))) == 1))
  stopifnot("ERROR: Nerwork object must be LINESTRING, please review and fix...." = (unique(st_geometry_type(replica_queried_network))[[1]] %in% c("LINESTRING")))

  replica_queried_network = replica_queried_network %>%
    mutate(flag_highway = dplyr::case_when(
      (str_detect(streetName, "^US") |
         str_detect(streetName, "^I [[:digit:]]+") |
         str_detect(streetName, "^SR [[:digit:]]+") |
         str_detect(streetName, "State Route") #|
       # str_detect(streetName, str_glue("^({paste0(pull(tigris::states(), STUSPS), collapse = '|')}) [[:digit:]]+"))
      )~"Flagged Highway"
      ,T~"Not a Highway")) %>%
    mutate(stableEdgeId = str_trunc(stableEdgeId , 14, "right", "")) %>%
    select(stableEdgeId, streetName, distance, osmid, highway, flag_contains, flag_highway)


  if (auto_save) {
    message("Writing network to gpkg...")
    sf::write_sf(replica_queried_network, here::here(location, folder, "replica_queried_network_links.gpkg"))
    message("Writing network to qs object...")
    qs::qsave(replica_queried_network, here::here(location, folder, "replica_queried_network_links.qs"))
  }

  return(replica_queried_network)
}
