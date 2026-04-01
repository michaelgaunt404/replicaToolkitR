#' Convert tabular replica network file to GIS layers and save to disk for later use.
#'
#' @description
#' This function converts a Replica/Google-downloaded network stored as a CSV (with WKT `geometry`) into an `sf`
#' LINESTRING layer (CRS 4326) and writes it to disk in the project directory (via `here::here(location, folder, ...)`).
#' Optionally, it also creates a POINT layer representing each link by its true midpoint and writes that as well.
#'
#' **Important:** This function is designed to *persist outputs for later re-loading*. It saves files and returns nothing.
#'
#' @param location Character string pointing to the top-level project location where network data is stored.
#' @param folder Character string of the folder name under `location` where the downloaded files live.
#' @param network_object Optional tabular network object containing a WKT `geometry` column. If `NULL`, the function
#'   searches `here::here(location, folder)` for a matching CSV and loads it.
#' @param make_centroid Logical; if `TRUE` (default), also creates a POINT layer using the true midpoint of each link.
#'
#' @return Invisibly returns the LINESTRING `sf` object (`replica_queried_network`). Primary outputs are saved to disk.
#'
#' @section Files written:
#' The function writes the following files under `here::here(location, folder)`:
#' \itemize{
#'   \item `replica_queried_network_links.gpkg` (LINESTRING links)
#'   \item `replica_queried_network_links.qs` (LINESTRING links)
#'   \item `replica_queried_network_cntds.gpkg` (POINT midpoints; if `make_centroid = TRUE`)
#'   \item `replica_queried_network_cntds.qs` (POINT midpoints; if `make_centroid = TRUE`)
#' }
#'
#' @importFrom here here
#' @importFrom data.table fread
#' @importFrom sf st_as_sf st_geometry_type write_sf
#' @importFrom dplyr mutate select case_when
#' @importFrom stringr str_detect str_trunc
#' @importFrom qs qsave
#' @export
#'
#' @examples
#' \dontrun{
#' data('replica_queried_network')
#'
#' head(replica_queried_network)
#'
#' postprocess_replica_network_layers(
#'   network_object = replica_queried_network
#'   ,auto_save = F, make_centroid = T
#' )
#' }
postprocess_replica_network_layers =  function(location, folder, network_object = NULL, make_centroid = T){

  auto_save = T

  if (is.null(network_object)){
    tryCatch({

    message("Spatial links were made using CSV from file and location...")

    replica_queried_network = here::here(location, folder) %>%
      list.files(full.names = T, pattern = "csv") %>%
      .[stringr::str_detect(., "replica_network|replica_queried_network")] %>%
      data.table::fread()
    replica_queried_network = sf::st_as_sf(replica_queried_network, wkt = "geometry", crs = 4326)

    }, error = function(err) {
      message("An error orccured...")
      print(err$message)
    })


  } else {
    message("Spatial links were made using supplied tabular network object...")
    replica_queried_network = sf::st_as_sf(network_object, wkt = "geometry", crs = 4326)
  }

  stopifnot("Network object had more than one geometry feature in it, please review and fix...." = (length(unique(sf::st_geometry_type(replica_queried_network))) == 1))
  stopifnot("ERROR: Nerwork object must be LINESTRING, please review and fix...." = (unique(sf::st_geometry_type(replica_queried_network))[[1]] %in% c("LINESTRING")))

  replica_queried_network = replica_queried_network %>%
    mutate(flag_highway = dplyr::case_when(
      (stringr::str_detect(streetName, "^US") |
         stringr::str_detect(streetName, "^I [[:digit:]]+") |
         stringr::str_detect(streetName, "^SR [[:digit:]]+") |
         stringr::str_detect(streetName, "State Route") #|
       # str_detect(streetName, str_glue("^({paste0(pull(tigris::states(), STUSPS), collapse = '|')}) [[:digit:]]+"))
      )~"Flagged Highway"
      ,T~"Not a Highway")) %>%
    dplyr::mutate(stableEdgeId = str_trunc(stableEdgeId , 14, "right", "")) %>%
    dplyr::select(stableEdgeId, streetName, distance, osmid, highway, flag_contains, flag_highway)


  if (auto_save) {
    message("Writing network to gpkg...")
    sf::write_sf(replica_queried_network, here::here(location, folder, "replica_queried_network_links.gpkg"))
    message("Writing network to qs object...")
    qs::qsave(replica_queried_network, here::here(location, folder, "replica_queried_network_links.qs"))
  }


  if (make_centroid){

    message("User elected to make gis layer repersenting networks polylines by their center points... perfroming now")

    replica_queried_network_cntds = gauntletMap::st_true_midpoint(replica_queried_network) %>%
      dplyr::select(!c(merge_id))

    stopifnot("Supplied network object had more than one geometry feature in it, please review and fix...." = (length(unique(st_geometry_type(replica_queried_network_cntds))) == 1))
    stopifnot("Supplied network object must be either POINT or LINESTRING, please review and fix...." = (unique(st_geometry_type(replica_queried_network_cntds))[[1]] %in% c("POINT")))

    if (auto_save) {
      message("Writing network to gpkg...")
      sf::write_sf(replica_queried_network_cntds, here::here(location, folder, "replica_queried_network_cntds.gpkg"))
      message("Writing network to qs object...")
      qs::qsave(replica_queried_network_cntds, here::here(location, folder, "replica_queried_network_cntds.qs"))
    }


  }




}
