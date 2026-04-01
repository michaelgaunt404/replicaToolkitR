#' Query Study Area Polygons from Replica
#'
#' @description Downloads Block Groups, Tracts, and ZCTAs based on a trip subset file.
#'
#' @param location Base directory path.
#' @param folder Project folder name.
#' @param customer_name Replica customer BigQuery project name.
#' @param get_tracts Logical; if TRUE, fetches Census Tracts.
#' @param get_zctas Logical; if TRUE, fetches ZCTAs (requires Tracts).
#' @param auto_save Logical; if TRUE, saves files to disk as .gpkg and .qs.
#'
#' @importFrom dplyr select pull %>%
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_trunc str_glue
#' @importFrom sf st_as_sf st_union st_simplify write_sf
#' @importFrom here here
#' @importFrom qs qsave
#' @importFrom bigrquery bq_project_query bq_table_download
#'
#' @examples
#' \dontrun{
#' query_studyarea_polys_from_replica(
#'   location = "data",
#'   folder = "my_project",
#'   get_zctas = TRUE
#' )
#' }
#' @export
query_studyarea_polys_from_replica = function(
    location, folder, customer_name = "replica-customer",
    get_tracts = FALSE, get_zctas = FALSE, auto_save = TRUE) {

  if (get_zctas) get_tracts <- TRUE

  message("Reading in 'trip subset' table and querying Blockgroups...")
  temp_data = postprocess_load_replica_file(location, folder, flag_qs = T, file_stem = "table_trips_subset") %>%
    dplyr::select(dplyr::ends_with("_bgrp")) %>%
    tidyr::pivot_longer(everything(), values_to = "GEOID") %>%
    dplyr::pull(GEOID) %>% unique()

  bg_polys = rplc_fetch_geos(temp_data, "bgrp", customer_name)

  if (auto_save) {
    sf::write_sf(bg_polys, here::here(location, folder, "replica_study_area_blkgrp.gpkg"))
    qs::qsave(bg_polys, here::here(location, folder, "replica_study_area_blkgrp.qs"))
  }

  if (get_tracts) {
    trct_ids = stringr::str_trunc(temp_data, 11, "right", "")
    trct_polys = rplc_fetch_geos(trct_ids, "trct", customer_name) %>%
      sf::st_as_sf(wkt = "geom", crs = 4326)

    if (auto_save) {
      sf::write_sf(trct_polys, here::here(location, folder, "replica_study_area_trct.gpkg"))
      qs::qsave(trct_polys, here::here(location, folder, "replica_study_area_trct.qs"))
    }

    if (get_zctas) {
      message("Querying ZCTAs via spatial intersect...")
      tract_area_wkt = trct_polys %>%
        gauntletMap::st_quick_buffer(radius = 10000) %>%
        sf::st_union() %>%
        sf::st_as_sf() %>%
        gauntletMap::st_quick_buffer(radius = 10000) %>%
        gauntletMap::st_quick_buffer(radius = -9000) %>%
        sf::st_simplify(dTolerance = 1000) %>%
        rplc_layer_extent_loadUnionWkt(layer = ., layer_name = "tract_area")

      zcta_query = stringr::str_glue("
        SELECT * FROM `{customer_name}.Geos.ZCTA`
        WHERE ST_INTERSECTS(ST_GEOGFROMTEXT('{tract_area_wkt}'), geom)")

      zcta_polys = bigrquery::bq_table_download(
        bigrquery::bq_project_query(customer_name, zcta_query, quiet = TRUE)
      ) %>% sf::st_as_sf(wkt = "geom", crs = 4326)

      if (auto_save) {
        sf::write_sf(zcta_polys, here::here(location, folder, "replica_study_area_zctas.gpkg"))
        qs::qsave(zcta_polys, here::here(location, folder, "replica_study_area_zctas.qs"))
      }
    }
  }
}
