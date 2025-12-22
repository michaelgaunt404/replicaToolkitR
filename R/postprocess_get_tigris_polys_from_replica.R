#' Get polygons using Replica study area poly index
#'
#' This function retrieves census block group polygons corresponding to the
#' Replica study area, based on the poly index. Polygons can be loaded from a
#' saved Replica download or constructed from a supplied network object.
#'
#' @param location Character string pointing to the top-level directory where
#'   Replica data downloaded from Google was saved.
#' @param folder Character string indicating the subfolder automatically created
#'   by the Google data download.
#' @param auto_save Logical (default = FALSE). If TRUE, the resulting GIS layer
#'   is written to disk; otherwise, only the object is returned.
#' @param network_object Optional network object containing links. If NULL
#'   (default), the function loads the study area poly index from
#'   \code{location/folder}. If provided, the poly index is derived directly from
#'   this object.
#' @param year Integer year of Census TIGER/Line data to pull. Replica uses 2010
#'   block groups, so the default is 2010 and generally should not be changed.
#'
#' @return An \code{sf} object of block group polygons corresponding to the
#'   Replica study area.
#' @export
#'
#' @importFrom here here
#' @importFrom data.table fread
#' @importFrom dplyr select mutate across filter count arrange
#' @importFrom stringr str_trunc str_glue
#' @importFrom tidyr replace_na
#' @importFrom purrr pmap_df
#' @importFrom tigris block_groups
#' @importFrom sf st_transform st_drop_geometry write_sf
#'
#' @examples
#' \dontrun{
#' # postprocess_get_tigris_polys_from_replica("data/req_dev", "data_20230111_150315")
#' }

postprocess_get_tigris_polys_from_replica = function(
    location, folder, auto_save = F, network_object = NULL, year = 2010){

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
      dplyr::select(raw_id)
  } else {
    message("Study area index made using suplied network object...")
    replica_queried_network = network_object %>%
      dplyr::select(raw_id)
  }

  index_replica_queried_network = replica_queried_network %>%
    dplyr::mutate(
      states = raw_id %>%
        stringr::str_trunc(2, "right", ellipsis = "") %>%
        as.integer()
      ,counties = raw_id %>%
        stringr::str_trunc(5, "right", ellipsis = "") %>%
        stringr::str_trunc(3, "left", ellipsis = "") %>%
        as.integer()) %>%
    dplyr::select(states, counties) %>%
    unique()

  block_groups = list(index_replica_queried_network$states
                      ,index_replica_queried_network$counties) %>%
    pmap_df(~{

      tigris::block_groups(state = .x
                           ,county = .y
                           ,year = year
                           ,refresh = TRUE)
    }, progress = T) %>%
    sf::st_transform(4326)

  block_groups_sub = block_groups %>%
    dplyr::mutate(flag_blkgrps = 1) %>%
    merge(replica_queried_network %>%
            dplyr::mutate(flag_index = 1
                   ,raw_id = as.character(raw_id))
          , by.x = "GEOID10", by.y = "raw_id", all = T) %>%
    dplyr::mutate(dplyr::across(c(flag_blkgrps, flag_index),~tidyr::replace_na(.x, 0))) %>%
    dplyr::mutate(flag_poly_merge = dplyr::case_when(
      (flag_blkgrps == 1 & flag_index == 0)~"Tirgris but not Replica"
      ,(flag_blkgrps == 1 & flag_index == 1)~"Both Tirgris and Replica"
      ,(flag_blkgrps == 0 & flag_index == 1)~"Replica but not Tigris"
      ,T~"Error please check.."
    ))

  check_match = block_groups_sub %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(flag_index == 1) %>%
    count(flag_blkgrps, flag_index) %>%
    dplyr::mutate(percent = paste0(100*(n/sum(n)), "%")) %>%
    capture.output() %>%
    paste0(collapse = "\n")

  stringr::str_glue("{gauntlet::strg_make_space_2('-')}1 - 1 combinations indicate good matches") %>%  message()
  stringr::str_glue("Anything else indicates bad poly matches") %>%  message()
  stringr::str_glue("The following query resulted in the following matches...\n{check_match}") %>%  message()

  block_groups_sub = block_groups_sub %>%
    dplyr::filter(flag_poly_merge == "Both Tirgris and Replica") %>%
    dplyr::select(!c(flag_blkgrps, flag_index, flag_poly_merge))

  stringr::str_glue("Replica query acquired {nrow(replica_queried_network)} polygons\nPolygon query returning {nrow(block_groups_sub)} polygons\n{100*nrow(block_groups_sub)/nrow(replica_queried_network)}% match{make_space('-')}") %>%  message()

  if (auto_save) {
    sf::write_sf(block_groups_sub, here::here(location, folder, "acquired_sa_polys.gpkg"))
  }

  return(block_groups_sub)
}
