#' Get polygons using Replica study area poly index.
#'
#' @param location character string pointing to top level location where data acquired from google was saved to.
#' @param folder character string of name where data was automatically saved to from google data download.
#' @param auto_save boolean (T/F - default F) indicating if you want the GIS layer to be saved. Default just creates an object without saving.
#' @param network_object network object containing links. Default is NULL or input left empty - function will use location and folder inputs to load object and then convert.
#' @param year integer indicating which year census polys should be acquired. Replica uses 2010 and this functions default is 2010, you should not change this.
#'
#' @return a spatial object containing polygons for study area polygons acquired from Replica.
#' @export
#'
#' @examples
#' #none
get_tigris_polys_from_replica_index_2 = function(
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
      select(raw_id)
  } else {
    message("Study area index made using suplied network object...")
    replica_queried_network = network_object %>%
      select(raw_id)
  }

  index_replica_queried_network = replica_queried_network %>%
    mutate(states = raw_id %>%
             str_trunc(2, "right", ellipsis = "") %>%
             as.integer()
           ,counties = raw_id %>%
             str_trunc(5, "right", ellipsis = "") %>%
             str_trunc(3, "left", ellipsis = "") %>%
             as.integer()) %>%
    select(states, counties) %>%
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
