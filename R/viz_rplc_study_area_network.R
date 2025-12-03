#' Query Replica's roadway network using an interactive or supplied study area
#'
#' This function retrieves roadway network segments from Replica's BigQuery
#' dataset based on a user-defined study area. The user may either draw the
#' study area interactively using \code{mapedit} or supply an existing spatial
#' object. The function assembles the appropriate network table name based on
#' customer, dataset location, and dataset period, executes a spatial
#' intersection query in BigQuery, and returns both tabular and \code{sf} outputs.
#'
#' Compared to \code{view_replica_study_area_network()}, this updated version:
#' \itemize{
#'   \item Allows users to provide their own spatial extent (skipping the drawing step).
#'   \item Automatically constructs the network table using customer, location,
#'         and time-period inputs.
#'   \item Optionally filters results to specific roadway types, either supplied
#'         through \code{links_pro} or using a built-in default set.
#'   \item Uses robust internal prompting utilities to confirm user choices
#'         before running potentially large queries.
#' }
#'
#' @param customer_name Character. Replica customer name / BigQuery project ID.
#'   Used to build the network table path (e.g., \code{"my_customer"}).
#'
#' @param links_pro Character vector or \code{NA}. Optional user-supplied list of
#'   roadway types to limit the query (e.g., \code{c("primary", "secondary")}).
#'   If \code{NA} and the user chooses to limit the network, a default set of
#'   common roadway types is applied.
#'
#' @param study_area_extent \code{sf} object or \code{NULL}. If provided, this
#'   object is used directly as the spatial query extent. If \code{NULL}, the
#'   user is prompted to draw the study area interactively.
#'
#' @param data_set_location Character. Replica dataset location tag (e.g.,
#'   \code{"wa"} or \code{"hi"}). Used to construct the network table name.
#'
#' @param data_set_period Character. The Replica dataset period identifier
#'   (e.g., \code{"2023_q3"}). Also used to construct the network table name.
#'
#' @return A named list with three objects:
#' \describe{
#'   \item{\code{table_network}}{A BigQuery table reference created via \code{bigrquery::bq_project_query()}.}
#'   \item{\code{table_network_data}}{A data frame of network segments intersecting the study area.}
#'   \item{\code{sf_network_data}}{An \code{sf} object converted from the table results.}
#' }
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Builds a fully qualified network table name of the form
#'   \code{<customer_name>.<data_set_location>.<data_set_location>_<data_set_period>_network_segments}.
#'
#'   \item Obtains a study area geometry by:
#'     \itemize{
#'       \item prompting the user to draw one or more polygons, or
#'       \item using a provided \code{sf} object.
#'     }
#'
#'   \item Converts the study area to WKT and runs a BigQuery spatial
#'         intersection query using \code{ST_INTERSECTS()}.
#'
#'   \item Optionally limits the query to a specific set of link types, informed
#'         either by user input or defaults.
#'
#'   \item Prompts the user to confirm execution after reporting the number of
#'         candidate roadway segments returned.
#'
#'   \item Downloads, arranges, and returns both the raw table and \code{sf}
#'         versions of the network.
#' }
#'
#' @importFrom stringr stringr::str_glue
#' @importFrom bigrquery bq_project_query bq_table_nrow bq_table_download
#' @importFrom dplyr arrange
#' @importFrom sf st_transform st_union st_as_sf st_as_text
#' @importFrom rlang parse_expr
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' out <- viz_rplc_study_area_network(
#'   customer_name = "my_customer_id",
#'   links_pro = NA,
#'   study_area_extent = NULL,
#'   data_set_location = "wa",
#'   data_set_period = "2023_q4"
#' )
#' }
viz_rplc_study_area_network = function(
    customer_name
    ,links_pro = NA
    ,study_area_extent = NULL
    ,data_set_location
    ,data_set_period
){


  network_table = stringr::str_glue("{customer_name}.{data_set_location}.{data_set_location}_{data_set_period}_network_segments")

  if (is.null(study_area_extent)) {
    message(stringr::str_glue("{gauntlet::strg_make_space_2()}Please draw a study area that will be used to query Replica's roadway network..."))
    message("Draw it as small and parsimonious as possible")
    message(stringr::str_glue("You can draw multiple, discrete objects if you wish{gauntlet::strg_make_space_2(last = F)}"))

    # Use manual drawing method
    study_area = mapedit_robust_draw_2()
    study_area_wkt = sf::st_as_text(st_union(study_area))

  } else {
    message("Using the provided spatial extent for the study area.")
    study_area = study_area_extent %>% sf::st_transform(4326)
    study_area_wkt = sf::st_as_text(sf::st_union(study_area))

  }

  limit_links = gauntlet::robust_prompt_used("limit the number of links using the links supplied links_pro")

  if (limit_links) {
    if (is.na(links_pro)) {
      message("User did not supply a links_pro input....")
      continue_response = gauntlet::robust_prompt_used("would you like to continue without limiting the number of links")
      if (continue_response) {
        links_response = c('highway','corridor','road'
                           ,'motorway','motorway_link'
                           ,'trunk','trunk_link'
                           ,'primary','primary_link'
                           ,'secondary','secondary_link'
                           ,'tertiary','tertiary_link') %>%
          paste0("'", ., "'", collapse = ", ")
      } else {
        stop("Execution terminated by user.")
      }
    } else {
      links_response = links_pro
    }
  } else {
    links_response = c('highway','corridor','road'
                       ,'motorway','motorway_link'
                       ,'trunk','trunk_link'
                       ,'primary','primary_link'
                       ,'secondary','secondary_link'
                       ,'tertiary','tertiary_link') %>%
      paste0("'", ., "'", collapse = ", ")
  }

  table_network = bigrquery::bq_project_query(
    customer_name,
    stringr::str_glue("select * from (
                      select *,
                      ST_INTERSECTS(
                      ST_GEOGFROMTEXT('{study_area_wkt}')
                      ,geometry) as flag_contains
                      from `{network_table}`
                      --where highway in ({links_response})
                      )
                      where flag_contains = TRUE"))

  table_network_count = bigrquery::bq_table_nrow(table_network)

  message(stringr::str_glue("{table_network_count} links were returned... \nWould you like to continue query execution or abort run completely..."))
  check_continue = gauntlet::robust_prompt_used("continue")
  stopifnot("Aborted" = check_continue)

  table_network_data = bigrquery::bq_table_download(table_network
                                                    ,page_size = 1000) %>%
    arrange(stableEdgeId)

  sf_network_data = table_network_data %>%
    sf::st_as_sf(wkt = "geometry", crs = 4326)

  return(
    list(table_network = table_network
         ,table_network_data = table_network_data
         ,sf_network_data = sf_network_data)
  )
}
