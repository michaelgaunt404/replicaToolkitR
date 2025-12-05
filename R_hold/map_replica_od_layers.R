#' Create an Interactive Map of Origin-Destination Trips
#'
#' The \code{map_replica_od_layers} function generates an interactive map
#' visualizing origin-destination data for trips. It takes a spatial polygon
#' layer of origin polygons and an origin-destination trip table with counts,
#' merges and aggregates the data, and creates two distinct map layers. These
#' layers are then combined into a singular map for visualization.
#'
#' @param acquired_sa_polys Spatial polygon layer of origin polygons.
#' @param replica_trip_origin_destination Origin-destination trip table with counts.
#'
#' @return An interactive map with two layers: one showing counts for each
#' origin-destination pair and another aggregating counts by vehicle types.
#'
#' @examples
#' \dontrun{
#' # Load required libraries
#' library(dplyr)
#' library(purrr)
#' library(leaflet)
#'
#' # Assuming acquired_sa_polys and replica_trip_origin_destination are available
#' result_map <- map_replica_od_layers(acquired_sa_polys, replica_trip_origin_destination)
#'
#' # Display the result_map
#' result_map
#' }
#'
#' @note
#' - Ensure that the required libraries (\code{dplyr}, \code{purrr}, \code{leaflet})
#'   are loaded before calling the function.
#' - The function utilizes the \code{mapview} and \code{leaflet} packages for creating
#'   interactive maps. Ensure they are installed in your R environment.
#' - The function assumes specific column names such as "GEOID10," "ALAND10," "count,"
#'   "origin_poly," and "vehicle_type" in the input datasets. Adjust column names if necessary.
#'
#' @seealso
#' \code{\link{mapview}}, \code{\link{leaflet}}
#'
#' @import dplyr
#' @import purrr
#' @import leaflet
#'
#' @export
map_replica_od_layers = function(
    acquired_sa_polys
    ,replica_trip_origin_destination){

  temp = acquired_sa_polys %>%
    merge(replica_trip_origin_destination %>%
            group_by(origin_poly, vehicle_type) %>%
            summarise(count = sum(count)) %>%
            ungroup()
          ,by.x = "GEOID10", by.y = "origin_poly")

  temp_agg = acquired_sa_polys %>%
    merge(replica_trip_origin_destination %>%
            group_by(origin_poly) %>%
            summarise(count = sum(count)) %>%
            ungroup()
          ,by.x = "GEOID10", by.y = "origin_poly")

  od_map = unique(temp$vehicle_type) %>%
    map(~{
      temp %>%
        filter(vehicle_type == .x) %>%
        mutate(area_mi2 = round(ALAND10/2589988, 2)
               ,count_density = round(count/area_mi2, 2)) %>%
        mapview(zcol = "count_density"
                ,color = "black"
                ,layer.name  = str_glue("{str_replace(.x, '_COMMERCIAL', ' Duty')} (trips per mi2)")
                ,homebutton = F)
    }) %>%
    reduce(`+`)

  od_agg_map = temp_agg %>%
    mutate(area_mi2 = round(ALAND10/2589988, 2)
           ,count_density = round(count/area_mi2, 2)) %>%
    mapview(zcol = "count"
            ,color = "black"
            ,layer.name  = str_glue("All (trips per mi2)")
            ,homebutton = F)

  temp_od = (od_agg_map + od_map) %>%
    .@map %>%
    leaflet::hideGroup(c("HEAVY Duty (trips per mi2)"
                         ,"MEDIUM Duty (trips per mi2)"))

  return(temp_od)

}
