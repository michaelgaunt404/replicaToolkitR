#' Generate Leaflet Map with Recolored Network Links Based on Link Bearing
#'
#' This function takes a Polyline Spatial object (\code{sf_network_link}) and generates a Leaflet map
#' with network links colored based on their bearing. The function requires the input object to be
#' pre-processed and include a \code{bearing} attribute.
#'
#' @param sf_network_link A Spatial object (sf) representing network links with a \code{bearing} attribute.
#'                        The object must be pre-processed and include the required attributes.
#'
#' @return A Leaflet map with recolored network links based on their bearing.
#'
#' @details The function utilizes the Leaflet package to create an interactive map. It uses the
#' \code{colorNumeric} function from Leaflet to assign colors to links based on their bearing.
#' The function assumes that the input object has a \code{bearing} attribute, and this attribute
#' should be named exactly as "bearing" (hardcoded in the function).
#'
#' @examples
#' # Example usage:
#' # sf_network_link <- ... # Load your Spatial object with bearing attribute
#' # replica_make_link_bearing_map(sf_network_link)
#'
#' @import leaflet
#' @import leafem
#' @importFrom purrr map
#' @importFrom htmltools HTML
#' @importFrom grDevices colorRamp
#'
#' @export
replica_make_link_bearing_map = function(
    sf_network_link){
  #makes parlette
  #note: attreibtues hard coded
  #requires: label and bearing attributes

  pal_network = leaflet::colorNumeric(
    palette = "viridis"
    ,sf_network_link$bearing
    ,reverse = T)

  pal_network = leaflet::colorNumeric(
    palette = grDevices::colorRamp(c("#96B856", "#56B8A9", "#7856B8",  "#B85665", "#96B856"), interpolate = "spline")
    ,sf_network_link$bearing
    ,reverse = T)

  tmp_map = leaflet::leaflet() %>%
    leaflet::addTiles(group = "OSM (default)") %>%
    gauntletMap::leaflet_default_tiles() %>%
    leaflet::addPolylines(
      data = sf_network_link
      ,layerId = ~stableEdgeId
      ,color = ~pal_network(sf_network_link$bearing)
      ,opacity = 1
      ,weight = 5
      ,group = "Network Links"
      ,label = sf_network_link$label %>%
        purrr::map(htmltools::HTML)
    ) %>%
    leaflet::addLayersControl(
      baseGroups = gauntletMap::leaflet_default_tiles_index()
      ,overlayGroups = c("Network Links")
      ,options = leaflet::layersControlOptions(collapsed = F, sortLayers = F)) %>%
    leafem::addMouseCoordinates() %>%
    leaflet::addLegend(
      position = "bottomleft"
      ,title = "Link Bearing"
      ,group = "Network Links"
      ,pal = pal_network
      ,values = sf_network_link$bearing)

  return(tmp_map)
}




