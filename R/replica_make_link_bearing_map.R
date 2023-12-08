
#needs documenation
#returns a leaflet object
#can be used alone but mostly intended to use for map selection via mapedit
#can also be

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
    palette = colorRamp(c("#96B856", "#56B8A9", "#7856B8",  "#B85665", "#96B856"), interpolate = "spline")
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




