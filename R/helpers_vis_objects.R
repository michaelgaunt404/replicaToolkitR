
#' Quickly map and inspect queried network links
#'
#' @description This is a convenience function which maps the network queried given user inputs.
#' Replica uses uncommon naming conventions for links and link types. It can be very beneficial to inspect these links once they are queried to ensure you are only including the links you car about in you analyis.
#'
#' This function creates a **crosstalk object** - a client-side, interactive, filterable HTML widget. It's effectively a bite-size dashboard that helps you better understand your network. You can filter links by link type and by street name.
#'
#' > **_NOTE:_** Network links are mapped as link _centroids_ - this is due to a quirk given the packages needed to create this feature.
#'
#' @param location character string pointing to top level location where data acquired from google was saved to.
#' @param folder character string of name where data was automatically saved to from google data download.
#' @param network_links_object network object containing links. Default is NULL or input left empty - function will use location and folder inputs to load object and then convert.
#'
#' @return an HTML widget map of acquired network links
#' @export
#'
#' @examples
#' data("replica_queried_network_links")
#'
#' inspect_queried_network(
#'   network_links_object = replica_queried_network_links
#' )
inspect_queried_network = function(location,folder,network_links_object = NULL){

  # location = "data/req_dev"
  # folder = "data_20230117_092037"
  # network_object = NULL
  # auto_save = F

  message(stringr::str_glue("{gauntlet::make_space('-')}\nW A R N I N G{gauntlet::make_space('-')}\nNetwork links are converted to LINK CENTROIDS and displayed in the map....\nPolylines cannot be filtered using the packages that create this feature{gauntlet::make_space()}"))

  if (is.null(network_links_object)){
    message("Map being created from spatial links object given file and location...")
    network_raw = here::here(location, folder, "replica_queried_network_links.gpkg") %>%
      sf::read_sf()
  } else {
    message("Map being created from supplied spatial links object ")
    network_raw = network_links_object
  }

  message("If this takes a long time try reducing the number of links displayed...")

  network_raw = network_raw %>%
    mutate(label = stringr::str_glue("Link Type: {highway}<br>Name: {streetName}"))

  pal = leaflet::colorFactor(
    rev(viridisLite::viridis(length(unique(network_raw$highway)),
    )),
    network_raw$highway)

  network_rawsd = crosstalk::SharedData$new(gauntlet::st_true_midpoint(network_raw))

  crosstalk::bscols(
    widths = c(9, 3, 4, 5)
    ,leaflet::leaflet(height = 500) %>%
      leaflet::addTiles() %>%
      leaflet::addCircleMarkers(data = network_rawsd
                                ,fillColor = ~pal(network_raw$highway)
                                ,color = "black"
                                ,opacity = .8
                                ,fillOpacity  = .5
                                ,weight = 1
                                ,radius = 5
                                ,label = network_raw$label %>%
                                  map(htmltools::HTML)
                                ,labelOptions = leaflet::labelOptions(noHide = F, textOnly = F))
    ,list(
      crosstalk::filter_checkbox("network_rawsd", "Select link element:", network_rawsd, ~highway)
      ,crosstalk::filter_select("network_rawsd_name", "Select specific street:", network_rawsd, ~streetName)
    )
    ,reactable::reactable(network_raw %>%
                            mutate(count = 1) %>%
                            st_drop_geometry() %>%
                            gauntlet::count_percent_zscore(grp_c = c(highway), grp_p = c(), rnd = 2, col = count) %>%
                            arrange(desc(count))
                          ,filterable = T, highlight = TRUE
                          ,compact = TRUE, fullWidth = T
                          ,wrap = FALSE, resizable = TRUE
                          , height = 400
                          ,striped = TRUE)
    ,reactable::reactable(network_raw %>%
                            mutate(count = 1) %>%
                            st_drop_geometry() %>%
                            gauntlet::count_percent_zscore(grp_c = c(streetName), grp_p = c(), rnd = 2, col = count) %>%
                            arrange(desc(count))
                          ,filterable = T, highlight = TRUE
                          ,compact = TRUE, fullWidth = T
                          ,wrap = FALSE, resizable = TRUE
                          ,pagination = F, height = 400
                          ,striped = TRUE)
  )
}


#' Create interactive HTML widget of network links (volumes agg. by origin and veh. type).
#'
#' @description This function creates an interactive HTML widget which displays network volumes aggregated by trips' origin poly and vehicle type.
#' This HTML contains filtering and mapping features that can be used to investigate the data.
#'
#' @param network_cntrd_object list object containing aggregated link centroid networks.
#' @param poi_list data frame object detailing id and group attributes for polygons that network links will be displayed for. These polygons are considered Points-of-Interests and should be within the study area polygon.
#' @param origin_polys processed acquired_sa_polys object created for this study.
#'
#' @return an HTML widget map of network volumes
#' @export
#'
#' @examples
#'
#' data("acquired_sa_polys")
#' data("poi_list")
#' data("network_centroid_aggregation_list")
#'
#' make_network_map_anlto(
#'   network_cntrd_object = network_centroid_aggregation_list
#'   ,poi_list = poi_list
#'   ,origin_polys = acquired_sa_polys
#' )
make_network_map_anlto = function(network_cntrd_object
                                  ,poi_list = NULL
                                  ,origin_polys = NULL){
  {
    # data("network_centroid_aggregation_list")
    # data("poi_list")
    # data("acquired_sa_polys")
    # origin_polys = acquired_sa_polys
  }
  stopifnot("Please supply acquired_sa_polys object, it is included in this map..." = !is.null(origin_polys))
  stopifnot("Please provide Point-of-Interest list of origin polygons...\nIt is required to limit the number data points that are mapped and focuses the analysis" = !is.null(poi_list))
  stopifnot("POI list column names must be 'id' and 'group' (lowercase), please change..." = (sum(names(poi_list) %in% c("id", "group")) == 2))

  # data("network_centroid_aggregation_list")
  # data("poi_list")

  # leaflet_default_tiles_index = c("OSM (default)", "Esri", "CartoDB")

  #create poly layer background
  {
    map_poly = origin_polys %>%
      merge(poi_list, by.x = "GEOID10", by.y = "id", all = T)

    query_poly_poi = map_poly %>%
      filter(!is.na(group))

    query_poly_nonpoi = map_poly %>%
      filter(is.na(group))

    map_elemet_poi = function(base_map){
      base_map %>%
        leaflet::addPolygons(data = query_poly_poi
                             ,fillColor = "blue"
                             ,fillOpacity = .4
                             ,color = "black", opacity = .5,weight = 2
                             ,group = "POI Polygons"
                             # ,label = query_poly_poi$label %>%
                             #   map(htmltools::HTML)
        ) %>%
        leaflet::addPolygons(data = query_poly_nonpoi
                             ,fillColor = "green", fillOpacity = .1
                             ,opacity = .4, weight = 1, color = "grey"
                             ,group = "Non-POI Study Area Polygons"
        )
    }
  }

  #process data
  {
  net = network_cntrd_object$agg_link_vehicle_type_origin %>%
    na.omit() %>%
    filter(origin_poly %in% poi_list$id) %>%
    merge(poi_list %>%
            mutate(id = as.character(id)), by.x = "origin_poly", by.y = "id", all.x = T)

  net_sd = crosstalk::SharedData$new(net)

  pal_centroids_od = leaflet::colorNumeric(
    palette = "magma"
    ,net$count
    ,reverse = T)
  }

  #make map
  {
  crosstalk::bscols(widths = c(3, 9)
                    ,list(
                      crosstalk::bscols(
                        widths = c(12, 12, 12, 6, 6, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12 , 12, 12)
                        ,htmltools::hr()
                        ,htmltools::HTML("Rdwy & Veh. Type Filters") %>%  htmltools::strong()
                        ,htmltools::hr()
                        ,crosstalk::filter_select("net_group", "Choose Origin Group:"
                                                  ,net_sd, ~group)
                        ,crosstalk::filter_select("net_origin_poly", "Choose Origin Poly:"
                                                  ,net_sd, ~origin_poly)
                        ,crosstalk::filter_checkbox("net_sd_vehicle_type", "Choose Vehicle Type:"
                                                    ,net_sd, ~vehicle_type, inline = T)
                        ,crosstalk::filter_checkbox("net_flag_highway", "Choose Rdwy Flag:"
                                                     ,net_sd, ~flag_highway, inline = T)
                        ,crosstalk::filter_checkbox("net_highway", "Choose Rdwy Type:"
                                                    ,net_sd, ~highway, inline = T)
                        ,crosstalk::filter_select("net_streetname", "Choose Specific Rdwy:"
                                                  ,net_sd, ~streetName)
                        ,htmltools::hr()
                        ,htmltools::HTML("Network Link Volume Filters") %>%  htmltools::strong()
                        ,htmltools::hr()
                        ,crosstalk::filter_slider("net_sd_count", "Link Volume (counts):"
                                                  ,net_sd, ~count)
                        ,crosstalk::filter_slider("net_count_nrm_mmax", "Link Volume (Min-Max Norm):"
                                                  ,net_sd, ~count_nrm_mmax)
                        ,crosstalk::filter_slider("net_count_nrm_prank", "Link Volume (%Rank Norm):"
                                                  ,net_sd, ~count_nrm_prank)
                        ,crosstalk::filter_slider("net_sd_per_util", "Link Utilization by Vehicle Type:"
                                                  ,net_sd, ~100*gauntlet::dgt2(percent))
                      )
                    )
                    ,leaflet::leaflet(height = 700) %>%
                      leaflet::addTiles(group = "OSM (default)") %>%
                      leaflet_default_tiles() %>%
                      leaflet::addCircleMarkers(data = net_sd
                                                ,fillColor = ~pal_centroids_od(net$count)
                                                ,color = "black"
                                                ,opacity = .8
                                                ,fillOpacity  = .5
                                                ,weight = 1
                                                ,radius = 5
                                                ,group = "Network Links (mid-points)"
                                                ,label = net$label %>%
                                                  purrr::map(htmltools::HTML)
                      ) %>%
                      map_elemet_poi() %>%
                      #layer control----
                    leaflet::addLayersControl(
                      baseGroups = leaflet_default_tiles_index()
                      ,overlayGroups =
                        c("Network Links (mid-points)"
                          ,"POI Polygons", "Non-POI Study Area Polygons")
                      ,options = layersControlOptions(collapsed = F, sortLayers = F)) %>%
                      hideGroup(c("Network Links (mid-points)", "Non-POI Study Area Polygons")) %>%
                      leafem::addMouseCoordinates() %>%
                      ##legends----
                    leaflet::addLegend(
                      position = "bottomleft"
                      ,title = "Link Volume (counts)"
                      ,group = "Network Links (mid-points)"
                      ,pal = pal_centroids_od
                      ,opacity = 0.7
                      ,values = net$count)

  )
  }
}

#' Create interactive HTML widget displaying network link volumes - ANLT (Agg. Netowrk Links by veh. Type)
#'
#' @description This function creates an interactive HTML widget which displays network volumes aggregated by vehicle type.
#' This HTML contains filtering and mapping features that can be used to investigate the data.
#'
#' @param network_cntrd_object list object containing aggregated link centroid networks.
#' @param poi_list data frame object detailing id and group attributes for polygons that network links will be displayed for. These polygons are considered Points-of-Interests and should be within the study area polygon.
#' @param origin_polys processed acquired_sa_polys object created for this study.
#'
#' @return an HTML widget map of network volumes
#' @export
#'
#' @examples
#'
#' data("acquired_sa_polys")
#' data("poi_list")
#' data("network_centroid_aggregation_list")
#'
#' make_network_map_anlt(
#'   network_cntrd_object = network_centroid_aggregation_list
#'   ,poi_list = poi_list
#'   ,origin_polys = acquired_sa_polys
#' )
make_network_map_anlt = function(network_cntrd_object
                                 ,poi_list = NULL
                                 ,origin_polys = NULL){
  {
    # data("network_centroid_aggregation_list")
    # data("poi_list")
    # data("acquired_sa_polys")
    # origin_polys = acquired_sa_polys
  }

  stopifnot("Please supply acquired_sa_polys object, it is included in this map..." = !is.null(origin_polys))
  stopifnot("Please provide Point-of-Interest list of origin polygons...\nIt is required to limit the number data points that are mapped and focuses the analysis" = !is.null(poi_list))
  stopifnot("POI list column names must be 'id' and 'group' (lowercase), please change..." = (sum(names(poi_list) %in% c("id", "group")) == 2))

  #create poly layer background
  {
    map_poly = origin_polys %>%
      merge(poi_list, by.x = "GEOID10", by.y = "id", all = T)

    query_poly_poi = map_poly %>%
      filter(!is.na(group))

    query_poly_nonpoi = map_poly %>%
      filter(is.na(group))

    map_elemet_poi = function(base_map){
      base_map %>%
        leaflet::addPolygons(data = query_poly_poi
                             ,fillColor = "blue"
                             ,fillOpacity = .4
                             ,color = "black", opacity = .5,weight = 2
                             ,group = "POI Polygons"
                             # ,label = query_poly_poi$label %>%
                             #   map(htmltools::HTML)
        ) %>%
        leaflet::addPolygons(data = query_poly_nonpoi
                             ,fillColor = "green", fillOpacity = .1
                             ,opacity = .4, weight = 1, color = "grey"
                             ,group = "Non-POI Study Area Polygons"
        )
    }
  }

  #process data
  {
    net = network_cntrd_object$agg_link_vehicle_type %>%
      na.omit()

    net_sd = crosstalk::SharedData$new(net)

    pal_centroids_od = leaflet::colorNumeric(
      palette = "magma"
      ,net$count
      ,reverse = T)
  }

  #make map
  {
    crosstalk::bscols(widths = c(3, 9)
                      ,list(
                        crosstalk::bscols(
                          widths = c(12)
                          ,htmltools::hr()
                          ,htmltools::HTML("Rdwy & Veh. Type Filters") %>%  htmltools::strong()
                          ,htmltools::hr()
                          ,crosstalk::filter_checkbox("net_sd_vehicle_type", "Choose Vehicle Type:"
                                                      ,net_sd, ~vehicle_type, inline = T)
                          ,crosstalk::filter_checkbox("net_flag_highway", "Choose Rdwy Flag:"
                                                      ,net_sd, ~flag_highway, inline = T)
                          ,crosstalk::filter_checkbox("net_highway", "Choose Rdwy Type:"
                                                      ,net_sd, ~highway, inline = T)
                          ,crosstalk::filter_select("net_streetname", "Choose Specific Rdwy:"
                                                    ,net_sd, ~streetName)
                          ,htmltools::hr()
                          ,htmltools::HTML("Network Link Volume Filters") %>%  htmltools::strong()
                          ,htmltools::hr()
                          ,crosstalk::filter_slider("net_sd_count", "Link Volume (counts):"
                                                    ,net_sd, ~count)
                          ,crosstalk::filter_slider("net_count_nrm_mmax", "Link Volume (Min-Max Norm):"
                                                    ,net_sd, ~count_nrm_mmax)
                          ,crosstalk::filter_slider("net_count_nrm_prank", "Link Volume (%Rank Norm):"
                                                    ,net_sd, ~count_nrm_prank)
                          ,crosstalk::filter_slider("net_sd_per_util", "Link Utilization by Vehicle Type:"
                                                    ,net_sd, ~100*gauntlet::dgt2(percent))
                        )
                      )
                      ,leaflet::leaflet(height = 700) %>%
                        leaflet::addTiles(group = "OSM (default)") %>%
                        leaflet_default_tiles() %>%
                        leaflet::addCircleMarkers(data = net_sd
                                                  ,fillColor = ~pal_centroids_od(net$count)
                                                  ,color = "black"
                                                  ,opacity = .8
                                                  ,fillOpacity  = .5
                                                  ,weight = 1
                                                  ,radius = 5
                                                  ,group = "Network Links (mid-points)"
                                                  ,label = net$label %>%
                                                    purrr::map(htmltools::HTML)
                        ) %>%
                        map_elemet_poi() %>%
                        #layer control----
                      leaflet::addLayersControl(
                        baseGroups = leaflet_default_tiles_index()
                        ,overlayGroups =
                          c("Network Links (mid-points)"
                            ,"POI Polygons", "Non-POI Study Area Polygons")
                        ,options = layersControlOptions(collapsed = F, sortLayers = F)) %>%
                        hideGroup(c("Network Links (mid-points)", "Non-POI Study Area Polygons")) %>%
                        leafem::addMouseCoordinates() %>%
                        ##legends----
                      leaflet::addLegend(
                        position = "bottomleft"
                        ,title = "Link Volume (counts)"
                        ,group = "Network Links (mid-points)"
                        ,pal = pal_centroids_od
                        ,opacity = 0.7
                        ,values = net$count)

    )
  }

}

#' Create interactive HTML widget displaying network link volumes - ANLT (Agg. Netowrk Links by veh. Type)
#'
#' @description This function creates an interactive HTML widget which displays network volumes aggregated by vehicle type.
#' This HTML contains filtering and mapping features that can be used to investigate the data.
#'
#' @param network_cntrd_object list object containing aggregated link centroid networks.
#' @param poi_list data frame object detailing id and group attributes for polygons that network links will be displayed for. These polygons are considered Points-of-Interests and should be within the study area polygon.
#' @param origin_polys processed acquired_sa_polys object created for this study.
#'
#' @return an HTML widget map of network volumes
#' @export
#'
#' @examples
#'
#' data("acquired_sa_polys")
#' data("poi_list")
#' data("network_centroid_aggregation_list")
#'
#' make_network_map_anlt(
#'   network_cntrd_object = network_centroid_aggregation_list
#'   ,poi_list = poi_list
#'   ,origin_polys = acquired_sa_polys
#' )
make_network_map_anltpt = function(network_cntrd_object
                                 ,poi_list = NULL
                                 ,origin_polys = NULL){
  {
    # data("network_centroid_aggregation_list")
    # data("poi_list")
    # data("acquired_sa_polys")
    # origin_polys = acquired_sa_polys
  }

  stopifnot("Please supply acquired_sa_polys object, it is included in this map..." = !is.null(origin_polys))
  stopifnot("Please provide Point-of-Interest list of origin polygons...\nIt is required to limit the number data points that are mapped and focuses the analysis" = !is.null(poi_list))
  stopifnot("POI list column names must be 'id' and 'group' (lowercase), please change..." = (sum(names(poi_list) %in% c("id", "group")) == 2))

  #create poly layer background
  {
    map_poly = origin_polys %>%
      merge(poi_list, by.x = "GEOID10", by.y = "id", all = T)

    query_poly_poi = map_poly %>%
      filter(!is.na(group))

    query_poly_nonpoi = map_poly %>%
      filter(is.na(group))

    map_elemet_poi = function(base_map){
      base_map %>%
        leaflet::addPolygons(data = query_poly_poi
                             ,fillColor = "blue"
                             ,fillOpacity = .4
                             ,color = "black", opacity = .5,weight = 2
                             ,group = "POI Polygons"
                             # ,label = query_poly_poi$label %>%
                             #   map(htmltools::HTML)
        ) %>%
        leaflet::addPolygons(data = query_poly_nonpoi
                             ,fillColor = "green", fillOpacity = .1
                             ,opacity = .4, weight = 1, color = "grey"
                             ,group = "Non-POI Study Area Polygons"
        )
    }
  }

  #process data
  {
    net = network_cntrd_object$agg_link_flag %>%
      na.omit()

    net_sd = crosstalk::SharedData$new(net)

    pal_centroids_od = leaflet::colorNumeric(
      palette = "magma"
      ,net$count
      ,reverse = T)
  }

  #make map
  {
    crosstalk::bscols(widths = c(3, 9)
                      ,list(
                        crosstalk::bscols(
                          widths = c(12)
                          ,htmltools::hr()
                          ,htmltools::HTML("Rdwy & Veh. Type Filters") %>%  htmltools::strong()
                          ,htmltools::hr()
                          ,crosstalk::filter_checkbox("net_flag_trip_type", "Choose Trip Type:"
                                                      ,net_sd, ~flag_trip_type, inline = T)
                          ,crosstalk::filter_checkbox("net_flag_highway", "Choose Rdwy Flag:"
                                                      ,net_sd, ~flag_highway, inline = T)
                          ,crosstalk::filter_checkbox("net_highway", "Choose Rdwy Type:"
                                                      ,net_sd, ~highway, inline = T)
                          ,crosstalk::filter_select("net_streetname", "Choose Specific Rdwy:"
                                                    ,net_sd, ~streetName)
                          ,htmltools::hr()
                          ,htmltools::HTML("Network Link Volume Filters") %>%  htmltools::strong()
                          ,htmltools::hr()
                          ,crosstalk::filter_slider("net_sd_count", "Link Volume (counts):"
                                                    ,net_sd, ~count)
                          ,crosstalk::filter_slider("net_count_nrm_mmax", "Link Volume (Min-Max Norm):"
                                                    ,net_sd, ~count_nrm_mmax)
                          ,crosstalk::filter_slider("net_count_nrm_prank", "Link Volume (%Rank Norm):"
                                                    ,net_sd, ~count_nrm_prank)
                          ,crosstalk::filter_slider("net_sd_per_util", "Link Utilization by Vehicle Type:"
                                                    ,net_sd, ~100*gauntlet::dgt2(percent))
                        )
                      )
                      ,leaflet::leaflet(height = 700) %>%
                        leaflet::addTiles(group = "OSM (default)") %>%
                        leaflet_default_tiles() %>%
                        leaflet::addCircleMarkers(data = net_sd
                                                  ,fillColor = ~pal_centroids_od(net$count)
                                                  ,color = "black"
                                                  ,opacity = .8
                                                  ,fillOpacity  = .5
                                                  ,weight = 1
                                                  ,radius = 5
                                                  ,group = "Network Links (mid-points)"
                                                  ,label = net$label %>%
                                                    purrr::map(htmltools::HTML)
                        ) %>%
                        map_elemet_poi() %>%
                        #layer control----
                      leaflet::addLayersControl(
                        baseGroups = leaflet_default_tiles_index()
                        ,overlayGroups =
                          c("Network Links (mid-points)"
                            ,"POI Polygons", "Non-POI Study Area Polygons")
                        ,options = layersControlOptions(collapsed = F, sortLayers = F)) %>%
                        hideGroup(c("Network Links (mid-points)", "Non-POI Study Area Polygons")) %>%
                        leafem::addMouseCoordinates() %>%
                        ##legends----
                      leaflet::addLegend(
                        position = "bottomleft"
                        ,title = "Link Volume (counts)"
                        ,group = "Network Links (mid-points)"
                        ,pal = pal_centroids_od
                        ,opacity = 0.7
                        ,values = net$count)

    )
  }

}


#' Make interactive network link volume maps using MapView - maps ANLT network links (Aggregated Network Links by vehicle Type).
#'
#' @description This is a helper function that quickly makes an interactive map of aggregated network link volumes.
#' All functions in the viz_static_ntwrk_map_% function family makes a singular leaflet/mapview object.
#'
#' This is another mapping option if you wish to map network links and view link volumes without making an interactive, HTML elements.
#'
#' @param spatial_agg_object spatial links objects of aggregated network. Has to be links, centroids will print but aesthetic options in Mapview code are specific to links.
#'
#' @return a leaflet object made using MapView API depicting links
#' @export
#'
#' @examples
#'
#' data_temp = aggregate_network_links(
#' agg_count_object = table_agg_by_link_subset_limited
#' ,network_object = replica_queried_network_links
#' )
#'
#' viz_static_ntwrk_map_anlt(
#'   spatial_agg_object = data_temp
#'   )
viz_static_ntwrk_map_anlt = function(spatial_agg_object){
  data = spatial_agg_object[["agg_link_vehicle_type"]] %>%
    mutate(lwd_rescale = rescale_to(count, 10))

  map_object = list(
    unique(data$vehicle_type)
    ,viridisLite::viridis(length(unique(data$vehicle_type)))
  )%>%
    pmap(function(x, y){
      temp = data %>%
        filter(vehicle_type == x)

      temp %>%
        mapview(label = "label"
                ,color = y
                ,layer.name = x
                ,lwd = 'lwd_rescale'
                ,popup = popup_tbl_pretty(temp %>%  select(-c(label, lwd_rescale)))
                ,homebutton = F)
    }) %>%
    reduce(`+`)

  map_object@map %>%
    leaflet::hideGroup(unique(data$vehicle_type))
}

#' Make interactive network link volume maps using MapView - maps ANLT network links (Aggregated Network Links by vehicle Type and Origin poly).
#'
#' @description This is a helper function that quickly makes an interactive map of aggregated network link volumes.
#' All functions in the viz_static_ntwrk_map_% function family makes a singular leaflet/mapview object.
#'
#' This is another mapping option if you wish to map network links and view link volumes without making an interactive, HTML elements.
#'
#' @param spatial_agg_object spatial links objects of aggregated network. Has to be links, centroids will print but aesthetic options in Mapview code are specific to links.
#'
#' @return a leaflet object made using MapView API depicting links
#' @export
#'
#' @examples
#'
#' data_temp = aggregate_network_links(
#' agg_count_object = table_agg_by_link_subset_limited
#' ,network_object = replica_queried_network_links
#' )
#'
#' viz_static_ntwrk_map_anlto_grp(
#'   spatial_agg_object = data_temp
#'   )
viz_static_ntwrk_map_anlto_grp = function(spatial_agg_object){
  data = spatial_agg_object[["agg_link_vehicle_type_origin"]]  %>%
    na.omit() %>%
    filter(origin_poly %in% as.character(poi_list$id)) %>%
    merge(poi_list %>%
            mutate(id = as.character(id)), by.x = "origin_poly", by.y = "id", all.x = T) %>%
    mutate(lwd_rescale = rescale_to(count, 10))

  cross_items = cross(
    list(unique(data$group)
         ,unique(data$vehicle_type)))

  map_object = list(cross_items
                    ,viridisLite::viridis(length(cross_items))) %>%
    pmap(function(x, y){

      temp = data %>%
        filter(
          group == x[[1]]
          ,vehicle_type == x[[2]]
        )

      if (nrow(temp)>0) {
        temp %>%
          mapview(label = "label"
                  ,color = y
                  ,layer.name = paste0(x[[2]], "_", x[[1]])
                  ,lwd = 'lwd_rescale'
                  ,popup = popup_tbl_pretty(temp %>%  select(-c(label, lwd_rescale)))
                  ,homebutton = F)
      }
    }) %>%
    reduce(`+`)

  map_object@map %>%
    leaflet::hideGroup(
      map_chr(cross_items, ~paste0(.x[[2]], "_", .x[[1]]))
    )

  return(map_object)
}

#' Make interactive network link volume maps using MapView - maps ANLT network links (Aggregated Network Links by vehicle Type and Origin poly).
#'
#' @description This is a helper function that quickly makes an interactive map of aggregated network link volumes.
#' All functions in the viz_static_ntwrk_map_% function family makes a singular leaflet/mapview object.
#'
#' This is another mapping option if you wish to map network links and view link volumes without making an interactive, HTML elements.
#'
#' @param spatial_agg_object spatial links objects of aggregated network. Has to be links, centroids will print but aesthetic options in Mapview code are specific to links.
#'
#' @return a leaflet object made using MapView API depicting links
#' @export
#'
#' @examples
#'
#' data_temp = aggregate_network_links(
#' agg_count_object = table_agg_by_link_subset_limited
#' ,network_object = replica_queried_network_links
#' )
#'
#' viz_static_ntwrk_map_anlto(
#'   spatial_agg_object = data_temp
#'   )
viz_static_ntwrk_map_anlto = function(spatial_agg_object){
  data = spatial_agg_object[["agg_link_vehicle_type_origin"]]  %>%
    na.omit() %>%
    filter(origin_poly %in% as.character(poi_list$id)) %>%
    merge(poi_list %>%
            mutate(id = as.character(id)), by.x = "origin_poly", by.y = "id", all.x = T) %>%
    mutate(lwd_rescale = rescale_to(count, 10))

  cross_items = cross(
    list(unique(data$origin_poly)
         ,unique(data$vehicle_type)))

  map_object = list(cross_items
                    ,viridisLite::viridis(length(cross_items))) %>%
    pmap(function(x, y){

      temp = data %>%
        filter(
          origin_poly == x[[1]]
          ,vehicle_type == x[[2]]
        )

      if (nrow(temp)>0) {
        temp %>%
          mapview(label = "label"
                  ,color = y
                  ,layer.name = paste0(x[[2]], "_", x[[1]])
                  ,lwd = 'lwd_rescale'
                  ,popup = popup_tbl_pretty(temp %>%  select(-c(label, lwd_rescale)))
                  ,homebutton = F)
      }
    }) %>%
    reduce(`+`)

  map_object@map %>%
    leaflet::hideGroup(
      map_chr(cross_items, ~paste0(.x[[2]], "_", .x[[1]]))
    )

}

#' Make interactive network link volume maps using MapView - maps ANLTPT network links (Aggregated Network Links by TriP Type ).
#'
#' @description This is a helper function that quickly makes an interactive map of aggregated network link volumes.
#' All functions in the viz_static_ntwrk_map_% function family makes a singular leaflet/mapview object.
#'
#' This is another mapping option if you wish to map network links and view link volumes without making an interactive, HTML elements.
#'
#' @param spatial_agg_object spatial links objects of aggregated network. Has to be links, centroids will print but aesthetic options in Mapview code are specific to links.
#'
#' @return a leaflet object made using MapView API depicting links
#' @export
#'
#' @examples
#'
#' data_temp = aggregate_network_links(
#' agg_count_object = table_agg_by_link_subset_limited
#' ,network_object = replica_queried_network_links
#' )
#'
#' viz_static_ntwrk_map_anltpt(
#'   spatial_agg_object = data_temp
#'   )
viz_static_ntwrk_map_anltpt = function(spatial_agg_object){
  data = spatial_agg_object[["agg_link_flag"]] %>%
    mutate(lwd_rescale = rescale_to(count, 10))

  map_object = list(
    unique(data$flag_trip_type)
    ,viridisLite::viridis(length(unique(data$flag_trip_type)))
  )%>%
    pmap(function(x, y){
      temp = data %>%
        filter(flag_trip_type == x)

      temp %>%
        mapview(label = "label"
                ,color = y
                ,layer.name = x
                ,lwd = 'lwd_rescale'
                ,popup = popup_tbl_pretty(temp %>%  select(-c(label, lwd_rescale)))
                ,homebutton = F)
    }) %>%
    reduce(`+`)

  map_object@map %>%
    leaflet::hideGroup(unique(data$flag_trip_type))

}


