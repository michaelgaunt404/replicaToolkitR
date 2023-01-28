check_links_download = function(location, folder){
  # location = "data/req_dev"
  # folder = "data_20230112_143236"

  replica_trip_agg_by_link_subset = here::here(location, folder, "replica_trip_agg_by_link_subset.csv") %>%
    data.table::fread()

  check_subset_agg = length(unique(temp$network_link_ids_unnested))

  replica_queried_network = here::here(location, folder, "replica_queried_network.csv") %>%
    data.table::fread()

  check_subset = length(unique(replica_queried_network$stableEdgeId))

  str_glue("The replica network link aggregate table returned {check_subset_agg} unique network links\nThe network subset download returned {check_subset} unique network links{make_space('-')}\n{100*dgt3(check_subset_agg/check_subset)}% of queried links have trip counts associated with them{make_space('-')}")

}


check_origin_counts = function(location, folder){
  # location = "C:/Users/USMG687637/Documents/071_projects/replica_testing/data/req_dev"
  # folder = "data_20230112_165230"

table_simple_origin_destination = here::here(location, folder, "replica_trip_origin_destination.csv") %>%
  data.table::fread()

table_trip_first_link_pro = here::here(location, folder, "replica_trip_origin_links.csv") %>%
  data.table::fread()


od_agg = table_simple_origin_destination %>%
  group_by(vehicle_type, origin_poly) %>%
  summarise(count_from_od_table = sum(count)) %>%
  ungroup()

origin_link_agg = table_trip_first_link_pro %>%
  group_by(vehicle_type, origin_poly) %>%
  summarise(count_from_first_links = sum(count)) %>%
  ungroup()

comparision_output = od_agg %>%
  merge(.
        ,origin_link_agg
        ,by = c('vehicle_type', 'origin_poly')
  ) %>%
  mutate(flag_check_same_origin_counts = (count_from_od_table == count_from_first_links)
         ,count_diff = count_from_od_table - count_from_first_links) %>%
  group_by(vehicle_type, flag_check_same_origin_counts) %>%
  summarise(count = n()
            ,count_diff_avg = mean(count_diff)
            ,count_diff_med = median(count_diff)
            ,count_diff_sd = sd(count_diff)
            ,count_diff_max = max(count_diff)
            ,count_diff_max_pct = max(count_diff/count_from_first_links))
}



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
#' @return
#' @export
#'
#' @examples
#' data("replica_queried_network_links")
#'
#' inspect_queried_network(
#'   network_links_object = replica_queried_network_links
#' )
inspect_queried_network = function(location, folder, network_links_object = NULL){

  # location = "data/req_dev"
  # folder = "data_20230117_092037"
  # network_object = NULL
  # auto_save = F

  message(stringr::str_glue("{make_space('-')}\nW A R N I N G{make_space('-')}\nNetwork links are converted to LINK CENTROIDS and displayed in the map....\nPolylines cannot be filtered using the packages that create this feature{make_space()}"))



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
    ,reactable::reactable(network_raw %>% mutate(count = 1) %>% st_drop_geometry() %>%
                            gauntlet::count_percent_zscore(grp_c = c(highway), grp_p = c(), rnd = 2, col = count) %>%  arrange(desc(count))
                          ,filterable = T, highlight = TRUE
                          ,compact = TRUE, fullWidth = T
                          ,wrap = FALSE, resizable = TRUE
                          , height = 400
                          ,striped = TRUE)
    ,reactable::reactable(network_raw %>% mutate(count = 1) %>% st_drop_geometry() %>%
                            gauntlet::count_percent_zscore(grp_c = c(streetName), grp_p = c(), rnd = 2, col = count) %>%  arrange(desc(count))
                          ,filterable = T, highlight = TRUE
                          ,compact = TRUE, fullWidth = T
                          ,wrap = FALSE, resizable = TRUE
                          ,pagination = F, height = 400
                          ,striped = TRUE)
  )
}

