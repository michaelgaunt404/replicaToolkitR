rplc_layer_extent_loadUnionWkt_2 = function(layer, layer_name) {
  message(stringr::str_glue("{make_space()}\nStarting extent layer processing...."))

  if (is.character(layer)) {
    message(stringr::str_glue("String detected for {layer_name}\nReading file now...."))
    temp_object <- layer %>%
      here::here() %>%
      sf::read_sf()
    message("Done")
  } else {
    temp_object <- layer
  }

  message(str_glue("Converting {layer_name} to WKT format\nExtent will be unioned to prevent sending multi-polygon..."))

  if (!(st_crs(temp_object)$input %in% c("EPSG:4326", "WGS 84"))) {
    message("CRS for the bounding layer is not set to EPSG:4326; converting...")
    temp_object <- sf::st_transform(temp_object, crs = 4326)
  }

  temp_wkt <- sf::st_as_text(sf::st_union(temp_object))

  message("Done")

  return(temp_wkt)
}
