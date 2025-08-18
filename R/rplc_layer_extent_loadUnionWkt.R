#' Convert Layer to Well-Known Text (WKT) Format
#'
#' This function converts a given layer, which can be a file location (character) or a spatial data frame, to Well-Known Text (WKT) format.
#' It also ensures that the layer's Coordinate Reference System (CRS) is set to EPSG:4326.
#'
#' @param layer The layer to be converted to WKT format. It can be either a character (file location) or a spatial data frame.
#' @param layer_name A descriptive name for the layer, used for logging messages.
#'
#' @return The layer converted to WKT format with a CRS of EPSG:4326.
#'
#' @examples
#' # Using a file location
#' layer <- "path/to/your_layer.shp"  # Replace with the actual file location
#' layer_name <- "Network Layer"
#' converted_layer <- convert_to_wkt(layer, layer_name)
#'
#' # Using a spatial data frame
#' layer <- your_spatial_data_frame
#' layer_name <- "Study Area Layer"
#' converted_layer <- convert_to_wkt(layer, layer_name)
#'
#' @export
rplc_layer_extent_loadUnionWkt = function(layer, layer_name) {
  message(stringr::str_glue("{gauntlet::strg_make_space_2()}Starting extent layer processing...."))

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

  # temp_wkt <- wellknown::sf_convert(sf::st_union(temp_object))
  temp_wkt <- st_as_text(sf::st_union(temp_object))

  message("Done")

  return(temp_wkt)
}
