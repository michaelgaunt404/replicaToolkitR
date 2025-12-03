
#I THINK GOOD BUT USED IN REALTIME-GTFS
#nothing in hear looks like it pertains replicatoolkit stuff since we never get speed info

convert_vp_data_to_sf_2 = function (data, crs_to) {
  # message("Converting VP Data to Spatial Format...")
  #
  # converted_data <- data %>%
  #   sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  #   sf::st_transform(crs = crs_to) %>%
  #   gauntletMap::st_extract_coords() %>%
  #   arrange(route_id, vehicle_id, trip_id, direction_id, timestamp ) %>%
  #   group_by(route_id, vehicle_id, trip_id, direction_id) %>%
  #     mutate(datetime_diff = timestamp-lag(timestamp)) %>%
  #   # ungroup() %>% glimpse()
  #   mutate(
  #     lon_diff = lon - lag(lon),
  #     lat_diff = lat - lag(lat),
  #     ttl_diff = sqrt(lon_diff^2 + lat_diff^2),
  #     speed_avg = (ttl_diff / datetime_diff) * 2.236936
  #   ) %>%
  #   ungroup() %>%
  #   filter(speed_avg < 90) %>%
  #   group_by(route_id, vehicle_id, trip_id, direction_id) %>%
  #   mutate(speed_avg_diff = speed_avg - lag(speed_avg)) %>%
  #   ungroup() %>%
  #   sf::st_transform(4326)
  #
  # message("VP Data converted to Spatial Format.")

  return(converted_data)
}
