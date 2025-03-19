make_speed_profiles_2 = function(sf_object, crs, xwalk_ob, vp_data, samp_dist = 100, over_ride = NA) {
  message("Starting make_speed_profiles...")

  if (!is.na(over_ride)) {
    message("over_ride input detected.... reducing routes to user supplied override")
    xwalk_ob = xwalk_override(xwalk_ob, over_ride)
  }

  message(str_glue("{gauntlet::strg_make_space_2()}Discretizing route shapeid(s)"))
  sf_object_points = sf_object %>%
    dplyr::filter(shape_id %in% unique(xwalk_ob$shape_id)) %>%
    dplyr::group_by(shape_id_1 = shape_id) %>%
    dplyr::group_map(~{
      message("Processing: ", unique(.x$shape_id))
      tmp_disc = st_line_sample_to_points_2(sf_object = .x, crs = crs, samp_dist = samp_dist)
      message("Number of segements created: ", max(tmp_disc$index))
      return(tmp_disc)
    }) %>%
    purrr::reduce(dplyr::bind_rows) %>%
    tibble::remove_rownames()


  message(str_glue("{gauntlet::strg_make_space_2()}Snapping vehicle positions to route segements"))
  temp_speed_profiles = xwalk_ob$shape_id %>%
    unique() %>%
    purrr::map_df(~{
      message("Processing shape ID: ", .x)

      index_xwalk = xwalk_ob %>%
        dplyr::filter(shape_id %in% .x) %>%
        dplyr::pull(trip_id)

      message("Number of trips: ", length(unique(index_xwalk)))

      temp_vp_data = vp_data %>%
        dplyr::filter(trip_id %in% index_xwalk)

      temp_sf_object_points = sf_object_points %>%
        dplyr::filter(shape_id %in% .x)

      # message("Number of rows in sf_object_points: ", nrow(temp_sf_object_points))
      # message("Max index in sf_object_points: ", max(temp_sf_object_points$index))

      temp_joined <- sf::st_join(x = temp_vp_data,
                                 y = temp_sf_object_points,
                                 join = sf::st_nearest_feature) %>%
        sf::st_drop_geometry()

      # Additional processing steps

    })

  message("make_speed_profiles completed.")

  return(list(route_samples = sf_object_points,
              speed_profiles = temp_speed_profiles))
}
