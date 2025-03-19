st_line_sample_to_points_2 = function (sf_object, samp_dist = 100, crs)
{
  sf_object_linestring = sf_object %>% mutate(merge_id = row_number()) %>%
    st_transform(crs) %>% st_cast("LINESTRING") %>% mutate(linestring_id = row_number()) %>%
    select(merge_id, linestring_id, shape_id)
  sf_object_points = sf_object_linestring %>%
    st_line_sample(density = 1/samp_dist) %>%
    st_transform(4326) %>%
    st_as_sf() %>%
    bind_cols(sf_object_linestring %>%sf::st_drop_geometry() %>%
                select(shape_id)) %>%
    st_cast("POINT") %>%
    mutate(index = row_number()) %>%
    rename(geometry = x) %>%
    gauntletMap::st_extract_coords() %>%
    rename(samp_lon = lon,
           samp_lat = lat)
}
