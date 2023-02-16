export_agg_links_to_spatial = function(data, location, folder){
  # data = aggregated_network_links

  index_names = names(data)

  full_location = here::here(location, folder, "spatial_output")

  if (!exists(full_location)){
    dir.create(full_location)
  }

  message(str_glue("Aggregated link RDS object elements will be saved individually at the following location:\n{paste0('./', location, '/', folder, '/', 'spatial_output')}"))

  index_names %>%
    purrr::map(~{
      write_sf(
        data[[.x]]
        ,here::here(location, folder, "spatial_output", stringr::str_glue("{.x}.gpkg"))
      )

    })
}


