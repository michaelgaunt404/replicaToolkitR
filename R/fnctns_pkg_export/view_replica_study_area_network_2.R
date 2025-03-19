view_replica_study_area_network_2 = function(
    customer_name
    ,links_pro = NA
    ,study_area_extent = NULL
    ,data_set_location
    ,data_set_period
){


  network_table = str_glue("{customer_name}.{data_set_location}.{data_set_location}_{data_set_period}_network_segments")

  if (is.null(study_area_extent)) {
    message(str_glue("{gauntlet::strg_make_space_2()}Please draw a study area that will be used to query Replica's roadway network..."))
    message("Draw it as small and parsimonious as possible")
    message(str_glue("You can draw multiple, discrete objects if you wish{gauntlet::strg_make_space_2(last = F)}"))

    # Use manual drawing method
    study_area = mapedit_robust_draw_2()
    study_area_wkt = sf::st_as_text(st_union(study_area))

  } else {
    message("Using the provided spatial extent for the study area.")
    study_area = study_area_extent %>% st_transform(4326)
    study_area_wkt = sf::st_as_text(st_union(study_area))

  }

  limit_links = robust_prompt_used("limit the number of links using the links supplied links_pro")

  if (limit_links) {
    if (is.na(links_pro)) {
      message("User did not supply a links_pro input....")
      continue_response <- robust_prompt_used("would you like to continue without limiting the number of links")
      if (continue_response) {
        links_response = c('highway','corridor','road'
                           ,'motorway','motorway_link'
                           ,'trunk','trunk_link'
                           ,'primary','primary_link'
                           ,'secondary','secondary_link'
                           ,'tertiary','tertiary_link') %>%
          paste0("'", ., "'", collapse = ", ")
      } else {
        stop("Execution terminated by user.")
      }
    } else {
      links_response = links_pro
    }
  } else {
    links_response = c('highway','corridor','road'
                       ,'motorway','motorway_link'
                       ,'trunk','trunk_link'
                       ,'primary','primary_link'
                       ,'secondary','secondary_link'
                       ,'tertiary','tertiary_link') %>%
      paste0("'", ., "'", collapse = ", ")
  }

  table_network = bigrquery::bq_project_query(
    customer_name,
    stringr::str_glue("select * from (
                      select *,
                      ST_INTERSECTS(
                      ST_GEOGFROMTEXT('{study_area_wkt}')
                      ,geometry) as flag_contains
                      from `{network_table}`
                      --where highway in ({links_response})
                      )
                      where flag_contains = TRUE"))

  table_network_count = bigrquery::bq_table_nrow(table_network)

  message(str_glue("{table_network_count} links were returned... \nWould you like to continue query execution or abort run completely..."))
  check_continue = robust_prompt_used("continue")
  stopifnot("Aborted" = check_continue)

  table_network_data = bigrquery::bq_table_download(table_network
                                                    ,page_size = 1000) %>%
    arrange(stableEdgeId)

  sf_network_data = table_network_data %>%
    st_as_sf(wkt = "geometry", crs = 4326)

  return(
    list(table_network = table_network
         ,table_network_data = table_network_data
         ,sf_network_data = sf_network_data)
  )
}
