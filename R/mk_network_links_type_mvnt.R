
mk_network_links_type_mvnt = function(
    location, folder, auto_save = F
    ,agg_count_object = NULL
    ,network_object = NULL
    ,file_prefix = "agg_links_type_mvmnt"){
  #TODO:make this compatible with other polygon types
  #TODO:review different aggregations - they don't make the most sense


  if (is.null(agg_count_object)){
    message("Aggregations will be made using file and location...")
    network_links = here::here(location, folder, "table_agg_by_link_subset_limited.csv") %>%
      data.table::fread() %>%
      mutate(flag_trip_type = str_glue("{flag_sa_origin}-{flag_sa_destination}")
             ,network_link_ids_unnested = str_trunc(network_link_ids_unnested, 14, "right", ""))
  } else {
    message("Aggregations will be made using supplied network object...")
    network_links = agg_count_object %>%
      mutate(flag_trip_type = str_glue("{flag_sa_origin}-{flag_sa_destination}")
             ,network_link_ids_unnested = str_trunc(network_link_ids_unnested, 14, "right", ""))
  }

  stopifnot("You need to supply an object to merge tabular data with, either network centroids or polylines...." = !is.null(network_object))
  stopifnot("Supplied network object had more than one geometry feature in it, please review and fix...." = (length(unique(st_geometry_type(network_object))) == 1))
  stopifnot("Supplied network object must be either POINT or LINESTRING, please review and fix...." = (unique(st_geometry_type(network_object))[[1]] %in% c("POINT", "LINESTRING")))

  #pre-process data
  {
    if (unique(st_geometry_type(network_object))[[1]] == "POINT"){
      index_extract = "POINT"
    } else {
      index_extract = "LINESTRING"
    }
  }


  #perform aggregations
  {
    message(stringr::str_glue("{gauntlet::strg_make_space_2()}Starting aggreagtion by link and study area destination flag...."))


    #ANLT - agg network by link and vehcile type
    {
      message(str_glue("{gauntlet::strg_make_space_2()}Starting aggreagtion by link and vehicle type...."))

      temp_network = network_links %>%
        group_by(network_link_ids_unnested, flag_sa_origin, vehicle_type ) %>%
        summarise(count = sum(count)) %>%
        group_by(network_link_ids_unnested, flag_sa_origin) %>%
        mutate(pct = round(count/sum(count), 2)) %>%
        ungroup() %>%
        group_by(network_link_ids_unnested) %>%
        mutate(ttl_count_link = sum(count)) %>%
        ungroup() %>%
        mutate(count_nrm_prank_ttl = gauntlet::dgt2(percent_rank(ttl_count_link))
               ,count_nrm_mmax_ttl = gauntlet::dgt2(normalize_min_max(ttl_count_link))) %>%
        group_by(vehicle_type) %>%
        mutate(count_nrm_prank = dgt2(percent_rank(count))
               ,count_nrm_mmax = dgt2(normalize_min_max(count)))

      temp_network_sf = merge(
        network_object, temp_network
        ,by.x = "stableEdgeId", by.y = "network_link_ids_unnested", all = T)

      temp_network_sf_pro = temp_network_sf %>%
        filter(!is.na(vehicle_type))
        # mutate(label = str_glue(
        #   "Link Name: {streetName} ({highway})
        #   <br>TTl Link Volume: {ttl_count_link} - {100*count_nrm_mmax_ttl}% (Min-Max norm.)
        #   <hr>
        #   Metrics Adj for Veh. Type: {vehicle_type}
        #   <br>Link Volume: {count}
        #   <br>Link Volume (Min-Max norm.): {100*count_nrm_mmax}%"))

      message("Aggregation complete....")
    }


  #save out
    temp_folder = here::here(location, folder, "network_output")
    dir.create(temp_folder)
    temp_file = here::here(temp_folder, "agg_links_type_mvmnt")

    if (auto_save) {
      message("You elected to automatically save the returned list object!")
      file = here::here(location, folder, "aggregated_network_links.rds")
      message(str_glue("It is saved at this location:\n{temp_file}.//EXT//"))

      data.table::fwrite(temp_network, paste0(temp_file, ".csv"))
      sf::write_sf(temp_network_sf_pro, paste0(temp_file, ".shp"))
      qs::qsave(temp_network_sf_pro, paste0(temp_file, ".qs"))

    } else {
      message("You did not elect to automatically save the returned list object!")
    }
  }

  return(temp_network_sf_pro)

}
