
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Custom script for bespoke analysis ask
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: intended to get OD matrix for unique links
#-------- ask from Mengqing
#-------- incorporating some changes in process for dev_get_custom_poly_data.R
#-------- but also using a bunch of code from query_replica_mvmnt_patterns.R
#-------- ----that is mostly the stuff below
#
# *please use 80 character margins
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#functions======================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Function to calculate heading between two GPS coordinates in a dataframe
sf_calculate_heading_dataframe <- function(df, startLatCol, startLonCol, endLatCol, endLonCol) {
  # Convert degrees to radians
  df$lat1_rad <- df[[startLatCol]] * pi / 180
  df$lon1_rad <- df[[startLonCol]] * pi / 180
  df$lat2_rad <- df[[endLatCol]] * pi / 180
  df$lon2_rad <- df[[endLonCol]] * pi / 180

  # Calculate differences in coordinates
  df$dlat <- df$lat2_rad - df$lat1_rad
  df$dlon <- df$lon2_rad - df$lon1_rad

  # Apply Haversine formula
  df$central_angle <- acos(sin(df$lat1_rad) * sin(df$lat2_rad) + cos(df$lat1_rad) * cos(df$lat2_rad) * cos(df$dlon))
  df$bearing <- (atan2(sin(df$dlon) * cos(df$lat2_rad), cos(df$lat1_rad) * sin(df$lat2_rad) - sin(df$lat1_rad) * cos(df$lat2_rad) * cos(df$dlon))) * (180 / pi)

  # Adjust bearing to be between 0 and 360 degrees
  df$bearing <- (df$bearing + 360) %% 360

  return(df)
}

replica_test_connection = function(network_table = network_table
                                   ,trip_table = trip_table
                                   ,customer_name = customer_name){

  test_outcome = list(
    c(network_table, trip_table)
    ,c("distance", "activity_id")
  ) %>%
    pmap(~{

      outcome = tryCatch({

        table = bigrquery::bq_project_query(
          customer_name, stringr::str_glue("select {.y} from `{.x}` limit 1;"))

        table_dl = bigrquery::bq_table_download(table)

        return(nrow(table_dl))
      },  error = function(e) {
        error_message <- paste("An error occurred while querying the trip table:\n", e$message)

        return(NA)
      })

    })

  stopifnot("One or more of the Replica tables could not be connected to.... \nCheck Replica table inputs" = any(is.na(unlist(test_outcome))) == F)
}

#library set-up=================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
library(gauntlet)

pkgs = c("tibble", "tidyverse", "lubridate", "data.table"
         ,"here", "sf", "mapview", "gauntlet", "replicaToolkitR")

package_load(pkgs)

# library(replicaTookitR)

#library set-up=================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

save_location = "data/req_zz/req_wa_hwy2"
data_set_location = "northwest"
# data_set_location = "southwest"
data_set_period = "2019_Q4"
# data_set_period = "2022_Q4"
data_set_day = "thursday"
customer_name = "replica-customer"
# mode_type = c('COMMERCIAL')
mode_type = c('PRIVATE_AUTO')
# query_links = c("highway", "motorway", "corridor")
# query_links = c("highway", "corridor", "road", "motorway", "motorway_link", "trunk")
prefix_origin = "origin"
prefix_dest = "destination"
jitter_factor = 0.003

# mvmnt_df = data.frame(
#   mvmnt_desc = c("mvmnt_67th_north_full", "mvmnt_67th_north_halfbottom", "mvmnt_67th_north_halftop", "mvmnt_67th_north_entrance"
#                  ,"mvmnt_67th_south_full", "mvmnt_67th_south_halfbottom", "mvmnt_67th_south_halftop", "mvmnt_67th_south_entrance"
#   )
#   ,ttl_seq = c(1, 1, 1, 1
#                ,1, 1, 1, 1)
#   ,bearing = c("bearing>355|bearing<005", "bearing>355|bearing<005", "bearing>355|bearing<005", "bearing>355|bearing<005"
#                ,"bearing>175&bearing<185", "bearing>175&bearing<185", "bearing>175&bearing<185", "bearing>175&bearing<185"))

mvmnt_df = data.frame(
  mvmnt_desc = c("mvmnt_smkypoint_nr_full", "mvmnt_smkypoint_nr_halfbottom", "mvmnt_smkypoint_nr_halftop", "mvmnt_smkypoint_nr_entrance"
                 ,"mvmnt_smkypoint_sth_full", "mvmnt_smkypoint_sth_halfbottom", "mvmnt_smkypoint_sth_halftop", "mvmnt_smkypoint_sth_entrance"
  )
  ,ttl_seq = c(1, 1, 1, 1
               ,1, 1, 1, 1)
  ,bearing = c("bearing>340|bearing<020", "bearing>340|bearing<020", "bearing>340|bearing<020", "bearing>340|bearing<020"
               ,"bearing>160&bearing<195", "bearing>160&bearing<195", "bearing>160&bearing<195", "bearing>160&bearing<195"))

# mvmnt_df = data.frame(
#   mvmnt_desc = c("mvmnt_51th_north_full", "mvmnt_51th_north_halfbottom", "mvmnt_51th_north_halftop", "mvmnt_51th_north_entrance"
#                  ,"mvmnt_51th_south_full", "mvmnt_51th_south_halfbottom", "mvmnt_51th_south_halftop", "mvmnt_51th_south_entrance"
#   )
#   ,ttl_seq = c(1, 1, 1, 1
#                ,1, 1, 1, 1)
#   ,bearing = c("bearing>355|bearing<005", "bearing>355|bearing<005", "bearing>355|bearing<005", "bearing>355|bearing<005"
#                ,"bearing>175&bearing<185", "bearing>175&bearing<185", "bearing>175&bearing<185", "bearing>175&bearing<185"))

# mvmnt_df = data.frame(
#   mvmnt_desc = c("mvmnt_I35_north_full", "mvmnt_I35_south_full"
#                  ,"mvmnt_TX130_north_full", "mvmnt_TX130_south_full"
#   )
#   ,ttl_seq = c(1, 1, 1, 1)
#   ,bearing = c("bearing>-5", "bearing>-5", "bearing>-5", "bearing>-5"))



network_table = str_glue("{customer_name}.{data_set_location}.{data_set_location}_{data_set_period}_network_segments")
trip_table = str_glue("{customer_name}.{data_set_location}.{data_set_location}_{data_set_period}_{data_set_day}_trip")



query_replica_mvmnt_patterns <- function(network_table, trip_table
                                         ,mode_type, customer_name, jitter_factor
                                         ,mvmnt_df
                                         ,save_location) {
  mapviewOptions(homebutton = F)
  message("Starting query to get movement pattern data from Replica")

  mode_type_pro = paste0("'", mode_type, "'", collapse = ", ")

  links_pro = paste0("'", query_links, "'", collapse = ", ")

  replica_test_connection(network_table, trip_table, customer_name)

  {
    # network_table = "replica-customer.northwest.northwest_2021_Q4_network_segments"
    # trip_table = "replica-customer.northwest.northwest_2021_Q4_thursday_trip"
    # mode_type = c('PRIVATE_AUTO')
    # customer_name = "replica-customer"
    # jitter_factor = 0.001
    # save_location = "data/req_zz"

  }

  # {
  #   check_dir_path(save_location)
  #   directory_path = make_dir_prfx_date(save_location, "mvmnt_data_")
  #   logger = log4r::logger("DEBUG"
  #                          ,appenders = log4r::file_appender(
  #                            here::here(directory_path, "log_file.txt")))
  #   gauntlet::log_and_info("Directories and log file created", logger)
  #   gauntlet::log_and_info(
  #     list(
  #       network_table = network_table
  #       ,trip_table = trip_table
  #       ,mode_type = mode_type
  #       ,customer_name = customer_name
  #       ,jitter_factor = jitter_factor
  #       ,user_provided_save_location = save_location
  #       ,directory_path = directory_path) %>%
  #       print_named_list() %>%
  #       paste0("\n", gauntlet::strg_make_space_2(), "Recording user inputs....\n", ., gauntlet::strg_make_space_2(last = T))
  #     ,logger
  #   )
  # }

  index_network_name_strip = c("Drive", "Boulevard", 'Broadway', 'Center', 'Circle', 'Lane', 'Loop'
                               ,'Park', 'Parkway', 'Place', 'Route', 'Road', 'Square', 'Street', 'View', 'Way') %>%
    paste0(collapse = "|")

  table_network_data = view_replica_study_area_network(
    network_table = network_table
    ,customer_name = customer_name
    ,links_pro = links_pro)

  info(logger, "User elected to acquire links... successful")
  info(logger, str_glue("table_network: {replica_temp_tbl_name(table_network_data$table_network)}"))

  table_network_data_sf =  table_network_data[[2]] %>%
    st_as_sf(wkt = "geometry", crs = 4326) %>%
    sf_calculate_heading_dataframe("startLat", "startLon", "endLat", "endLon")

  table_network_data_simp =  table_network_data_sf %>%
    st_drop_geometry() %>%
    select(stableEdgeId, streetName) %>%
    mutate(streetName = str_remove(streetName, index_network_name_strip) %>%
             str_trim())

  check_save_net = robust_prompt_used("to save the network at this point")
  if (check_save_net){
    write_sf(table_network_data_sf, here::here(directory_path, "network_sf.gpkg"))
    message("Network saved..")
  } else {
    message("Table network has been saved to log file\nif you want to download the network at a later point in time..")
  }

  message(str_glue("{gauntlet::strg_make_space_2()}Starting link selection process..."))
  log4r::info(logger, "started link selection")
  message(str_glue("The network you queried will be displayed with a {jitter_factor} jitter factor..."))

  table_network_data_sf_jit = table_network_data_sf %>%
    filter(!is.na(streetName)) %>% #this needs to be a selection
    st_jitter(factor = jitter_factor)

  mapview(table_network_data_sf_jit, zcol = "flags", burst = T)

  rejitter = TRUE
  while (rejitter) {
    rejitter = robust_prompt_used("change the jitter factor and reijitter")

    if (rejitter) {
      jitter_factor = prompt_jitter_factor()

      table_network_data_sf_jit = table_network_data_sf %>%
        st_jitter(factor = jitter_factor)

      mapview(table_network_data_sf_jit, zcol = "flags", burst = T) %>%  print()
    }
  }


  # rejitter = TRUE
  # while (rejitter) {
  #   rejitter = robust_prompt_used("change the jitter factor and reijitter")
  #
  #   if (rejitter) {
  #     jitter_factor = prompt_jitter_factor()
  #
  #     jitter_factor = 0.001
  #
  #     table_network_data_sf_jit = table_network_data_sf %>%
  #       st_jitter(factor = jitter_factor)
  #
  #     tmp = table_network_data_sf_jit %>%
  #       filter(!is.na(streetName)) %>%
  #       calculate_heading_dataframe() %>%
  #       mutate(
  #         bearing = dgt0(bearing)
  #         ,bearing_cut = cut(bearing, seq(0, 360, 10))
  #       )
  #
  #     tmp_filter = tmp %>%
  #       filter(bearing<5 | bearing >355) %>%
  #       filter(str_detect(streetName, "51st Avenue Northeast"))
  #
  #     tmp_filter %>%
  #       mapview(zcol = "bearing", burst = F
  #       )
  #
  #     color_palette <- colorRampPalette(c("red", "blue", "yellow", "orange"))(360)
  #
  #     tmp %>%
  #       mapview(zcol = "bearing", burst = F
  #               # ,color = colorRampPalette(c("red", "blue", "red"))(360)
  #               ,color = colorRampPalette(c("red", "blue", "yellow", "orange"))(361)
  #               ) %>%  print()
  #   }
  # }


  {
    link_selections = list(
      mvmnt_df$mvmnt_desc
      ,mvmnt_df$ttl_seq
      ,mvmnt_df$bearing
    ) %>%
      pmap(function(x, y, z) {
        print(str_glue("{strg_make_space_2()}Select links for: {x}"))
        tmp_list = list()

        for (i in 1:y){
          print(str_glue("Links for {i} ({i}/{y}) movement pattern..."))
          tmp_list[[str_glue("seq_{i}")]] =
            table_network_data_sf_jit %>%
            filter(str_detect(flags, "ALLOWS_CAR")) %>%
            filter(eval(parse(text = z))) %>%
            mapedit::selectFeatures() %>%
            pull(stableEdgeId)
          Sys.sleep(2)
        }

        tmp_list_named = setNames(list(tmp_list), x)

        return(tmp_list_named)
      })

    link_selections_df = link_selections %>%
      flatten() %>%
      flatten_named_list() %>%
      tidyr::separate(col = "name", into = c("intersection", "sequence"), sep = "\\.")

    write_csv(link_selections_df, here::here(directory_path, 'link_selections_smkypoint.csv'))

    link_selections_index_pro = paste0("'", sort(unique(link_selections_df$value)), "'", collapse = ", ")

    # table_network_data_sf %>%
    #   filter(stableEdgeId %in% unique(link_selections_df$value)) %>%
    #   st_as_sf(wkt = "geometry", crs = 4326) %>%
    #   mapview()

    review_map = unique(link_selections_df$intersection) %>%
      map(~{
        link_selections_df %>%
          filter(intersection == .x) %>%
          merge(table_network_data_sf, .
                ,by.x = "stableEdgeId", by.y = "value") %>%
          mapview(zcol = "sequence", layer.name = .x
                  # ,label = "label"
                  ,lwd = 4
                  ,color = hcl.colors(5, palette = "viridis")
          )
      }) %>%
      reduce(`+`)

    review_map

    #need to have a check here if we want to reselect one of them by name

    message(str_glue("Completed link selection{gauntlet::strg_make_space_2(last  = F)}"))
  }

  #sec: table query
  {
    message(str_glue("{gauntlet::strg_make_space_2()}Starting data acquisition process....."))

    table_trips_that_use_links = bigrquery::bq_project_query(
      customer_name
      ,stringr::str_glue("select * from (
select * except(network_link_ids)
,ROW_NUMBER ()
    OVER (PARTITION BY activity_id) AS link_ord
from `{trip_table}`, unnest(network_link_ids) as network_links
where 1=1
and mode in ({mode_type_pro})
--and mode in ('COMMERCIAL', 'PRIVATE_AUTO')
)
where 1 = 1
and network_links in ({link_selections_index_pro});"))

#get aggregated links if you want them
{
  #NOTE: use this section if you want to get link volumes using the data above
#   table_agg_links = bigrquery::bq_project_query(
#   customer_name
#   ,stringr::str_glue("select network_links, mode, count(*) as count
# from {replica_temp_tbl_name(table_trips_that_use_links)}
# group by network_links, mode;"))
#
# table_agg_links_dl = bigrquery::bq_table_download(table_agg_links)
#
# agg_links_sf = table_network_data_sf %>%
#   merge(table_agg_links_dl %>%
#           pivot_wider(values_from = "count", names_from = "mode") %>%
#           mutate(
#             ttl_count = COMMERCIAL+PRIVATE_AUTO
#             ,pct_commercial = 100*dgt2(COMMERCIAL/ttl_count))
#         ,by.x = 'stableEdgeId', by.y = "network_links") %>%
#   select(stableEdgeId, streetName, highway, PRIVATE_AUTO, COMMERCIAL, ttl_count, pct_commercial)
#
# mapview(agg_links_sf, zcol = "ttl_count", layer.name = "Total Volume") +
#   mapview(agg_links_sf, zcol = "PRIVATE_AUTO", layer.name = "Private Auto") +
#   mapview(agg_links_sf, zcol = "COMMERCIAL", layer.name = "Commercial") +
#   mapview(agg_links_sf, zcol = "pct_commercial", layer.name = "Commercial % Share")
}


table_pro = bigrquery::bq_project_query(
  customer_name
  ,stringr::str_glue("select
activity_id,mode, network_links,vehicle_type, link_ord
,start_lng, start_lat, end_lng, end_lat, destination_bgrp, origin_bgrp
,ROW_NUMBER ()
    OVER (PARTITION BY activity_id) AS seq_ord
,count(*)
    OVER (PARTITION BY activity_id) AS act_link_count
from {replica_temp_tbl_name(table_trips_that_use_links)}
order by activity_id, link_ord;"))

info(logger, str_glue("queired_mvmnt_trips: {replica_temp_tbl_name(table_pro)}"))

message(str_glue("Query resulted in {bigrquery::bq_table_nrow(table_pro)} through identified movement patterns"))

check_continue = robust_prompt_used("conntinue and download")
stopifnot("Aborted" = check_continue)

turning_links = bigrquery::bq_table_download(table_pro
                                             ,page_size = 1000,
                                             quiet = F)


  }

  #sec: perform QC and processing operation
  {
    message(str_glue("{gauntlet::strg_make_space_2()}Starting data V&V process....."))

    counties_sel = mapedit::selectFeatures(counties) %>%
      st_transform(4326)

    processed_link_ods = unique(link_selections_df$intersection) %>%
      # x = unique(link_selections_df$intersection)[1]
      map(~{
        link_sub = link_selections_df %>%
          filter(intersection == .x)

        intersection = .x

        index_max_seq = max(parse_number(link_sub$sequence))

        tl_sub = turning_links %>%
          filter(network_links %in% link_sub$value) %>%
          arrange(activity_id, seq_ord ) %>%
          group_by(activity_id) %>%
          mutate(seq_ord_rltv = row_number()) %>%
          ungroup() %>%
          group_by(activity_id) %>%
          filter(n() == length(link_sub$value)) %>%
          ungroup() %>%
          mutate(mvmnt = intersection)

        {
          #NOTE: this section was used to aggregate by ODs for the texas location
        # agg_od = tl_sub %>%
        #   select(activity_id, mode, vehicle_type, destination_bgrp, origin_bgrp, mvmnt) %>%
        #   unique() %>%
        #   mutate(
        #     destination_pro = case_when(
        #       str_trunc(destination_bgrp, 5, "right", ellipsis = "") %in% counties_sel$GEOID ~ destination_bgrp
        #       ,T~str_trunc(destination_bgrp, 5, "right", ellipsis = ""))
        #     ,origin_pro = case_when(
        #       str_trunc(origin_bgrp, 5, "right", ellipsis = "") %in% counties_sel$GEOID ~ origin_bgrp
        #       ,T~str_trunc(origin_bgrp, 5, "right", ellipsis = ""))
        #   ) %>%
        #   count(vehicle_type, mvmnt, destination_pro, origin_pro )
          }

        temp = c("start", "end") %>%
          map_df(~{
            tl_sub %>%
              st_as_sf(
                coords = c(
                  paste0(.x, "_lng")
                  ,paste0(.x, "_lat")
                  ), crs = 4326
                ) %>%
              mutate(data_set = paste0(intersection, "_", .x))
            }) %>%
          select(activity_id, mode, vehicle_type
                 ,end_lng, end_lat, start_lng, start_lat, data_set) %>%
          unique()

        return(list(temp, agg_od))

      }, .progress = "Perfroming intersection quality checks")

    counties = tigris::counties("TX")

    counties_sel = mapedit::selectFeatures(counties)

    bgs = tigris::block_groups(state = "WA", county = counties_sel$COUNTYFP)
    bgs = bgs %>% st_transform(4326)

    processed_mvmnt_links = processed_link_ods %>%
      map_df(~.x[[1]])

    combined_map = processed_mvmnt_links %>%
      group_by(data_set_1 = data_set) %>%
      group_map(~{
        .x %>%
          # st_join(counties) %>%
          # st_drop_geometry() %>%
          # group_by(GEOID) %>%
          # summarise(count = n()) %>%
          # merge(counties, ., by = "GEOID") %>%
          mapview(
            #zcol = "count"
            layer.name = str_replace(unique(.x$data_set), "mvmnt", "data")
            ,alpha.regions = .5
            ,alpha = .5)
      }) %>%
      reduce(`+`)  +
      review_map

    combined_map@map %>%
      leaflet::hideGroup(c(str_replace(unique(processed_mvmnt_links$data_set), "mvmnt", "data")))






    tmp = processed_mvmnt_links %>%
      group_by(data_set_1 = data_set) %>%
      group_map(~{
        .x %>%
          st_join(counties) %>%
          st_drop_geometry() %>%
          group_by(GEOID, data_set) %>%
          summarise(count = n()) #%>%
          # merge(counties, ., by = "GEOID")

      }) %>%
      reduce(bind_rows)

    tmp %>%
      group_by(GEOID, direction = gsub(".*[[:digit:]]_", "\\1", data_set)) %>%
      mutate(pct_share = 100*dgt3(count/sum(count))) %>%
      group_by(GEOID) %>%
      merge(counties, ., by = "GEOID") %>%
      mutate(label = str_glue("{data_set}<br>Percent Share: {pct_share}%<br>Count: {count}<br>County {NAMELSAD}")) %>%

      group_by(data_set_1 = data_set) %>%
      group_map(~{
        .x %>%
          mapview(
            zcol = "pct_share"
            ,layer.name = unique(.x$data_set)
            ,popup = "label"
            ,alpha.regions = .5
            ,alpha = .5)

      }) %>%
      reduce(`+`)


    counties = read_sf(
      here::here(directory_path, "tx_counties.gpkg")
    )

    bgs = read_sf(
      here::here(directory_path, "tx_bgs.gpkg")
    )

    study_area_ods_sf = bind_rows(
      counties %>% select(GEOID, NAMELSAD) %>% filter(!(GEOID %in% counties_sel$GEOID))
      ,bgs %>% select(GEOID = GEOID10, NAMELSAD = NAMELSAD)
    )

    mapview(study_area_ods_sf, label = "NAMELSAD")


    processed_agg_od = processed_link_ods %>%
      map_df(~.x[[2]])

    processed_agg_od_pro = processed_agg_od %>%
      group_by(mvmnt, destination_pro, origin_pro) %>%
      summarise(count = sum(n)) %>%
      ungroup() %>%
      mutate(direction = gsub(".*[[:digit:]]_", "\\1", mvmnt)) %>%
      group_by(destination_pro) %>%
      mutate(dest_ttl_count = sum(count)) %>%
      group_by(destination_pro, mvmnt) %>%
      mutate(dest_highway_pct_share = 100*dgt3(sum(count)/dest_ttl_count)) %>%
      ungroup() %>%
      # filter(destination_pro == 48491) %>%
      # arrange(desc(count)) %>%
      merge(study_area_ods_sf %>%  st_drop_geometry() %>% select(GEOID, destination_name = NAMELSAD)
            ,by.x = "destination_pro", by.y = "GEOID") %>%
    merge(study_area_ods_sf %>%  st_drop_geometry() %>% select(GEOID, origin_name = NAMELSAD)
          ,by.x = "origin_pro", by.y = "GEOID") %>%
      select(mvmnt, direction, starts_with("destination"), starts_with("origin"), count, dest_ttl_count, dest_highway_pct_share)

    write.csv(processed_agg_od_pro, here::here(directory_path, "tabular_processed_agg_od_pro.csv"))

    study_area_ods_sf %>%
      merge(processed_agg_od_pro, by.x = "GEOID", by.y = "origin_pro") %>%
      mapview(zcol = "count") + study_area_ods_sf




      group_by(GEOID, ) %>%
      mutate(pct_share = 100*dgt3(count/sum(count))) %>%
      group_by(GEOID) %>%
      merge(counties, ., by = "GEOID") %>%
      mutate(label = str_glue("{data_set}<br>Percent Share: {pct_share}%<br>Count: {count}<br>County {NAMELSAD}")) %>%























































    processed_mvmnt_links %>%
      filter(activity_id == "6158222008380857117")

    turning_links %>%
      filter(activity_id == "6158222008380857117")


  }


  write_sf(processed_mvmnt_links, here::here(directory_location, 'processed_mvmnt_links.gpkg'))

  return(processed_mvmnt_links)
}
