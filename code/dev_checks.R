

library(mapview)
library(data.table)

# table_agg_by_link_subset = bq_table_download(table_agg_by_link_subset)


index_links_got = unique(table_agg_by_link_subset$network_link_ids_unnested) %>%
  # .[1] %>%
  str_trunc(., 14, "right", "")

# table_network = bq_table_download(table_network)
table_network_gis = table_network %>%
  mutate(stableEdgeId_trunc = str_trunc(stableEdgeId, 14, "right", "")) %>%
  mutate(flag_links_hit = case_when(stableEdgeId_trunc %in% index_links_got~"Hit", T~"Miss")) %>%
  st_as_sf(wkt = "geometry", crs = 4326)

table_network_gis %>%
  # st_jitter(.00005) %>%
  mapview::mapview(zcol = "flag_links_hit", burst = T)


table_network_gis %>%
  filter(str_detect(stableEdgeId, "^17118394272")) %>%
  data.frame()
table_network_gis %>%  count(flag_links_hit)


table_trip_first_link_pro = bq_table_download(table_trip_first_link_pro)

table_trip_first_link_pro %>%
  st_as_sf(coords = c("startLon", "startLat"), crs = 4326) %>%
  filter(count>=5) %>%
  mapview::mapview(
    zcol = "count"
  )


block_groups = tigris::block_groups(state = "WA", year = 2010)

# table_simple_origin_destination = bq_table_download(table_simple_origin_destination)
table_simple_origin_destination = "C:/Users/USMG687637/Documents/071_projects/replica_testing/data/req_dev/data_20230112_165230/replica_trip_origin_destination.csv" %>%  data.table::fread()
table_trip_first_link_pro = "C:/Users/USMG687637/Documents/071_projects/replica_testing/data/req_dev/data_20230112_165230/replica_trip_origin_links.csv" %>%  data.table::fread()


od_agg = table_simple_origin_destination %>%
  group_by(vehicle_type, origin_poly) %>%
  summarise(count_from_od_table = sum(count)) %>%
  ungroup()

origin_link_agg = table_trip_first_link_pro %>%
  group_by(vehicle_type, origin_poly) %>%
  summarise(count_from_first_links = sum(count)) %>%
  ungroup()

od_agg %>%
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


#main header====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#main header====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#aggregation functions===========================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#these functions emulate link aggregations functions that were first used in zz_replica
# count_percent_zscore_dt = function(data, grp_c = ..., grp_p = ..., grp_z = ...,
#                                    col, prefix = NULL, rnd = NULL, cntr_scl = FALSE){
#
#   tmp = data %>%
#     data.table::data.table() %>%
#     .[,.(count = sum(.SD)), .SDcols = col, by = grp_c] %>%
#     .[,`:=`(percent = (count/sum(count)) %>%
#               { if (!is.null(rnd)) round(., rnd) else .}), by = grp_p] %>%
#     { if (cntr_scl) (.) %>%
#         .[,`:=`(zscore = as.vector(scale(count))), by = grp_z]
#       else .}
#
#   if (is.null(prefix)){
#     tmp = tmp
#   } else {
#     newname1 = str_glue("{prefix}_count")
#     newname2 = str_glue("{prefix}_percent")
#     rename(tmp, !!newname1 := count, !!newname2 := percent)
#   }
#
#   return(tmp)
#
# }

# normalize_min_max <- function(x, na.rm = TRUE) {
#   return((x - min(x)) /(max(x)-min(x)))
# }

test = aggregate_network_links(
  location = "data/req_dev"
  ,folder = "data_20230117_092037"
  ,network_object = NULL
  ,auto_save = F
)













