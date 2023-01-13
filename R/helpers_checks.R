check_links_download = function(location, folder){
  # location = "data/req_dev"
  # folder = "data_20230112_143236"

  replica_trip_agg_by_link_subset = here::here(location, folder, "replica_trip_agg_by_link_subset.csv") %>%
    data.table::fread()

  check_subset_agg = length(unique(temp$network_link_ids_unnested))

  replica_queried_network = here::here(location, folder, "replica_queried_network.csv") %>%
    data.table::fread()

  check_subset = length(unique(replica_queried_network$stableEdgeId))

  str_glue("The replica network link aggregate table returned {check_subset_agg} unique network links\nThe network subset download returned {check_subset} unique network links{make_space('-')}\n{100*dgt3(check_subset_agg/check_subset)}% of queried links have trip counts associated with them{make_space('-')}")

}


check_origin_counts = function(location, folder){
  # location = "C:/Users/USMG687637/Documents/071_projects/replica_testing/data/req_dev"
  # folder = "data_20230112_165230"

table_simple_origin_destination = here::here(location, folder, "replica_trip_origin_destination.csv") %>%
  data.table::fread()

table_trip_first_link_pro = here::here(location, folder, "replica_trip_origin_links.csv") %>%
  data.table::fread()


od_agg = table_simple_origin_destination %>%
  group_by(vehicle_type, origin_poly) %>%
  summarise(count_from_od_table = sum(count)) %>%
  ungroup()

origin_link_agg = table_trip_first_link_pro %>%
  group_by(vehicle_type, origin_poly) %>%
  summarise(count_from_first_links = sum(count)) %>%
  ungroup()

comparision_output = od_agg %>%
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
}
