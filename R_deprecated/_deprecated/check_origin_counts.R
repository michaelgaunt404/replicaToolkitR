
#PROBABLY DEPRECATED
check_origin_counts = function(location, folder){
  # location = "C:/Users/USMG687637/Documents/071_projects/replica_testing/data/req_dev"
  # folder = "data_20230112_165230"
#
#   table_simple_origin_destination = here::here(location, folder, "replica_trip_origin_destination.csv") %>%
#     data.table::fread()
#
#   table_trip_first_link_pro = here::here(location, folder, "replica_trip_origin_links.csv") %>%
#     data.table::fread()
#
#   od_agg = table_simple_origin_destination %>%
#     group_by(vehicle_type, origin_poly) %>%
#     summarise(count_from_od_table = sum(count)) %>%
#     ungroup()
#
#   origin_link_agg = table_trip_first_link_pro %>%
#     group_by(vehicle_type, origin_poly) %>%
#     summarise(count_from_first_links = sum(count)) %>%
#     ungroup()
#
#   comparision_output = od_agg %>%
#     merge(.
#           ,origin_link_agg
#           ,by = c('vehicle_type', 'origin_poly')
#     ) %>%
#     mutate(flag_check_same_origin_counts = (count_from_od_table == count_from_first_links)
#            ,count_diff = count_from_od_table - count_from_first_links) %>%
#     group_by(vehicle_type, flag_check_same_origin_counts) %>%
#     summarise(total_count = n()) %>%
#     ungroup()
#
#   message()
#
#   message(stringr::str_glue("{paste0(capture.output(comparision_output), collapse = '\n')}"))
}



