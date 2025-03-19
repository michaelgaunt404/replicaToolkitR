# aggregate_network_links = function(location, folder, auto_save = F
#          ,agg_count_object = NULL
#          ,network_object = NULL){
#   #TODO:make this compatible with other polygon types
#   #TODO:review different aggregations - they don't make the most sense
#
#   {
#     # location = "data/req_dev"
#     # folder = "data_20230207_161636"
#     # agg_count_object = NULL
#     # auto_save = F
#     # data('replica_queried_network_cntds')
#     # network_object = replica_queried_network_cntds
#   }
#
#   if (is.null(agg_count_object)){
#     message("Aggregations will be made using file and location...")
#     network_links = here::here(location, folder, "table_agg_by_link_subset_limited.csv") %>%
#       data.table::fread() %>%
#       mutate(flag_trip_type = str_glue("{flag_sa_origin}-{flag_sa_destination}")
#              ,network_link_ids_unnested = str_trunc(network_link_ids_unnested, 14, "right", ""))
#   } else {
#     message("Aggregations will be made using supplied network object...")
#     network_links = agg_count_object %>%
#       mutate(flag_trip_type = str_glue("{flag_sa_origin}-{flag_sa_destination}")
#              ,network_link_ids_unnested = str_trunc(network_link_ids_unnested, 14, "right", ""))
#   }
#
#   stopifnot("You need to supply an object to merge tabular data with, either network centroids or polylines...." = !is.null(network_object))
#   stopifnot("Supplied network object had more than one geometry feature in it, please review and fix...." = (length(unique(st_geometry_type(network_object))) == 1))
#   stopifnot("Supplied network object must be either POINT or LINESTRING, please review and fix...." = (unique(st_geometry_type(network_object))[[1]] %in% c("POINT", "LINESTRING")))
#
#   #pre-process data
#   {
#     if (unique(st_geometry_type(network_object))[[1]] == "POINT"){
#       index_extract = "POINT"
#     } else {
#       index_extract = "LINESTRING"
#     }
#   }
#
#
#   #perform aggregations
#   {
#     message(stringr::str_glue("{make_space()}\nStarting aggreagtion by link and study area destination flag...."))
#
#     #ANL
#     {
#       message(str_glue("{make_space()}\nStarting aggreagtion by link...."))
#
#       agg_link = network_links %>%
#         count_percent_zscore(
#           grp_c = c("mvmnt", "mvmnt_seq", 'network_link_ids_unnested')
#           ,grp_p = c()
#           ,col = count, rnd = 2) %>%
#         mutate(count_nrm_prank_ttl = gauntlet::dgt2(percent_rank(count))
#                ,count_nrm_mmax_ttl = gauntlet::dgt2(normalize_min_max(count)))
#
#       agg_link_mrg = merge(
#         network_object, agg_link
#         ,by.x = "stableEdgeId", by.y = "network_link_ids_unnested", all = T)
#
#       agg_link_mrg_pro = agg_link_mrg %>%
#         filter(!is.na(count)) %>%
#         mutate(label = str_glue(
#           "Link Name: {streetName} ({highway})
#           <br>TTl Link Volume: {count} - {100*count_nrm_mmax_ttl}% (Min-Max norm.)"))
#
#       agg_link_mrg_pro %>%
#         filter(mvmnt_seq  == "seq_1") %>%
#         group_by(mvmnt_1 = mvmnt) %>%
#         group_map(~{
#           temp_data = .x
#           temp_layer_name = unique(temp_data$mvmnt)
#           mapview(temp_data, zcol = "count", lwd = "count", layer.name = temp_layer_name)
#         }) %>%
#         reduce(`+`)
#         mapview(zcol = "count", lwd = "count")
#
#       message("Aggregation complete....")
#     }
#
#     #ANLT
#     {
#       # message(str_glue("{make_space()}\nStarting aggreagtion by link and vehicle type...."))
#       #
#       # agg_link_vehicle_type = network_links %>%
#       #   count_percent_zscore(
#       #     grp_c = c("mvmnt", "mvmnt_seq", 'network_link_ids_unnested', 'vehicle_type')
#       #     ,grp_p = c("mvmnt", "mvmnt_seq", 'network_link_ids_unnested')
#       #     ,col = count, rnd = 2) %>%
#       #   group_by(mvmnt, mvmnt_seq, network_link_ids_unnested) %>%
#       #   mutate(ttl_count_link = sum(count)) %>%
#       #   ungroup() %>%
#       #   mutate(count_nrm_prank_ttl = gauntlet::dgt2(percent_rank(ttl_count_link))
#       #          ,count_nrm_mmax_ttl = gauntlet::dgt2(normalize_min_max(ttl_count_link))) %>%
#       #   group_by(vehicle_type) %>%
#       #   mutate(count_nrm_prank = dgt2(percent_rank(count))
#       #          ,count_nrm_mmax = dgt2(normalize_min_max(count)))
#       #
#       # agg_link_vehicle_type_mrg = merge(
#       #   network_object, agg_link_vehicle_type
#       #   ,by.x = "stableEdgeId", by.y = "network_link_ids_unnested", all = T)
#       #
#       # agg_link_vehicle_type_mrg_pro = agg_link_vehicle_type_mrg %>%
#       #   filter(!is.na(vehicle_type)) %>%
#       #   mutate(label = str_glue(
#       #     "Link Name: {streetName} ({highway})
#       #     <br>TTl Link Volume: {ttl_count_link} - {100*count_nrm_mmax_ttl}% (Min-Max norm.)
#       #     <hr>
#       #     Metrics Adj for Veh. Type: {vehicle_type}
#       #     <br>Link Volume: {count}
#       #     <br>Link Volume (Min-Max norm.): {100*count_nrm_mmax}%"))
#       #
#       #
#       #
#       # agg_link_mrg_pro %>%
#       #   filter(mvmnt_seq  == "seq_1") %>%
#       #   group_by(mvmnt_1 = mvmnt) %>%
#       #   group_map(~{
#       #     temp_data = .x
#       #     temp_layer_name = unique(temp_data$mvmnt)
#       #     mapview(temp_data, zcol = "count", lwd = "count", layer.name = temp_layer_name)
#       #   }) %>%
#       #   reduce(`+`)
#       # mapview(zcol = "count", lwd = "count")
#       #
#       # message("Aggregation complete....")
#     }
#
#     #ANLTT
#     {
#       agg_link_flag = network_links %>%
#         count_percent_zscore(
#           grp_c = c("mvmnt", "mvmnt_seq", 'network_link_ids_unnested', 'flag_trip_type')
#           ,grp_p = c("mvmnt", "mvmnt_seq", 'network_link_ids_unnested')
#           ,col = count, rnd = 2) %>%
#         group_by(mvmnt, mvmnt_seq, network_link_ids_unnested) %>%
#         mutate(ttl_count_link = sum(count)) %>%
#         ungroup() %>%
#         mutate(count_nrm_prank_ttl = gauntlet::dgt2(percent_rank(ttl_count_link))
#                ,count_nrm_mmax_ttl = gauntlet::dgt2(normalize_min_max(ttl_count_link))) %>%
#         group_by(mvmnt, mvmnt_seq, flag_trip_type) %>%
#         mutate(count_nrm_prank = gauntlet::dgt2(percent_rank(count))
#                ,count_nrm_mmax = gauntlet::dgt2(normalize_min_max(count))) %>%
#         ungroup()
#
#       agg_link_flag_mrg = merge(
#         network_object, agg_link_flag
#         ,by.x = "stableEdgeId", by.y = "network_link_ids_unnested", all = T)
#
#       agg_link_flag_mrg_pro = agg_link_flag_mrg %>%
#         filter(!is.na(flag_trip_type )) %>%
#         mutate(label = str_glue(
#           "Link Name: {streetName} ({highway})
#           <br>TTl Link Volume: {ttl_count_link} - {100*count_nrm_mmax_ttl}% (Min-Max norm.)
#           <hr>
#           Metrics Adj for Trip Type: {flag_trip_type}
#           <br>Link Volume: {count}
#           <br>Link Volume (Min-Max norm.): {100*count_nrm_mmax}%"))
#
#       agg_link_flag_mrg_pro %>%
#         filter(mvmnt_seq  == "seq_1") %>%
#         filter(mvmnt %in% c("I5N_to_WA96", "212th_to_airport")) %>%
#         group_by(mvmnt_1 = mvmnt, flag_trip_type_1 = flag_trip_type ) %>%
#         group_map(~{
#           temp_data = .x
#           temp_mvmnt = unique(temp_data$mvmnt)
#           temp_flag_trip_type = unique(temp_data$flag_trip_type)
#           mapview(temp_data, zcol = "count", lwd = "count", layer.name = paste0(temp_mvmnt, "-", temp_flag_trip_type))
#         }) %>%
#         reduce(`+`)
#       mapview(zcol = "count", lwd = "count")
#
#       message("Aggregation complete....")
#     }
#
#     #ANLTO
#     {
#       # message(str_glue("{make_space()}\nStarting aggreagtion by link, vehicle type, and originating poly....{gauntlet::make_space('-')}"))
#       # message("INFO:\nBy default external-external and external-internal trips are removed before aggregation")
#       # message("This is because origin based visualizations and calcultions display locations WITHIN/INTERNAL TO the user provided study area...")
#       # message(str_glue("Prefiltering makes processing faster and limits size of data object{gauntlet::make_space('-')}"))
#       # #NOTE: total link volume calculated here will be less than previous
#       # #--this is because it only counts int-int, and int-ext trips
#       #
#       #
#       # agg_link_vehicle_type_origin = network_links %>%
#       #   filter(!is.na(vehicle_type)) %>%
#       #   filter(origin_poly != "out of study area") %>%
#       #   count_percent_zscore(
#       #     grp_c = c('network_link_ids_unnested', 'vehicle_type', "origin_poly")
#       #     ,grp_p = c('network_link_ids_unnested', "origin_poly")
#       #     ,col = count, rnd = 2) %>%
#       #   group_by(network_link_ids_unnested) %>%
#       #   mutate(ttl_count_link = sum(count)) %>%
#       #   ungroup() %>%
#       #   mutate(count_nrm_prank_ttl = gauntlet::dgt2(percent_rank(ttl_count_link))
#       #          ,count_nrm_mmax_ttl = gauntlet::dgt2(normalize_min_max(ttl_count_link))) %>%
#       #   group_by(origin_poly, vehicle_type) %>%
#       #   mutate(count_nrm_prank = dgt2(percent_rank(count))
#       #          ,count_nrm_mmax = dgt2(normalize_min_max(count))) %>%
#       #   ungroup()
#       #
#       # agg_link_vehicle_type_origin_mrg = merge(
#       #   network_object, agg_link_vehicle_type_origin
#       #   ,by.x = "stableEdgeId", by.y = "network_link_ids_unnested", all = T)
#       #
#       # nrow(filter(agg_link_flag_mrg, is.na(flag_trip_type)))
#       # (nrow(filter(agg_link_flag_mrg, is.na(stableEdgeId))) == 0)
#       #
#       # agg_link_vehicle_type_origin_mrg_pro = agg_link_vehicle_type_origin_mrg %>%
#       #   filter(!is.na(origin_poly)) %>%
#       #   mutate(label = str_glue(
#       #     "Origin: {origin_poly}
#       #     <br>Link name: {streetName} ({highway})
#       #     <br>Link Volume: {ttl_count_link} (int-int/ext trips)
#       #     <hr>
#       #     Metrics Adj for Origin and Vehicle Type: {vehicle_type}
#       #     <br>Link Volume: {count} - {100*count_nrm_mmax}% (Min-Max norm.)"))
#       #
#       # message("Aggregation complete....")}
#   }
#
#   #save out
#   {
#     list_objects = list(agg_link = agg_link_mrg_pro
#                         ,agg_link_flag = agg_link_flag_mrg_pro
#                         ,agg_link_vehicle_type = agg_link_vehicle_type_mrg_pro
#                         ,agg_link_vehicle_type_origin = agg_link_vehicle_type_origin_mrg_pro)
#
#     if (auto_save) {
#       message("You elected to automatically save the returned list object!")
#       file = here::here(location, folder, "aggregated_network_links.rds")
#       message(str_glue("It is saved at this location:\n{file}"))
#       saveRDS(list_objects, file)
#     } else {
#       message("You did not elect to automatically save the returned list object!")
#     }
#   }
#
#   return(list_objects)
#
# }
