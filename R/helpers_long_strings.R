
# qs_log_inputs = function(){
#  stringr::str_glue("{make_space()}\nLogging Query Inputs\nPath to network boundary file: {bb_network_layer}\nPath to study area boundary file: {bb_sa_layer}\nCutsomer Name: {customer_name}\nSchema Table: {trip_table}\nLinks Provided:{make_space('-', n = 10)}\n{paste0(str_glue('{sort(query_links)}'),collapse = '\n')}{make_space('-', n = 10)}")
# }

# qs_log_inputs = stringr::str_glue("{make_space()}\nLogging Query Inputs\nPath to network boundary file: {bb_network_layer}\nPath to study area boundary file: {bb_sa_layer}\nCutsomer Name: {customer_name}\nSchema Table: {trip_table}\nLinks Provided:{gauntlet::make_space('-', n = 10)}\n{paste0(str_glue('{sort(query_links)}'),collapse = '\n')}{gauntlet::make_space('-', n = 10)}")


# origin_poly
# destination_poly
# flag_sa_origin
# flag_sa_destination
#queries

# qs_table_network = function(){
#   str_glue("select * from (
# select *,
# ST_INTERSECTS(
# ST_GEOGFROMTEXT('{list_wkt_objects[[1]]}')
# ,geometry) as flag_contains
# from `{network_table}`
# where highway in ({links_pro})
# )
# where flag_contains = TRUE")
# }

# qs_table_sa_poly_index = function(){
#   str_glue("select * from (
# select *,
# ST_INTERSECTS(
# ST_GEOGFROMTEXT('{list_wkt_objects[[2]]}')
# ,surface_point) as flag_contains
# from `Geos.bgrp`
# )
# where flag_contains = TRUE")
# }

# qs_table_trip_subset = function(){
#   str_glue("select distinct activity_id, network_link_ids
# from
# (select *
# from `{trip_table}`
# where mode = 'COMMERCIAL'
# ), unnest(network_link_ids) as network_link_ids
# ;")
# }

# qs_table_trip_network_match = function(){
#   str_glue("select distinct activity_id
# from {replica_temp_tbl_name(table_trip_subset)}
# where
# 1 = 1
# and network_link_ids in (select stableEdgeId from {replica_temp_tbl_name(table_network)});")
# }

# qs_table_trips_thru_zone = function(){
#   str_glue("select *
# ,case
# when origin_bgrp in (select raw_id from {replica_temp_tbl_name(table_sa_poly_index)}) then origin_bgrp
# else 'out of study area'
# END as origin_poly
# ,case
# when destination_bgrp in (select raw_id from {replica_temp_tbl_name(table_sa_poly_index)}) then destination_bgrp
# else 'out of study area'
# END as destination_poly
# ,case
# when origin_bgrp in (select raw_id from {replica_temp_tbl_name(table_sa_poly_index)}) then 'internal'
# else 'external'
# END as flag_sa_origin
# ,case
# when destination_bgrp in (select raw_id from {replica_temp_tbl_name(table_sa_poly_index)}) then 'internal'
# else 'external'
# END as flag_sa_destination
# from (select *
# from `{trip_table}`
# where 1 = 1
# and activity_id in (select activity_id from {replica_temp_tbl_name(table_trip_network_match)}))")
# }



##QUERIES: OD===================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# qs_table_simple_origin_destination = function(){
#   str_glue("select mode
#       ,vehicle_type
#       ,origin_poly, flag_sa_origin
#       ,destination_poly, flag_sa_destination
#       ,count(*) as count
#       from {replica_temp_tbl_name(table_trips_thru_zone)}
#       group by mode, vehicle_type
#            ,origin_poly, flag_sa_origin
#       ,destination_poly, flag_sa_destination;")
# }

# qs_table_ordered_trip_links = function(){
#   str_glue("select
#     activity_id, mode, vehicle_type
#     ,origin_bgrp, origin_poly, flag_sa_origin
#     ,destination_bgrp, destination_poly, flag_sa_destination
#     ,network_link_ids_unnested
#     ,ROW_NUMBER ()
#     OVER (PARTITION BY activity_id) AS index
#     from {replica_temp_tbl_name(table_trips_thru_zone)}
#                           ,unnest(network_link_ids) as network_link_ids_unnested;")
# }

# qs_table_trip_first_link = function(){
#   str_glue("select
#   mode, vehicle_type
#   ,origin_bgrp, origin_poly, flag_sa_origin, network_link_ids_unnested
#   , count(*) as count
#   from {replica_temp_tbl_name(table_ordered_trip_links)}
#   where 1 = 1
#   and index = 1
#   group by mode, vehicle_type,origin_bgrp, origin_poly, flag_sa_origin, network_link_ids_unnested;")
# }

# qs_table_trip_first_link_pro = function(){
#   str_glue("select
# table_left.*
# ,table_right.*
# from {replica_temp_tbl_name(table_trip_first_link)} table_left
# left join (select stableEdgeId,startLat,startLon,endLat,endLon
# from {network_table}
# where stableEdgeId in (select network_link_ids_unnested from {replica_temp_tbl_name(table_trip_first_link)})) table_right
# on (table_left.network_link_ids_unnested = table_right.stableEdgeId);")
# }

##QUERIES: network==============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# qs_table_agg_by_link = function(){
#   str_glue("select
# mode, vehicle_type
# ,origin_poly, flag_sa_origin
# ,flag_sa_destination
# ,network_link_ids_unnested
# ,count(*) as count
# from {replica_temp_tbl_name(table_ordered_trip_links)}
# group by
# mode, vehicle_type
# ,origin_poly, flag_sa_origin
# ,flag_sa_destination
# ,network_link_ids_unnested")
# }

# qs_table_agg_by_link_subset = function(){
#   str_glue("select *
# from {replica_temp_tbl_name(table_agg_by_link)}
# where network_link_ids_unnested in
#          (select distinct stableEdgeId from {replica_temp_tbl_name(table_network)})")
# }

# qs_table_agg_by_link_sum = function(table){
# str_glue("
# select flag_link, count(*) as count
# from (
# select *,
# case
# when count = 1 then '1 count'
# when count = 2 then '2 count'
# when count = 3 then '3 count'
# when count = 4 then '4 count'
# when count = 5 then '5 count'
# when count <= 10 then '6-10 count'
# else '11 or greater' end as flag_link
# from {replica_temp_tbl_name(table)}
# ) group by flag_link
# order by count")
# }

# mes_network_size = function(){
#   str_glue("{make_space()}\nUser supplied inputs resulted in {gauntlet::pretty_num(sum(summary_table_link_counts$count))} records in link aggregation table....\nSee the following table:{make_space('-', 30)}\n{paste0(capture.output(summary_table_link_counts), collapse = '\n')}{make_space('-', 30)}\nBy default, links with less than 5 counts on them are removed\n---this would result in downloading {summary_table_link_counts[[3, 6]]} records....\n---An ideal number of records is ~500,000")
# }

# mes_network_size_option = function(){
#   str_glue("If your selection has resulted in too many records, you can............
#          1) Decrease the study area layer resulting in less originating polys
#          2) Decrease the size of the network layer supplied to the function
#          3) Reduce the number of link types queired by the function by changing query_links input")
# }
























