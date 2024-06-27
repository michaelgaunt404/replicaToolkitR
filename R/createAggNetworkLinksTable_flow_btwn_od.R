
createAggNetworkLinksTable_flow_btwn_od <- function(customer_name, table_trips_thru_zone, table_network) {
  message(stringr::str_glue("{make_space()}\nLink aggreations commencing...."))

  message(stringr::str_glue("Only links that fit user supplied criteria will be filtered"))

  #TODO - this can be potentially be optimized to take a string to define attributes to aggregate on

  cols_outter = str_glue("poly_name_origin, poly_name_destination, mode, vehicle_type
  ,network_link_ids_unnested")

  cols_inner = str_glue("poly_name_origin, poly_name_destination, activity_id, mode, vehicle_type
  ,network_link_ids_unnested")

  query <- str_glue("select {cols_outter}, count(*) as count
    from (
    select {cols_inner},
        ROW_NUMBER() OVER (PARTITION BY activity_id) AS index
    from {replica_temp_tbl_name(table_custom_poly_list_data)}
        ,unnest(network_link_ids) as network_link_ids_unnested
    )
    where network_link_ids_unnested in
             (select distinct stableEdgeId from {replica_temp_tbl_name(table_network)})
    group by {cols_outter}")

  table_agg_network_links <- bigrquery::bq_project_query(customer_name, query)

  message(stringr::str_glue("Completed{make_space()}"))

  return(table_agg_network_links)
}

















