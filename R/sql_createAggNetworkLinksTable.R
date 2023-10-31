#' Aggregate network links by mode, vehicle type, and origin/destination classification.
#'
#' This function generates a BigQuery query to aggregate network links based on
#' mode, vehicle type, and the origin and destination classification of trips.
#'
#' @param customer_name The name of the BigQuery customer project.
#' @param table_trips_thru_zone The name of the temporary table containing trips through a zone.
#' @param table_network The name of the temporary network table.
#'
#' @return A BigQuery result containing aggregated network links.
#'
#' @examples
#' queryAggNetworkLinks("your_project_name", "your_trips_thru_zone_table", "your_network_table")
#'
createAggNetworkLinksTable <- function(customer_name, table_trips_thru_zone, table_network) {
  message(stringr::str_glue("{make_space()}\nLink aggreations commencing...."))

  message(stringr::str_glue("Only links that fit user supplied criteria will be filtered"))

  #TODO - this can be potentially be optimized to take a string to define attributes to aggregate on

  query <- str_glue("select
    mode, vehicle_type,
    origin_poly, flag_sa_origin,
    flag_sa_destination,
    network_link_ids_unnested,
    count(*) as count
    from (
    select
        activity_id, mode, vehicle_type,
        origin_bgrp, origin_poly, flag_sa_origin,
        destination_bgrp, destination_poly, flag_sa_destination,
        network_link_ids_unnested,
        ROW_NUMBER() OVER (PARTITION BY activity_id) AS index
    from {replica_temp_tbl_name(table_trips_thru_zone)}
        ,unnest(network_link_ids) as network_link_ids_unnested
    )
    where network_link_ids_unnested in
             (select distinct stableEdgeId from {replica_temp_tbl_name(table_network)})
    group by
    mode, vehicle_type,
    origin_poly, flag_sa_origin,
    flag_sa_destination,
    network_link_ids_unnested")

  table_agg_network_links <- bigrquery::bq_project_query(customer_name, query)

  message(stringr::str_glue("Completed{make_space()}"))

  return(table_agg_network_links)
}
