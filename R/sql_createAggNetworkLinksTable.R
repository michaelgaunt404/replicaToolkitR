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
#' @export
#' @examples
#' \dontrun{
#' # none
#'
#' }
sql_createAggNetworkLinksTable <- function(customer_name, table_trips_thru_zone, table_network, mvmnt_query = F) {
  message(stringr::str_glue("{gauntlet::strg_make_space_2()}Link aggreations commencing...."))

  message(stringr::str_glue("Only links that fit user supplied criteria will be filtered"))

  #TODO - this can be potentially be optimized to take a string to define attributes to aggregate on

  cols_outter = stringr::str_glue("mode, vehicle_type, flag_sa_origin, flag_sa_destination,
    network_link_ids_unnested {ifelse(mvmnt_query, ', mvmnt, mvmnt_seq', '') }")

  # cols_outter = str_glue("mode, vehicle_type, origin_poly, flag_sa_origin, flag_sa_destination,
  #   network_link_ids_unnested {ifelse(mvmnt_query, ', mvmnt, mvmnt_seq', '') }")

  cols_inner = stringr::str_glue("activity_id, mode, vehicle_type, origin_bgrp, origin_poly, flag_sa_origin, destination_bgrp, destination_poly, flag_sa_destination
                        ,network_link_ids_unnested {ifelse(mvmnt_query, ', mvmnt, mvmnt_seq', '') }")

  query <- stringr::str_glue("select {cols_outter}, count(*) as count
    from (
    select {cols_inner},
        ROW_NUMBER() OVER (PARTITION BY activity_id) AS index
    from {replica_temp_tbl_name(table_trips_thru_zone)}
        ,unnest(network_link_ids) as network_link_ids_unnested
    )
    where network_link_ids_unnested in
             (select distinct stableEdgeId from {replica_temp_tbl_name(table_network)})
    group by {cols_outter}")

  table_agg_network_links = bigrquery::bq_project_query(customer_name, query)

  message(stringr::str_glue("Completed{gauntlet::strg_make_space_2()}"))

  return(table_agg_network_links)
}

