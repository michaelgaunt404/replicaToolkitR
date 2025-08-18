#' Create a temporary BigQuery table summarizing network link usage between OD pairs.
#'
#' This function constructs and submits a BigQuery query to create a temporary table
#' that aggregates network link usage between origin-destination (OD) polygon pairs.
#' It filters only for network links that exist in a specified network table, and counts the number
#' of trips traversing those links, grouped by attributes such as mode and vehicle type.
#'
#' @param customer_name The name of the BigQuery customer project.
#' @param table_trips_thru_zone The name of the BigQuery table containing trip data through polygon zones.
#' @param table_network The name of the BigQuery network table containing link geometry and stableEdgeId values.
#'
#' @return A BigQuery table object with aggregated counts of trips across network links by OD pair and mode/vehicle attributes.
#'
#' @importFrom bigrquery bq_project_query
#' @importFrom stringr str_glue
#' @importFrom gauntlet strg_make_space_2
#'
#' @export
#'
#' @examples
#' \dontrun{
#' sql_createAggNetworkLinksTable_flow_btwn_od(
#'   customer_name = "your_project_id",
#'   table_trips_thru_zone = "project.dataset.trips_thru_zone",
#'   table_network = "project.dataset.network_links"
#' )
#' }
sql_createAggNetworkLinksTable_flow_btwn_od = function(customer_name, table_trips_thru_zone, table_network) {
  message(stringr::str_glue("{gauntlet::strg_make_space_2()}Link aggreations commencing...."))

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

  message(stringr::str_glue("Completed{gauntlet::strg_make_space_2(last = F)}"))

  return(table_agg_network_links)
}

















