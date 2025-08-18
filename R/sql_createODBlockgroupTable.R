#' Perform Origin and Destination Aggregations
#'
#' This function logs a message and performs the aggregation of origin and destination data.
#'
#' @param customer_name The customer's name.
#' @param table_trips_thru_zone The name of the trips table.
#' @param replica_temp_tbl_name The name of the replica temp table.
#'
#' @return A message indicating the aggregation process.
#'
#' @export
sql_createODBlockgroupTable = function(customer_name, table_trips_thru_zone, mvmnt_query = F) {
  message(stringr::str_glue("{gauntlet::strg_make_space_2()}Origin and Destination aggregations commencing...."))

  cols = str_glue("mode, vehicle_type, origin_poly, flag_sa_origin,
                      destination_poly, flag_sa_destination {ifelse(  mvmnt_query, ', mvmnt, mvmnt_seq', '') }")

  query = stringr::str_glue("select {cols}, count(*) as count
                      from {replica_temp_tbl_name(table_trips_thru_zone)}
                      group by {cols};")

  table_simple_origin_destination = bigrquery::bq_project_query(
    customer_name, query)

  return(table_simple_origin_destination)
}












