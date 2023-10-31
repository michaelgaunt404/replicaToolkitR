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
createODBlockgroupTable <- function(customer_name, table_trips_thru_zone) {
  message(stringr::str_glue("{make_space()}\nOrigin and Destination aggregations commencing...."))

  query = stringr::str_glue("select mode, vehicle_type, origin_poly, flag_sa_origin,
                      destination_poly, flag_sa_destination, count(*) as count
                      from {replica_temp_tbl_name(table_trips_thru_zone)}
                      group by mode, vehicle_type, origin_poly, flag_sa_origin,
                      destination_poly, flag_sa_destination;")

  table_simple_origin_destination = bigrquery::bq_project_query(
    customer_name, query)

  return(table_simple_origin_destination)
}












