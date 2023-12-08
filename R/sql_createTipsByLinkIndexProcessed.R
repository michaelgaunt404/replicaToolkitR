#' Process Trips Table by Link Index
#'
#' This function processes a trips table by adding attributes that define the order in which network links are used in each trip.
#'
#' @param customer_name A character string specifying the BigQuery project name.
#' @param table_trips_that_use_links A character string specifying the BigQuery table containing trips with associated network links.
#'
#' @return A BigQuery result object with additional attributes representing the order and count of network links used in each trip.
#'
#' @examples
#' \dontrun{
#' # Example usage
#' result <- createTipsByLinkIndexProcessed("your_project_name", "your_trip_table_with_links")
#' }
#'
#' @importFrom bigrquery bq_project_query
#' @importFrom stringr str_glue
#'
#' @export
createTipsByLinkIndexProcessed <- function(customer_name, table_trips_that_use_links) {
  message(stringr::str_glue("{make_space()}\nPorcessing tips table...."))

  query <- stringr::str_glue("select
activity_id, mode, network_links,vehicle_type, link_ord
,distance_miles,start_time, end_time, destination_bgrp, origin_bgrp,start_lng, start_lat, end_lng, end_lat
,ROW_NUMBER ()
    OVER (PARTITION BY activity_id) AS seq_ord
,count(*)
    OVER (PARTITION BY activity_id) AS act_link_count
from {replica_temp_tbl_name(table_trips_that_use_links)}
order by activity_id, link_ord;")

  tmp_object <- bigrquery::bq_project_query(customer_name, query)

  message(stringr::str_glue("Completed{make_space()}"))

  return(tmp_object)
}



