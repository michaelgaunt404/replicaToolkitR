#' Query trips through a zone and classify origin and destination.
#'
#' This function generates a BigQuery query to select trips through a zone and
#' classify their origin and destination points based on whether they are inside
#' the specified spatial index. It also categorizes the trips as internal or external.
#'
#' @param customer_name The name of the BigQuery customer project.
#' @param trip_table The name of the trip data table in BigQuery.
#' @param table_sa_poly_index The name of the temporary table containing study area blockgroups.
#' @param mode_type_pro A vector of mode types to filter the data.
#'
#' @return A BigQuery result containing trips through a zone with classified origin
#' and destination points.
#'
#' @examples
#' queryTripsThruZone("your_project_name", "your_trip_table", c("mode1", "mode2"))
#'
#' @export
sql_createTripsThruZoneTable <- function(customer_name, trip_table, table_sa_poly_index, mode_type_pro) {
  message(stringr::str_glue("{strg_make_space_2()}Creating trips through zone table now...."))


  query <- stringr::str_glue("select *,
    case
    when origin_bgrp in (select raw_id from {replica_temp_tbl_name(table_sa_poly_index)}) then origin_bgrp
    else 'out of study area'
    end as origin_poly,
    case
    when destination_bgrp in (select raw_id from {replica_temp_tbl_name(table_sa_poly_index)}) then destination_bgrp
    else 'out of study area'
    end as destination_poly,
    case
    when origin_bgrp in (select raw_id from {replica_temp_tbl_name(table_sa_poly_index)}) then 'internal'
    else 'external'
    end as flag_sa_origin,
    case
    when destination_bgrp in (select raw_id from {replica_temp_tbl_name(table_sa_poly_index)}) then 'internal'
    else 'external'
    end as flag_sa_destination
    from `{trip_table}`
    where 1=1
    AND mode in ({mode_type_pro})
    AND ((origin_bgrp in (select raw_id from `{replica_temp_tbl_name(table_sa_poly_index)}`) OR
          destination_bgrp in (select raw_id from `{replica_temp_tbl_name(table_sa_poly_index)}`)))")

  table_trips_thru_zone <- bigrquery::bq_project_query(customer_name, query)

  message(stringr::str_glue("Completed{strg_make_space_2()}"))

  return(table_trips_thru_zone)
}
