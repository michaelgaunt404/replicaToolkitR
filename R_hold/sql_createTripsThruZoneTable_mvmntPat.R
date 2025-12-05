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
#' # none
#'
#' @export
sql_createTripsThruZoneTable_mvmntPat = function(customer_name, trip_table, table_sa_poly_index, activity_id_pro, table_activity_mvmnt_seq_list_comb) {
  message(stringr::str_glue("{make_space()}\nCreating trips through zone table now...."))


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
    from (select * from `{trip_table}`
    where 1=1
    AND activity_id in ({activity_id_pro})) as data_1
    right join {replica_temp_tbl_name(table_activity_mvmnt_seq_list_comb)} as data_2 on
data_1.activity_id = data_2.activity_id_duplicate")

  table_trips_thru_zone <- bigrquery::bq_project_query(customer_name, query)

  message(stringr::str_glue("Completed{make_space()}"))

  return(table_trips_thru_zone)
}
