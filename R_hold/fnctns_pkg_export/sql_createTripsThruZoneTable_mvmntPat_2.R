sql_createTripsThruZoneTable_mvmntPat_2 = function(customer_name, trip_table, table_sa_poly_index, activity_id_pro, table_activity_mvmnt_seq_list_comb) {
  message(stringr::str_glue("{make_space()}\nCreating trips through zone table now...."))

  #have to break out index string if its really long
  temp_query_chunks = strg_chunk_split_by_delim(input_string = activity_id_pro, max_length = 800000, delimiter = ",")

  list_temp_table_trips_thru_zone =
    temp_query_chunks %>%
    map(~{
      # browser()
      # gauntlet::log_and_info(str_glue('Processing: {x} and {y}'), logger)

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
    AND activity_id in ({.x})) as data_1
    right join {replica_temp_tbl_name(table_activity_mvmnt_seq_list_comb)} as data_2 on
data_1.activity_id = data_2.activity_id_duplicate")

      temp_table_trips_thru_zone <- bigrquery::bq_project_query(customer_name, query)

      return(temp_table_trips_thru_zone)

    })



  if (length(list_temp_table_trips_thru_zone)==1){
    query_string = stringr::str_glue('select * from {replica_temp_tbl_name(list_temp_table_trips_thru_zone[[1]])};')
  }else {
    query_string = stringr::str_glue('{paste0("select * from `", unlist(map(list_temp_table_trips_thru_zone, replica_temp_tbl_name)), "`",  collapse = " union all ")};')
  }

  table_trips_thru_zone = bigrquery::bq_project_query(
    customer_name
    ,query_string)

  message(stringr::str_glue("Completed{make_space()}"))

  return(table_trips_thru_zone)
}
