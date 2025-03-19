

rplc_assign_mvmntSeq_to_trips = function(
    logger, customer_name, table_pro
    ,processed_link_ods_pro){


  tmp_mvmnt_merge = processed_link_ods_pro %>%
    select(mvmnt, mvmnt_seq) %>%
    unique()

  #note: this creates a crosswalk table for activities and movement locations
  message(str_glue("{gauntlet::strg_make_space_2()}Creating crosswalk table for movements/sequences and activities...."))
  #leaving for trouble shooting
  # tmp_mvmnt_merge = tmp_mvmnt_merge %>%
  #   # filter(mvmnt_seq == "seq_1") %>%
  #   # filter(mvmnt != "I5N_enter")

  table_activity_mvmnt_seq_list =
    list(
      tmp_mvmnt_merge$mvmnt
      ,tmp_mvmnt_merge$mvmnt_seq
    ) %>%
    pmap(function(x, y) {
      # browser()
      gauntlet::log_and_info(str_glue('Processing: {x} and mvmnt {y}'), logger)

      #make big index of links for mvmnt
      pulled_activity_id_pro_mrg = processed_link_ods_pro %>%
        filter(mvmnt == x, mvmnt_seq == y) %>%
        pull(activity_id) %>%
        unique() %>%
        # head() %>%
        paste0("'", ., "'", collapse = ", ")

      #breaks up index into chunkc
      #note: have to do this as index string has to be less than 1.M characters
      temp_query_chunks = strg_chunk_split_by_delim(
        input_string = pulled_activity_id_pro_mrg
        ,max_length = 800000, delimiter = ",")
      # length(temp_query_chunks)

      list_temp_table_poly_subset =
        temp_query_chunks %>%
        map(~{

          temp_table_poly_subset = bigrquery::bq_project_query(
            customer_name
            ,stringr::str_glue("select distinct activity_id as activity_id_duplicate
          ,'{x}' as mvmnt
          ,'{y}' as mvmnt_seq
          from (
select *
from `{replica_temp_tbl_name(table_pro)}`
where activity_id in ({.x}));"))
          return(temp_table_poly_subset)

        })

      if (length(list_temp_table_poly_subset)==1){
        query_string = stringr::str_glue('select * from {replica_temp_tbl_name(list_temp_table_poly_subset[[1]])};')
      }else {
        query_string = stringr::str_glue('{paste0("select * from `", unlist(map(list_temp_table_poly_subset, replica_temp_tbl_name)), "`",  collapse = " union all ")};')
      }

      temp_table_poly_subset = bigrquery::bq_project_query(
        customer_name
        ,query_string)

      gauntlet::log_and_info(str_glue('There are {bigrquery::bq_table_nrow(temp_table_poly_subset)} rows in subset...'), logger)

      return(temp_table_poly_subset)
    }
    )

  table_activity_mvmnt_seq_list_comb = bigrquery::bq_project_query(
    customer_name
    ,stringr::str_glue('{paste0("select * from `", unlist(map(table_activity_mvmnt_seq_list, replica_temp_tbl_name)), "`",  collapse = " union all ")};')) #need space at end

  log4r::info(logger,stringr::str_glue("table_activity_mvmnt_seq_list_comb: {replica_temp_tbl_name(table_activity_mvmnt_seq_list_comb)}"))

  return(
    list(
      table_activity_mvmnt_seq_list_comb = table_activity_mvmnt_seq_list_comb
    )
  )
}
