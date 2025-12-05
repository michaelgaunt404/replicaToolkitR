rplc_make_mvmnt_trip_subsets =  function(link_selections_df
                                         ,customer_name
                                         ,trip_table
                                         ,mode_type_pro
                                         ,logger
                                         ,page_size = NULL
                                         ,page_limit = Inf){

  link_selections_index_pro = paste0(
    "'", sort(unique(link_selections_df$stableEdgeId)), "'", collapse = ", ")

  message(str_glue("{gauntlet::strg_make_space_2()}Starting data acquisition process....."))

  #note: query makes subset of trips that ANY links in index
  #----- just tries to make it smaller to manage
  #----- removes any trips that does not have any links in index
  #----- this is the expensive one
  #----- --thing always about 120 GB since interacting with the big table
  table_trips_that_use_links = sql_createTipsByLinkIndex2(
    customer_name = customer_name
    ,trip_table = trip_table
    ,mode_type_pro = mode_type_pro
    ,link_selections_index_pro = link_selections_index_pro)

  #note: this query processes the data in the above table
  #spec: it creates link order attributes
  table_pro = sql_createTipsByLinkIndexProcessed(
    customer_name = customer_name
    ,table_trips_that_use_links = table_trips_that_use_links
  )

  info(logger, str_glue("queired_mvmnt_trips: {replica_temp_tbl_name(table_pro)}"))

  message(str_glue("Query resulted in {bigrquery::bq_table_nrow(table_pro)} trip/links identified via movement patterns"))

  check_continue = robust_prompt_used("continue and download")
  stopifnot("Aborted" = check_continue)

  turning_links = bigrquery::bq_table_download(
    table_pro, page_size = page_size, quiet = F, n_max = page_limit )

  return(
    list(
      table_trips_that_use_links = table_trips_that_use_links
      ,table_pro = table_pro
      ,turning_links = turning_links
      ,link_selections_index_pro = link_selections_index_pro
    )
  )
}
