rplc_save_out_table = function(table, save_location, file_name)
{
  message(str_glue("Starting data download now..... {gsub('.csv', '\\1', file_name)}"))

  nrow_cloud = bigrquery::bq_table_nrow(table)
  temp_table = bigrquery::bq_table_download(table, n_max = max_record)
  nrow_local = nrow(temp_table)
  check_sizes = ifelse(nrow_local == nrow_cloud , 'GOOD', 'BAD')
  message(str_glue("{nrow_local} of {nrow_cloud} records downloaded  ---- {check_sizes}"))
  write.csv(temp_table, file = here(save_location, file_name) , row.names = F)

  temp_df = data.frame(
    file = file_name
    ,download_status = check_sizes
    ,table_id = replica_temp_tbl_name(table)
  )

  return(temp_df)
}
