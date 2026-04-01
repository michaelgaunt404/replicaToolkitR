#' Fetch Polygons from Replica BigQuery
#'
#' @description Internal helper to batch query Replica geography tables.
#'
#' @param ids Vector of GEOIDs.
#' @param geo_type Character string of geography level (e.g., "bgrp", "trct").
#' @param customer_name BigQuery project/dataset prefix.
#'
#' @importFrom stringr str_glue
#' @importFrom purrr map_df
#' @importFrom bigrquery bq_project_query bq_table_download
#' @importFrom data.table as.data.table
#' @export
rplc_fetch_geos = function(ids, geo_type, customer_name) {
  id_string = paste0("'", sort(unique(ids)), "'", collapse = ",")
  batches = rplc_split_long_string(id_string, max_len = 5e5)
  message(stringr::str_glue("Querying {geo_type}: {length(batches)} batch(es)"))

  purrr::map_df(batches, ~{
    query = stringr::str_glue("select * from {customer_name}.Geos.{geo_type} where raw_id in ({.x})")
    bigrquery::bq_table_download(bigrquery::bq_project_query(customer_name, query, quiet = TRUE))
  }) %>%
    data.table::as.data.table()
}


















