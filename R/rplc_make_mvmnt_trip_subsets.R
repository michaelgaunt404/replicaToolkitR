#' Create movement-based trip subsets for selected network links
#'
#' This function orchestrates a multi-step BigQuery workflow to identify,
#' process, and optionally download trips that traverse a specified set
#' of network links. It first filters trips to those using any selected
#' links, then derives link-order attributes, and finally downloads the
#' resulting trip–link records.
#'
#' @details
#' The function constructs a SQL-ready list of network link identifiers
#' from \code{link_selections_df} and uses this list to subset a large
#' trips table in BigQuery. Because the initial filtering query operates
#' on a very large source table, it can be computationally expensive.
#'
#' Users are prompted before downloading results to allow inspection
#' of query size and row counts. The downloaded data represents trips
#' that include at least one of the selected links, with additional
#' attributes describing link order and sequence within each trip.
#'
#' @param link_selections_df A data frame containing selected network links;
#'   must include a \code{stableEdgeId} column.
#' @param customer_name Character string; BigQuery project ID.
#' @param trip_table Character string; fully qualified BigQuery trips table.
#' @param mode_type_pro Character string; SQL-ready list of mode values to filter on.
#' @param logger Logger object used for recording query metadata and progress.
#' @param page_size Optional integer; number of rows to fetch per page when downloading.
#' @param page_limit Integer; maximum number of rows to download. Defaults to \code{Inf}.
#'
#' @return A named list containing:
#' \itemize{
#'   \item \code{table_trips_that_use_links}: BigQuery table of trips filtered by selected links.
#'   \item \code{table_pro}: BigQuery table with processed link-order attributes.
#'   \item \code{turning_links}: Downloaded data frame of trip–link records.
#'   \item \code{link_selections_index_pro}: SQL-ready string of selected link IDs.
#' }
#'
#' @importFrom stringr str_glue
#' @importFrom bigrquery bq_table_download bq_table_nrow
#' @export
rplc_make_mvmnt_trip_subsets =  function(link_selections_df
                                         ,customer_name
                                         ,trip_table
                                         ,mode_type_pro
                                         ,logger
                                         ,page_size = NULL
                                         ,page_limit = Inf){

  link_selections_index_pro = paste0(
    "'", sort(unique(link_selections_df$stableEdgeId)), "'", collapse = ", ")

  message(stringr::str_glue("{gauntlet::strg_make_space_2()}Starting data acquisition process....."))

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

  info(logger, stringr::str_glue("queired_mvmnt_trips: {replica_temp_tbl_name(table_pro)}"))

  message(stringr::str_glue("Query resulted in {bigrquery::bq_table_nrow(table_pro)} trip/links identified via movement patterns"))

  #note:gaunt20260218 - removed this wanted to download after function
  # check_continue = gauntlet::robust_prompt_used("continue and download")
  # stopifnot("Aborted" = check_continue)
  #
  # turning_links = bigrquery::bq_table_download(
  #   table_pro, page_size = page_size, quiet = F, n_max = page_limit )

  return(
    list(
      table_trips_that_use_links = table_trips_that_use_links
      ,table_pro = table_pro
      # ,turning_links = turning_links
      ,link_selections_index_pro = link_selections_index_pro
    )
  )
}
