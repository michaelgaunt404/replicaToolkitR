#' Create Trip Table by Link Index
#'
#' This function retrieves trip data based on user-defined link selections and mode types.
#'
#' @param customer_name A character string specifying the BigQuery project name.
#' @param trip_table A character string specifying the BigQuery trip table name.
#' @param mode_type_pro A character vector specifying the mode types to filter trips (e.g., "COMMERCIAL", "PRIVATE_AUTO").
#' @param link_selections_index_pro A character vector specifying the user-selected link indices.
#'
#' @return A BigQuery result object containing trip data for the specified links and mode types.
#'
#' @examples
#' \dontrun{
#' # Example usage
#' result <- sql_createTipsByLinkIndex("your_project_name", "your_trip_table", c("COMMERCIAL", "PRIVATE_AUTO"), c(1, 2, 3))
#' }
#'
#' @importFrom bigrquery bq_project_query
#' @importFrom stringr str_glue
#'
#' @export
sql_createTipsByLinkIndex <- function(customer_name, trip_table, mode_type_pro, link_selections_index_pro) {
  message(stringr::str_glue("{gauntlet::strg_make_space_2()}\nFiltering for trips that use specified links...."))

  query <- stringr::str_glue("select * from (
    select * except(network_link_ids)
    ,ROW_NUMBER ()
        OVER (PARTITION BY activity_id) AS link_ord
    from `{trip_table}`, unnest(network_link_ids) as network_links
    where 1=1
    and mode in ({mode_type_pro})
    )
    where 1 = 1
    and network_links in ({link_selections_index_pro});")

  tmp_object <- bigrquery::bq_project_query(customer_name, query)

  message(stringr::str_glue("Completed{gauntlet::strg_make_space_2()}"))

  return(tmp_object)
}


