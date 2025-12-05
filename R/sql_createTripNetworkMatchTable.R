#' Query distinct activity IDs based on network link matching.
#'
#' This function generates a BigQuery query to obtain distinct activity IDs
#' where network link IDs match those in the temporary network table.
#'
#' @param customer_name The name of the BigQuery customer project.
#' @param trip_table The name of the trip data table in BigQuery.
#' @param table_network The name of the temporary table containing network data.
#' @param mode_type_pro A vector of mode types to filter the data.
#'
#' @return A BigQuery result containing distinct activity IDs.
#'
#' @export
#' @examples
#' \dontrun{
#' # none
#'
#' }
sql_createTripNetworkMatchTable <- function(customer_name
                                        ,trip_table = trip_table
                                        ,mode_type_pro = mode_type_pro
                                        ,table_network = table_network) {
  message(stringr::str_glue("{gauntlet::strg_make_space_2()}Filtering trips that only use queried network now....\nStep involves unnesting trips' links and then using table subset to filter on"))


  query <- stringr::str_glue("select distinct activity_id
    from (
    select distinct activity_id, network_link_ids
    from
    (select *
    from `{trip_table}`
    where mode in ({mode_type_pro})
    ), unnest(network_link_ids) as network_link_ids
    )
    where
    1 = 1
    and network_link_ids in (select stableEdgeId from `{replica_temp_tbl_name(table_network)}`);")

  table_trip_network_match <- bigrquery::bq_project_query(customer_name, query)

  message(stringr::str_glue("Completed{strg_make_space_2()}"))

  return(table_trip_network_match)
}
