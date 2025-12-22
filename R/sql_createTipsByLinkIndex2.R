#' Create a BigQuery query filtering trips by selected network links
#'
#' Constructs and runs a BigQuery SQL query that filters trips to those
#' that traverse one or more specified network link indices. The query
#' unnests link-level data, filters by travel mode and selected links,
#' and assigns an ordinal position to each link within an activity.
#'
#' @details
#' The function assumes that \code{mode_type_pro} and
#' \code{link_selections_index_pro} are already formatted as SQL-ready
#' value lists (e.g., \code{'1, 2, 3'} or \code{'\"WALK\", \"BIKE\"'}).
#' No additional escaping or validation is performed.
#'
#' @param customer_name Character string; BigQuery project ID.
#' @param trip_table Character string; fully qualified BigQuery table containing trip data.
#' @param mode_type_pro Character string; SQL-ready list of mode values to filter on.
#' @param link_selections_index_pro Character string; SQL-ready list of network link indices.
#'
#' @return A \code{bigrquery::bq_project_query} object.
#'
#' @importFrom stringr str_glue
#' @importFrom bigrquery bq_project_query
#' @export
#' @example
#' \dontrun{
#' #none
#' }
sql_createTipsByLinkIndex2 = function(customer_name, trip_table, mode_type_pro, link_selections_index_pro) {
  message(stringr::str_glue("{gauntlet::strg_make_space_2()}\nFiltering for trips that use specified links...."))

  query <- stringr::str_glue("select
*
,ROW_NUMBER () OVER (PARTITION BY activity_id) AS link_ord from (
select * from (
select * except(network_link_ids, geometry, transit_route_ids)
from (
select *
from `{trip_table}`
where 1=1
and mode in ({mode_type_pro})
) as foo, unnest(network_link_ids) as network_links) as foo3
where 1=1
and network_links in ({link_selections_index_pro})
) as foo4;")

  tmp_object <- bigrquery::bq_project_query(customer_name, query)

  message(stringr::str_glue("Completed{gauntlet::strg_make_space_2()}"))

  return(tmp_object)
}
