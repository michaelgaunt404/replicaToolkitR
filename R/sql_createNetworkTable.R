#' Create a temporary BigQuery table for network data.
#'
#' This function generates a BigQuery query to create a temporary table by
#' selecting network data based on a study area defined by geometry and link types.
#'
#' @param customer_name The name of the BigQuery customer project.
#' @param network_table The name of the network data table in BigQuery.
#' @param links_pro A vector of link types to filter the data.
#' @param list_wkt_objects A list of well-known text (WKT) geometry objects
#'   used to define the study area.
#'
#' @return A temporary BigQuery table containing filtered network data.
#'
#' @examples
#' createNetworkTable("your_project_name", "your_network_table", c("highway", "street"), list_of_wkt_objects)
#'
#' @export
createNetworkTable <- function(customer_name
                               ,network_table = network_table
                               ,links_pro = links_pro
                               ,wkt_object) {
  message(stringr::str_glue("{make_space()}\nCreating network table now...."))

  query <- stringr::str_glue("select * from (
    select *,
    ST_INTERSECTS(
      ST_GEOGFROMTEXT('{wkt_object}'),
      geometry
    ) as flag_contains
    from `{network_table}`
    where highway in ({links_pro})
  )
  where flag_contains = TRUE")

  table_network <- bigrquery::bq_project_query(customer_name, query)
  return(table_network)
}
