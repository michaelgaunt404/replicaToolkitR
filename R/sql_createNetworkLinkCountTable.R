#' Query the count of network links by highway type.
#'
#' This function generates a BigQuery query to calculate the count of network
#' links by highway type using a temporary table created previously.
#'
#' @param customer_name The name of the BigQuery customer project.
#' @param table_network The name of the temporary table containing network data.
#'
#' @return A BigQuery result containing the count of network links by highway type.
#'
#' @examples
#' queryNetworkLinkCount("your_project_name", "your_temp_table_name")
#'
#' @export
sql_createNetworkLinkCountTable <- function(customer_name, table_network = table_network) {

  query <- stringr::str_glue("SELECT highway, count(*) as count
                            from `{replica_temp_tbl_name(table_network)}`
                            group by highway
                            order by count;")

  table_network_link_count = bigrquery::bq_project_query(customer_name, query)

  temp = bigrquery::bq_table_download(table_network_link_count)
  return(temp)
}




