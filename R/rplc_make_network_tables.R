#' Create and validate Replica roadway network tables (convenience wrapper)
#'
#' This function is a convenience wrapper that bundles a set of commonly repeated
#' operations used across Replica network query functions. It:
#'
#' \enumerate{
#'   \item Creates a network table for the user-defined study area.
#'   \item Creates a linked highway count table associated with the network.
#'   \item Performs a consistency check between queried links and the returned
#'         highway count table, logging results for review.
#' }
#'
#' The function exists to reduce duplicate code across multiple query workflows
#' by providing a unified helper that builds the derived tables and applies the
#' standard link-checking logic.
#'
#' @param logger A logger object used to record diagnostic messages and results.
#'   Typically created earlier in the workflow using a project-specific logging
#'   utility.
#' @param wkt_object A well-known text (WKT) geometry object defining the spatial
#'   extent of the network query.
#' @param links_pro A vector or table of link IDs used as input for the network
#'   table creation and link-checking steps.
#' @param customer_name The name of the customer (Replica project namespace)
#'   containing the relevant BigQuery tables.
#' @param network_table The name of the primary roadway network table to be used
#'   as input when constructing the temporary network subset table.
#'
#' @return
#' A named list containing:
#' \itemize{
#'   \item \code{table_network}: The created network table returned by
#'     \code{sql_createNetworkTable()}.
#'   \item \code{highway_counts}: The link-level count table returned by
#'     \code{sql_createNetworkLinkCountTable()}.
#' }
#'
#' @details
#' Internally, this function calls:
#' \itemize{
#'   \item \code{sql_createNetworkTable()}
#'   \item \code{sql_createNetworkLinkCountTable()}
#'   \item \code{rplc_check_and_log_queired_links()}
#' }
#'
#' These three operations commonly co-occur in multiple Replica workflows; this
#' wrapper reduces duplication and helps enforce consistent logging and QC.
#'
#' @importFrom gauntlet robust_prompt_used
#' @export
#'
#' @examples
#' \dontrun{
#' logger <- rplc_create_logger()
#' study_area <- sf::st_as_text(sf::st_buffer(sf::st_point(c(-122.3, 47.6)), 0.01))
#'
#' out <- rplc_make_network_tables(
#'   logger = logger,
#'   wkt_object = study_area,
#'   links_pro = some_link_vector,
#'   customer_name = "customer_x",
#'   network_table = "full_network_table"
#' )
#'
#' str(out)
#' }
rplc_make_network_tables =  function(
    logger, wkt_object, links_pro, customer_name, network_table){

  table_network = sql_createNetworkTable(
    customer_name = customer_name,
    network_table = network_table,
    links_pro = links_pro,
    wkt_object = wkt_object
  )

  highway_counts = sql_createNetworkLinkCountTable(
    customer_name = customer_name,
    table_network = table_network
  )

  rplc_check_and_log_queired_links(
    counts_object = highway_counts,
    query_links = links_pro,
    logger_object = logger
  )

  return(
    list(
      table_network = table_network,
      highway_counts = highway_counts
    )
  )
}
