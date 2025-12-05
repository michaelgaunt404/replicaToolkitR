#' Test Connection to Replica Tables Before Running a Larger Query
#'
#' This function tests the connection to the specified Replica network and trip tables
#' by attempting to download a row from each table. It ensures that the tables exist
#' and can be accessed before running a more substantial query. If the tables do not
#' exist or if there is an error during the connection attempt, an error message is provided.
#'
#' @param network_table Name of the Replica network table.
#' @param trip_table Name of the Replica trip table.
#' @param customer_name Name of the customer owning the Replica tables.
#'
#' @return Invisible. Throws an error if the connection to one or more tables fails.
#'
#' @details The function uses the \code{bigrquery} package to query the Replica network
#' and trip tables by attempting to download a single row from each table. It checks
#' the existence and accessibility of the tables before proceeding with a more extensive query.
#' If an error occurs during the connection attempt, an error message is displayed.
#'
#' @examples
#' # Example usage:
#' # replica_test_connection(network_table = "your_network_table",
#' #                         trip_table = "your_trip_table",
#' #                         customer_name = "your_customer_name")
#'
#' @import bigrquery
#' @import stringr
#'
#' @export
replica_test_connection = function(network_table = network_table
                                   ,trip_table = trip_table
                                   ,customer_name = customer_name){

  #NOTE: I think that this is now captured in checkValidTableConnections which is in the replica package
  #this script does need a replica_test_connection
#
#   test_outcome =
#     c(network_table,trip_table
#   ) %>%
#     map(~{
#       temp_table = .x
#
#       outcome = tryCatch({
#         table_check = bigrquery::bq_table_exists(temp_table)
#         return(table_check)
#       },  error = function(e) {
#         # error_message <- paste("An error occurred while querying the trip table:\n", e$message)
#
#         return(NA)
#       })
#
#     })
#
#   stopifnot("One or more of the Replica tables could not be connected to.... \nCheck Replica table inputs" = (any(is.na(unlist(test_outcome))) | any(unlist(test_outcome) == F))  == F)
}
