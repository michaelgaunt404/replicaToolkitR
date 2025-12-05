#' Function to download sample network and trip tables, perform validity checks, and notify users of errors.
#'
#' This function downloads a sample of both network and trip tables, puts them into temporary tables, and checks for errors in the data.
#'
#' @param prefix_origin The prefix for the origin location columns.
#' @param prefix_dest The prefix for the destination location columns.
#' @param network_table The network table to be downloaded.
#' @param trip_table The trip table to be downloaded.
#'
#' @return This function performs checks on the downloaded data, and if errors are found, it will stop the function and notify the user.
#'
#' @details The function first downloads sample data from both the network and trip tables, setting the maximum number of records to 1. It then checks whether the downloaded trip table contains valid column names based on the provided prefixes for origin and destination, followed by "_lat" and "_lng".
#'
#' If either of the downloaded tables has no records, the function will flag an error, stop the execution, and notify the user.
#'
#' Additionally, the function checks whether the column names derived from the prefixes for origin and destination are valid call names in the temporary data frame.
#'
#' @importFrom bigrquery bq_table_exists bq_table_download
#' @importFrom stringr str_glue
#' @importFrom gauntlet strg_make_space_2
#'
#' @export
#'
#' @examples
#' \dontrun{
#' prefix_origin <- "origin"
#' prefix_dest <- "destination"
#' network_table <- "your_network_table_name"
#' trip_table <- "your_trip_table_name"
#' download_and_check(prefix_origin, prefix_dest, network_table, trip_table)
#' }
rplc_checkValidTableConnections <- function(prefix_origin, prefix_dest, network_table, trip_table) {

  message(stringr::str_glue("{gauntlet::strg_make_space_2()}Perfroming Replica connection test\nValidating user supplied inputs"))


  if (!bigrquery::bq_table_exists(network_table) || !bigrquery::bq_table_exists(trip_table)) {
    stop("Error: One or more of the specified network or trip tables does not exist")
  }

  temp_network_table_dl <- bigrquery::bq_table_download(network_table, n_max = 1)
  temp_trip_table_dl <- bigrquery::bq_table_download(trip_table, n_max = 1)

  if (nrow(temp_network_table_dl) == 0 || nrow(temp_trip_table_dl) == 0) {
    stop("Error: Downloaded table(s) have no records. Either the network, trip or both tables were incorrectly specified")
  }

  valid_origin_col <- paste0(prefix_origin, c("_lat", "_lng"))
  valid_dest_col <- paste0(prefix_dest, c("_lat", "_lng"))

  if (!all(valid_origin_col %in% colnames(temp_trip_table_dl)) ||
      !all(valid_dest_col %in% colnames(temp_trip_table_dl))) {
    stop("Error: Invalid column names in the downloaded trip table.")
  }

  message(stringr::str_glue("Table connections validated"))
}
