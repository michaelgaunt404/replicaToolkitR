#' Check and Download a BigQuery Table
#'
#' This helper function checks whether the user wants to download a BigQuery table,
#' and if confirmed, downloads the table using `bigrquery`, saves it to a specified folder
#' in `.csv`, `.qs`, or both formats, and optionally returns the downloaded object.
#'
#' @param bq_table A `bigrquery` table reference object.
#' @param max_record The maximum number of records to download from the table.
#' @param table_name A character string for display in the message indicating the table name.
#' @param folder A character string specifying the folder path to save the downloaded file(s).
#' @param file_name A character string specifying the base file name (without extension).
#' @param return_obj Logical. If `TRUE`, the downloaded table is returned as an object.
#' @param write_format A character string indicating the file format(s) to write.
#'   One of `"csv"`, `"qs"`, or `"both"`.
#'
#' @return If `return_obj = TRUE`, returns a data frame containing the downloaded BigQuery table.
#' Otherwise, returns `NULL`.
#'
#' @details This function is designed to prompt the user interactively before downloading large
#' BigQuery tables. It utilizes `gauntlet::robust_prompt_used()` to ensure that the user
#' explicitly confirms the download action. The function supports saving the data as CSV and/or
#' a fast binary `.qs` file using `qs::qsave()`.
#'
#' @importFrom bigrquery bq_table_nrow bq_table_download
#' @importFrom stringr str_glue
#' @importFrom here here
#' @importFrom gauntlet robust_prompt_used
#' @importFrom qs qsave
#'
#' @export
#'
#' @examples
#' \dontrun{
#' bq_ref <- bigrquery::bq_table("my_project", "my_dataset", "my_table")
#' check_to_download_bqtable(
#'   bq_table = bq_ref,
#'   max_record = 10000,
#'   table_name = "my_table",
#'   folder = "data/output",
#'   file_name = "my_table",
#'   return_obj = TRUE,
#'   write_format = "both"
#' )
#' }
check_to_download_bqtable <- function(
    bq_table,
    max_record,
    table_name,
    folder,
    file_name,
    return_obj,
    write_format = c("csv", "qs", "both")) {
  write_format <- match.arg(write_format)

  temp_nrow <- bigrquery::bq_table_nrow(bq_table)
  message(stringr::str_glue("{gauntlet::strg_make_space_2()}There are {temp_nrow} records in the table:\n{table_name}"))

  dl_check <- gauntlet::robust_prompt_used("download this table")

  tryCatch({
    if (dl_check) {
      message("User elected to download this table")


      bq_table <- bigrquery::bq_table_download(bq_table, n_max = max_record
                                               ,page_size = 2000)

      path_base <- here::here(folder, file_name)

      if (write_format %in% c("csv", "both")) {
        write.csv(bq_table, file = paste0(path_base, ".csv"), row.names = FALSE)
      }

      if (write_format %in% c("qs", "both")) {
        qs::qsave(bq_table, file = paste0(path_base, ".qs"))
      }

      if (return_obj) {
        return(bq_table)
      }

    } else {
      message("User did not elect to download this table")
      return(NULL)
    }

  }, error = function(err) {
    message("An error orccured....")
    message("Manually grab the following table::")
    message(paste0(bq_table[-4],collapse = "."))

    print(err$message)
  })


}
