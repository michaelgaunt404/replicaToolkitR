#' Load Replica data file (CSV or QS)
#'
#' This helper function loads a Replica dataset saved either as a CSV
#' (`fread`) or QS file (`qs::qread`). The function checks that the file exists
#' before attempting to load it.
#'
#' @param location Top-level directory where Replica data is stored.
#' @param folder Subfolder within \code{location} containing the data download.
#' @param file_stem Base name of the file (default:
#'   \code{"replica_trip_origin_destination"}).
#' @param flag_qs Logical; if TRUE, loads the `.qs` version. If FALSE (default),
#'   loads the `.csv` version.
#'
#' @return A data.frame or list, depending on file contents.
#' @export
#'
#' @examples
#' \dontrun{
#' postprocess_load_replica_file("data/req_dev", "data_20230111_150315")
#' postprocess_load_replica_file("data/req_dev", "data_20230111_150315", flag_qs = TRUE)
#' }
postprocess_load_replica_file = function(location = location, folder = folder,
                              file_stem = "replica_trip_origin_destination",
                              flag_qs = FALSE) {

  message(str_glue("Reading from {location}/{folder}"))

  ext <- if (flag_qs) ".qs" else ".csv"
  file_path <- here::here(location, folder, paste0(file_stem, ext))

  if (!file.exists(file_path)) {
    stop(
      sprintf("File not found:\n%s\n(flag_qs = %s)", file_path, flag_qs),
      call. = FALSE
    )
  }

  if (flag_qs) {
    message("Loading QS file: ", file_path)
    return(qs::qread(file_path))
  } else {
    message("Loading CSV file: ", file_path)
    return(data.table::fread(file_path))
  }
}
