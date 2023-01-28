
#' Make new **Query and Process Replica** script from template.
#'
#' @description This function copies an R script template from this package and saves it to usesr specified location.
#' The template is a basic workflow for acquiring and processing replica data.
#'
#' @param folder a string indicating where this file should be saved.
#' @param file_name a string name (include .r extension)
#'
#' @return a script in the location and with the name choosen by the user
#' @export
#'
#' @examples
#' #none
make_query_and_process_replica_script = function(folder = "code", file_name = NULL){
  folder_location = here::here(folder)
  file_location = here::here(stringr::str_glue("{folder}/{file_name}"))

  stopifnot("Please provide a filename (include .r extentsion)..." = !is.null(file_name)
            ,"Folder location does not exist, please make it first..." = !exists(folder_location))

  file.copy(
    system.file("scripts", "template_query_and_process_replica.r", package="replicaToolkitR")
    ,file_location
  )

  file.edit(file_location)
}


