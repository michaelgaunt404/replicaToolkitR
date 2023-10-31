#' Extract project name, dataset, and tablename from google big query object.
#'
#' When BQuery objects (tables) are made using bq_project_query() they are given temporary dataset and table names.
#' This function extracts all of a table's identifying elements and glues them into a singular string.
#' This string can be used in subsequent queires.
#'
#' Function is helpful when making mulitstepped queries where temporary tables are created and subsequently interacted with.
#'
#' @param object
#'
#' @return a character string
#' @export
#'
#' @examples
#'
#' temp = list(username = "replica_fake_name"
#',dataset = "replica_temp_dataset"
#',table = "replica_temp_table")
#'
#'replica_temp_tbl_name(temp)
replica_temp_tbl_name = function(object) {
  str_glue("{object[[1]]}.{object[[2]]}.{object[[3]]}")
}


