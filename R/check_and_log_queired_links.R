#' Check and log information about the number of unique links per Highway type.
#'
#' This function performs checks on the provided data to ensure that the number
#' of unique links is not zero and logs information accordingly.
#'
#' @param counts_object A data frame containing the count of unique links per
#'   highway type.
#' @param query_links A vector of requested link types.
#' @param logger_object A logger_object object for logging messages.
#'
#' @return None.
#'
#' @export
check_and_log_queired_links = function(counts_object, query_links, logger_object) {
  if (nrow(counts_object) == 0) {
    mes_fatal <- "Number of returned links was zero\n...change bounding box\n...stopping...."
    log4r::fatal(logger_object, mes_fatal)
    stopifnot("Number of returned links was zero\n...change bounding box\n...stopping...." = FALSE)
  } else {
    message <- str_glue(
      "{strg_make_space_2()}Non-empty data returned, Good\nQuery continued...\nIn total {sum(counts_object$count)} links\n",
      str_glue('-{counts_object$count} ({100 * gauntlet::dgt2(counts_object$count / sum({counts_object$count}))})% {counts_object$highway}') %>%
        paste0(collapse = '\n')
    )
    message %>% gauntlet::log_and_info(., logger_object)

    index_empty_highways <- query_links[-which(query_links %in% counts_object$highway)]

    if (length(index_empty_highways) == 0) {
      gauntlet::log_and_info(stringr::str_glue("All requested link types returned with some number of links\nnone empty.... GOOD"), logger_object)
    } else {
      gauntlet::log_and_warn(
        str_glue("{strg_make_space_2()}The following link types were requested but not found in the bounding box:\n{paste0(index_empty_highways, collapse = '\n')}"),
        logger_object
      )
    }
  }

  # log4r::info(logger_object, stringr::str_glue("table_network: df{replica_temp_tbl_name(table_network)}"))

  if (gauntlet::robust_prompt_used("to continue the function execution") == FALSE) {
    log4r::warn(logger_object, "You have elected to terminate function run...")
    stopifnot("Terminating..." = FALSE)
  }
}
