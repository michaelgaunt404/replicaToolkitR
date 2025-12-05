#' Prompt User for Jitter Factor
#'
#' This function prompts the user to enter a jitter factor value between 0.00001
#' and 0.01. The function accepts the user's input and handles invalid input by
#' prompting the user again.
#'
#' @return A numeric value indicating the jitter factor entered by the user.
#'
#' @export
#' @examples
#' \dontrun{
#' rplc_jitter_factor_prompt()
#' }
rplc_jitter_factor_prompt <- function() {
  while(TRUE) {
    jitter <- as.numeric(readline(prompt = "Provide jitter value: "))
    if(!is.na(jitter) && is.numeric(jitter) && jitter >= 0.00001 && jitter <= 0.01) {
      return(jitter)
    }
    else {
      cat("Invalid input. Jitter value must be a number between 0.00001 and 0.01.\n")
    }
  }
}
