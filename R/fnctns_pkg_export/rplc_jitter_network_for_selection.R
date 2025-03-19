#' Jitter Factor Adjustment for Network Link Visualization
#'
#' This function facilitates an interactive process to help users select an appropriate jitter factor
#' for visualizing links in a spatial network dataset. The function iteratively applies a specified jitter
#' factor to the data, displaying it each time for user inspection. The final output is the network data
#' with the user-selected jitter factor applied.
#'
#' @param table_network_data_sf An `sf` object containing the network data to be visualized.
#' @param jitter_factor A numeric value representing the initial jitter factor to apply for spatial adjustments.
#' @param logger A `log4r` logger object for logging the jitter selection process.
#' @return An `sf` object with the final jitter factor applied, prepared for later network selection.
#' @details
#' This function is designed to assist users in visualizing spatial network data with a jitter factor
#' to ensure appropriate visual separation of overlapping links. Users can iteratively adjust the jitter factor
#' until they achieve a satisfactory visualization. Once finalized, this adjusted dataset can be used for
#' subsequent network selection steps. The function utilizes `mapview` to display the network data interactively
#' and `log4r` for logging key steps in the jitter selection process.
#'
#' @examples
#' \dontrun{
#' table_network_data_sf <- read_sf("path/to/your/network_data.shp")
#' initial_jitter_factor <- 0.001
#' logger <- log4r::logger()
#' final_network <- rplc_jitter_network_for_selection(table_network_data_sf, initial_jitter_factor, logger)
#' }
#'
#' @export
rplc_jitter_network_for_selection <- function(table_network_data_sf, jitter_factor, logger) {
  # Initial messages and logging
  message(str_glue("{gauntlet::strg_make_space_2()}Starting jitter factor selection process..."))
  log4r::info(logger, "started jitter factor selection")
  message(str_glue("The network you queried will be displayed with a {jitter_factor} jitter factor..."))

  # Apply initial jitter to the network data
  table_network_data_sf_jit <- table_network_data_sf %>%
    st_jitter(factor = jitter_factor)

  # Display initial map
  mapview(table_network_data_sf_jit, zcol = "flags", burst = TRUE) %>% print()

  # Rejitter loop for interactive adjustment
  rejitter <- TRUE
  while (rejitter) {
    rejitter <- robust_prompt_used("change the jitter factor and rejitter")

    if (rejitter) {
      message(str_glue("Previous jitter used was {jitter_factor}"))

      # Prompt for new jitter factor
      jitter_factor <- prompt_jitter_factor()

      # Apply updated jitter factor
      table_network_data_sf_jit <- table_network_data_sf %>%
        st_jitter(factor = jitter_factor)

      # Display updated map
      mapview(table_network_data_sf_jit, zcol = "flags", burst = TRUE) %>% print()
    }
  }

  # Return the final jittered network data
  return(table_network_data_sf_jit)
}
