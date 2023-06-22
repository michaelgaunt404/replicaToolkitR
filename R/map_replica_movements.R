#' Maps discrete movement patterns from Replica
#'
#' This function takes a data frame and creates a map using the mapview package.
#' The map shows the distribution of the 'order' column values, grouped by the 'flag_grp' column values.
#'
#' @param data a data frame
#'
#' @return A map object created using mapview package
#'
#' @import dplyr
#' @import mapview
#' @import magritter
#'
#' @examples
#' map_replica_movements(data)
#'
#' @export
map_replica_movements <- function(data) {
  unique(data$flag_grp) %>%
    map(~{
      data %>%
        filter(flag_grp  == .x) %>%
        mutate(order = as.factor(order)) %>%
        mapview(zcol = "order", layer.name = .x
                ,label = "label"
                ,color = hcl.colors(5, palette = "viridis")
        )
    }) %>%
    reduce(`+`)

}
