#' Visualize Replica Study Regions
#'
#' This convenience function loads the \code{replica_states_region} dataset and
#' displays it in an interactive map using \code{mapview}, colored by
#' \code{replica_region}.
#'
#' @return An interactive \code{mapview} object.
#' @importFrom mapview mapview
#' @export
#'
#' @examples
#' \dontrun{
#' viz_rplc_study_regions()
#' }
viz_rplc_study_regions = function(){

  utils::data(replica_states_region)

  mapview::mapview(replica_states_region, z = "replica_region")
}


