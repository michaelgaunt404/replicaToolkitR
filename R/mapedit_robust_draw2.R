#' Robust interactive geometry drawing using `mapedit`
#'
#' This function provides a robust wrapper around \code{mapedit::drawFeatures()},
#' allowing a user to interactively draw a geometry and review or repeat the process
#' before returning a final \code{sf} object.
#'
#' The function repeatedly prompts the user until a valid geometry is created.
#' If the user accidentally closes the drawing tool without selecting **Finish**,
#' or produces \code{NULL}, the function offers an option to restart the process.
#' After a successful geometry is drawn, the user may visually review it
#' (via \code{mapview::mapview()}) and optionally redraw the geometry.
#'
#' @details
#' This function:
#' \enumerate{
#'   \item Launches an interactive map using \code{mapedit::drawFeatures()}.
#'   \item Catches errors (e.g., forgetting to click "Finish") and prompts the user
#'   to retry.
#'   \item Transforms the resulting geometry to EPSG 4326.
#'   \item Prompts the user to review the geometry and optionally redraw it.
#' }
#'
#' Internally, the function uses \code{gauntlet::robust_prompt_used()} to manage
#' user prompts and yes/no logic.
#'
#' @return
#' An \code{sf} object representing the user-drawn geometry, transformed to
#' EPSG 4326.
#'
#' @importFrom mapedit drawFeatures
#' @importFrom sf st_transform
#' @importFrom mapview mapview
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Run the interactive geometry tool
#' geom <- mapedit_robust_draw2()
#'
#' # View returned geometry
#' print(geom)
#' }
mapedit_robust_draw2 = function(){
  check_repeat = TRUE


  while (check_repeat) {
    user_created_geometry = tryCatch({
      mapedit::drawFeatures() %>% sf::st_transform(4326)

    }, error = function(e) {
      message("User geometry NA!!!!")
      message(paste("Error occurred with shape generation:\n",
                    e$message))
      message("Likely that you did not press ''Finish'' before pressing ''Done''")
      return(NA)
    })

    if (is.null(user_created_geometry)) {
      check_repeat = gauntlet::robust_prompt_used("repeat the shape generation process again")
    }
    else {
      message("User geometry not NA - good\n")
      check_review = gauntlet::robust_prompt_used("review your geometry (Y) or continue (N)")
      check_repeat = check_review
      if (check_review) {
        mapview::mapview(user_created_geometry) %>% print()
        check_repeat = gauntlet::robust_prompt_used("repeat the shape generation process again")
      }
    }

  }
  return(user_created_geometry)
}
