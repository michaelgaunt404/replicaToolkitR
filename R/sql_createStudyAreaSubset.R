#' Create Study Area Subset with Spatial Index
#'
#' This function creates a study area subset using a spatial index. It queries a BigQuery table to select records where the geometry intersects with the provided WKT geometry.
#'
#' @param wkt_geometry The Well-Known Text (WKT) geometry for the study area.
#'
#' @return A BigQuery result containing the study area subset.
#'
#' @examples
#' wkt_geometry <- "POLYGON((x1 y1, x2 y2, x3 y3, x4 y4, x1 y1))"
#' study_area_subset <- create_study_area_subset(wkt_geometry)
#'
#' @export
sql_createStudyAreaSubset <- function(customer_name, wkt_geometry) {
  message(stringr::str_glue("{strg_make_space_2()}Creating study area subset now...."))

  query <- stringr::str_glue("select * from (
      select *,
      ST_INTERSECTS(
        ST_GEOGFROMTEXT('{wkt_geometry}'),
        surface_point
      ) as flag_contains
      from `Geos.bgrp`
    )
    where flag_contains = TRUE")

  table_sa_poly_index <- bigrquery::bq_project_query(customer_name, query)

  return(table_sa_poly_index)
}
