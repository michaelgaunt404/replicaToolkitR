# #' Query Replica's roadway network given a user defined map extent
# #'
# #' This function prompts the user to draw a study area using the \code{mapedit::drawFeatures()} function, and returns a data frame of the roadway network data that intersects with the study area.
# #'
# #' @param network_table The name of the table containing the roadway network data to be queried.
# #' @param customer_name The name of the customer whose project contains the \code{network_table} table.
# #'
# #' @return A data frame of the roadway network data that intersects with the study area.
# #'
# #' @import mapedit
# #' @importFrom stringr str_glue
# #' @importFrom wellknown sf_convert
# #' @importFrom bigrquery bq_project_query bq_table_nrow bq_table_download
# #' @importFrom dplyr arrange
# #' @importFrom rlang parse_expr
# #' @importFrom sf st_transform st_union
# #' @export
# #'
# #' @examples
# #' \dontrun{
# #' view_replica_study_area_network("network_table", "customer_name")
# #' }
view_replica_study_area_network = function(network_table
                                           ,customer_name
                                           ,links_pro = NA
                                           ){
  # message(str_glue("{gauntlet::strg_make_space_2()}Please draw a study area that will be used to query Replica's roadway network..."))
  # message("Draw it as small and parsimonious as possible")
  # message(str_glue("You can draw mulitple, discrete objects if you wish{gauntlet::strg_make_space_2(last = F)}"))
  #
  # limit_links = robust_prompt_used("limit the number of links using the links supplied links_pro")
  #
  # if (limit_links) {
  #   if (is.na(links_pro)) {
  #     message("User did not supply a links_pro input....")
  #     continue_response <- robust_prompt_used("would you like to continue without limiting the number of links")
  #     if (continue_response) {
  #       links_response = c('highway','corridor','road'
  #                          ,'motorway','motorway_link'
  #                          ,'trunk','trunk_link'
  #                          ,'primary','primary_link'
  #                          ,'secondary','secondary_link'
  #                          ,'tertiary','tertiary_link') %>%
  #         paste0("'", ., "'", collapse = ", ")
  #     } else {
  #       stop("Execution terminated by user.")
  #     }
  #   } else {
  #     links_response = links_pro
  #   }
  # } else {
  #   links_response = c('highway','corridor','road'
  #                      ,'motorway','motorway_link'
  #                      ,'trunk','trunk_link'
  #                      ,'primary','primary_link'
  #                      ,'secondary','secondary_link'
  #                      ,'tertiary','tertiary_link') %>%
  #     paste0("'", ., "'", collapse = ", ")
  # }
  #
  # # study_area = mapedit::drawFeatures() %>% st_transform(4326)
  # study_area = gauntletMap::mapedit_robust_draw()
  # study_area_wkt = wellknown::sf_convert(st_union(study_area))
  #
  # table_network = bigrquery::bq_project_query(
  #   customer_name,
  #   stringr::str_glue("select * from (
  #                     select *,
  #                     ST_INTERSECTS(
  #                     ST_GEOGFROMTEXT('{study_area_wkt}')
  #                     ,geometry) as flag_contains
  #                     from `{network_table}`
  #                     where highway in ({links_response})
  #                     )
  #                     where flag_contains = TRUE"))
  #
  # table_network_count = bigrquery::bq_table_nrow(table_network)
  #
  # message(str_glue("{table_network_count} links were returned... \nWould you like to continue query execution or abort run completely..."))
  # check_continue = robust_prompt_used("continue")
  # stopifnot("Aborted" = check_continue)
  #
  # table_network_data = bigrquery::bq_table_download(table_network
  #                                                   ,page_size = 1000) %>%
  #   arrange(stableEdgeId)
  #
  # return(
  #   list(table_network = table_network
  #        ,table_network_data = table_network_data)
  # )
}
