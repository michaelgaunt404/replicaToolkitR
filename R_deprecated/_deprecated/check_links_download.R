
#LIKELY DEPRECATED
#WAS ONLY IN BBOX QUERY AT THE OTHER END BUT NOT PERFROMED



#' Automatically check which Replica links have volumes associated with them.
#'
#' @description This function compares the network links found in the raw network link volume table with those in the acquired Repllica network.
#'
#' This function is ran automatically at the end of the query function but can be ran manually as well.
#'
#' @param location character string pointing to top level location where data acquired from Google was saved to.
#' @param folder character string of name where data was automatically saved to from google data download.
#'
#' @return a returned object detailing which network links were acquired but do not have volumes associated with them.
#' @export
#'
#' @examples
#'
#' #none
check_links_download = function(location, folder){
  # location = "data/req_dev"
  # folder = "data_20230112_143236"
#
#   table_agg_by_link_subset_limited = here::here(location, folder, "table_agg_by_link_subset_limited.csv") %>%
#     data.table::fread()
#
#   check_subset_agg = length(unique(table_agg_by_link_subset_limited$network_link_ids_unnested))
#
#   replica_queried_network = here::here(location, folder, "replica_queried_network.csv") %>%
#     data.table::fread()
#
#   check_subset = length(unique(replica_queried_network$stableEdgeId))
#
#   merge_check = replica_queried_network %>%
#     select(stableEdgeId, streetName, highway) %>%
#     unique() %>%
#     mutate(stableEdgeId = stringr::str_trunc(stableEdgeId, 14, "right", "")) %>%
#     merge(table_agg_by_link_subset_limited %>%
#             select(network_link_ids_unnested) %>%
#             mutate(flag_merge = 1
#                    ,network_link_ids_unnested = stringr::str_trunc(network_link_ids_unnested, 14, "right", "")) %>%
#             unique()
#           ,by.x = 'stableEdgeId', by.y = "network_link_ids_unnested", all = T)
#
#   str_glue("The replica network link volume table returned {check_subset_agg} unique network links\nThe network subset download returned {check_subset} unique network links\n{gauntlet::strg_make_space_2()}{100*dgt3(check_subset_agg/check_subset)}% of queried links have trip counts associated with them\n{gauntlet::strg_make_space_2()}") %>%
#     message()
#
#   message(
#     stringr::str_glue("It is expected that not all links will have trips assocaited with them, this is because
#          1) Links with less volume than the provided threshold were removed
#          2) Links may not have any trips asscoiated with them by default
#          3) The Replica network may occiasionally contain spurious links that are doubled, these do not have any volume"))
#
#   return(merge_check)
}

