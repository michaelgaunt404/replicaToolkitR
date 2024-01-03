#' Movement Pattern VMT Summary
#'
#' This function aggregates unique trips along a specific movement pattern and sequence number
#' by linking two data frames: link_selections_df and replica_queried_network. It calculates
#' vehicle miles traveled (VMT) per mode and vehicle type based on movement and movement sequence.
#'
#' @param link_selections_df A data frame containing movement pattern and sequence information.
#' @param replica_queried_network A data frame containing queried network information.
#' @param location The location or directory where data files are stored.
#' @param folder The folder within the specified location containing necessary data files.
#' @param include_mode_veh_type A logical indicating whether to include mode and vehicle type in the output.
#'
#' @return A data frame summarizing VMT per mode and vehicle type based on movement and movement sequence.
#'
#' @examples
#' \dontrun{
#' # Example usage
#' result_summary <- mvmnt_pattern_vmt_smmry(link_selections_df, replica_queried_network, "your_location", "your_folder", include_mode_veh_type = TRUE)
#' }
#'
#' @importFrom data.table fread
#' @importFrom dplyr select mutate arrange group_by summarise
#' @importFrom here here
#'
#' @export
mvmnt_pattern_vmt_smmry = function(
    link_selections_df = NULL
    ,replica_queried_network = NULL
    ,replica_trip_mvmnt_seq_table = NULL
    ,location
    ,folder
    ,include_mode_veh_type = TRUE
    ,auto_save = F){

  if (is.null(link_selections_df)){
    message(str_glue("Variable _{deparse(substitute(link_selections_df))}_ is NULL or not provided.....\nWill use provided location/folder to load...."))
    link_selections_df = data.table::fread(
      here::here(location, folder, "link_selections_df.csv"))
  } else {
    message(str_glue("Variable _{deparse(substitute(link_selections_df))}_ is not NULL, will use instead of loading from the directory"))
  }

  if (is.null(replica_queried_network)){
    message(str_glue("Variable _{deparse(substitute(replica_queried_network))}_ is NULL or not provided.....\nWill use provided location/folder to load...."))
    replica_queried_network = data.table::fread(
      here::here(location, folder, "replica_queried_network.csv"))
  } else {
    message(str_glue("Variable _{deparse(substitute(replica_queried_network))}_ is not NULL, will use instead of loading from the directory"))
  }

  if (is.null(replica_trip_mvmnt_seq_table)){
    message(str_glue("Variable _{deparse(substitute(replica_trip_mvmnt_seq_table))}_ is NULL or not provided.....\nWill use provided location/folder to load...."))
    replica_trip_mvmnt_seq_table = data.table::fread(
      here::here(location, folder, "replica_trip_mvmnt_seq_table.csv"))
  } else {
    message(str_glue("Variable _{deparse(substitute(replica_trip_mvmnt_seq_table))}_ is not NULL, will use instead of loading from the directory"))
  }

  link_selections_df_pro = link_selections_df %>%
    merge(., replica_queried_network %>%
            select(stableEdgeId, streetName, distance, highway)
          ,by.x = "value", by.y = "stableEdgeId", all.x = TRUE) %>%
    mutate(distance_mm = distance
           ,distance_ft = distance/304.8
           ,distance_mile = distance_ft/5280)  %>%
    select(mvmnt,  sequence, streetName, index_sel_seq, value
           ,everything()) %>%
    rename(network_links = value
           # ,mvmnt = intersection
           ,mvmnt_seq = sequence) %>%
    arrange(mvmnt, mvmnt_seq, index_sel_seq) %>%
    group_by(mvmnt, mvmnt_seq) %>%
    summarise(distance_mile_ttl = sum(distance_mile)
              ,ttl_links_used = n(), .groups = "drop")


  if(include_mode_veh_type) {
    cols_index_agg = c("mvmnt",  "mvmnt_seq", "mode", "vehicle_type")
  } else {
    cols_index_agg = c("mvmnt",  "mvmnt_seq")
  }

  agg_trips_mvmnt = replica_trip_mvmnt_seq_table %>%
    select(mvmnt, mvmnt_seq, activity_id, mode, vehicle_type) %>%
    unique() %>%
    mutate(count = 1) %>%
    group_by(across({{cols_index_agg}})) %>%
    summarise(count = sum(count), .groups = "drop")

  tmp_output = agg_trips_mvmnt %>%
    merge(link_selections_df_pro
          ,by = c("mvmnt", "mvmnt_seq")) %>%
    mutate(vmt = round(count * distance_mile_ttl, 2) )

  if (auto_save) {
    write.csv(tmp_output, here::here(location, folder, "mvmnt_pattern_vmt_smmry.csv"))
  }

  return(tmp_output)

}
