#' Query Replica movement patterns using link gates
#'
#' @description
#' Queries Replica BigQuery tables to extract movement patterns through
#' user-defined network link "gates". The workflow builds temporary
#' network subsets, filters trips by mode and link sequence, and optionally
#' downloads auxiliary demographic and trip-chain tables. Outputs are written
#' to a timestamped folder with structured logs.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Constructs BigQuery table references for network, trip, and population tables.
#'   \item Validates access to required Replica tables.
#'   \item Creates a timestamped output directory and initializes logging.
#'   \item Converts boundary geometries to WKT for spatial filtering.
#'   \item Builds temporary network tables filtered by highway class.
#'   \item Subsets trips passing through ordered link gates.
#'   \item Downloads queried tables to disk (CSV and/or QS).
#'   \item Optionally extracts population demographics and all trips
#'   for matched persons.
#' }
#'
#' @param bb_network_layer An \code{sf} polygon or multipolygon defining the
#'   spatial boundary used to filter Replica network segments.
#'
#' @param query_links Character vector of highway or roadway classes used
#'   to filter the network table.
#'
#' @param mode_type Character vector of Replica travel mode values used
#'   to filter trips.
#'
#' @param data_set_location Character string identifying the Replica dataset
#'   location (e.g., \code{"seattle"}).
#'
#' @param data_set_period Character string identifying the dataset period
#'   (e.g., \code{"2024"} or \code{"2024q1"}).
#'
#' @param data_set_day Character string identifying the trip-day subset
#'   (e.g., \code{"weekday"}).
#'
#' @param customer_name Character. BigQuery project or customer name.
#'
#' @param prefix_origin Character prefix used for origin-related validation.
#'
#' @param prefix_dest Character prefix used for destination-related validation.
#'
#' @param input_folder_write Character path (relative to project root) where
#'   outputs will be written.
#'
#' @param input_folder_suffix Optional character suffix appended to the output
#'   folder name.
#'
#' @param max_record Integer maximum number of rows permitted when downloading
#'   BigQuery tables.
#'
#' @param input_highway_regrex Optional. Reserved for future highway regex filtering.
#'
#' @param check_aux_tables Logical or \code{NULL}. If \code{TRUE}, auxiliary
#'   demographic and trip tables are queried. If \code{NULL}, the user is prompted.
#'
#' @param auto_table_download Logical. If \code{TRUE}, queried tables are
#'   automatically written to disk.
#'
#' @param link_selections_df Data frame defining ordered link gates used to
#'   subset trips. Must contain the columns expected by
#'   \code{rplc_make_mvmnt_trip_subsets()} (typically including a sequence column).
#'
#'
#' @examples
#' \dontrun{
#' query_replica_via_gates(
#'   bb_network_layer = corridor_poly,
#'   query_links = c("primary", "secondary"),
#'   mode_type = c("PASSENGER_CAR", "WALKING"),
#'   data_set_location = "seattle",
#'   data_set_period = "2024",
#'   data_set_day = "weekday",
#'   customer_name = "replica_project",
#'   input_folder_write = "data/replica_outputs",
#'   auto_table_download = TRUE,
#'   link_selections_df = link_gates
#' )
#' }
#'
#' @importFrom stringr str_glue
#' @importFrom purrr map pmap
#' @importFrom here here
#' @importFrom log4r logger file_appender info
#' @importFrom qs qsave
#' @importFrom data.table fwrite
#' @importFrom bigrquery bq_project_query
#'
#' @export
query_replica_via_gates = function(
    bb_network_layer
    # bb_sa_layer,
    ,query_links = c("highway", "corridor", "road", "motorway", "motorway_link", "trunk"
                     ,"trunk_link", "primary", "primary_link", "secondary", "secondary_link", "tertiary"
                     ,"tertiary_link")
    ,mode_type = c("PASSENGER_CAR", "PRIVATE_AUTO", "COMMERCIAL", "CARPOOL", "WALKING"
                   ,"BIKING", "PUBLIC_TRANSIT", "ON_DEMAND_AUTO", "OTHER_TRAVEL_MODE")
    ,data_set_location
    ,data_set_period
    ,data_set_day
    ,customer_name
    ,prefix_origin = "origin"
    ,prefix_dest = "destination"
    ,input_folder_write
    ,input_folder_suffix = NULL
    ,max_record = 1000
    ,input_highway_regrex = NULL
    ,check_aux_tables = NULL
    ,auto_table_download = F
    ,link_selections_df
) {


  #sec: Initialization Block====================================================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  {
    mapviewOptions(homebutton = F)

    # Notify initialization
    message(stringr::str_glue("{gauntlet::strg_make_space_2()}{gauntlet::strg_make_space_2()}REPLICATOOLKITR: Initializing function run............"))

    # ── Create references to tables in BigQuery ──
    network_table <- stringr::str_glue("{customer_name}.{data_set_location}.{data_set_location}_{data_set_period}_network_segments")
    trip_table    <- stringr::str_glue("{customer_name}.{data_set_location}.{data_set_location}_{data_set_period}_{data_set_day}_trip")
    person_table  <- stringr::str_glue("{customer_name}.{data_set_location}.{data_set_location}_{data_set_period}_population")

    # ── Validate access to required tables ──
    rplc_checkValidTableConnections(
      prefix_origin = prefix_origin,
      prefix_dest = prefix_dest,
      network_table = network_table,
      trip_table = trip_table
    )

    # ── Get current timestamp for folder naming ──
    query_start <- gauntlet::strg_clean_datetime()

    # ── Construct the folder path where results/logs will be written ──
    folder <- if (is.null(input_folder_suffix)) {
      here::here(input_folder_write, stringr::str_glue("data_mvmnt_{query_start}"))
    } else {
      here::here(input_folder_write, stringr::str_glue("data_mvmnt_{query_start}_{input_folder_suffix}"))
    }

    # ── Prevent overwriting an existing folder ──
    stopifnot("Folder location exists already, stop and prevent overwriting files" = !dir.exists(folder))
    dir.create(folder)

    # ── Initialize log file and logger ──
    log_file <- here::here(folder, "log_file.txt")  # fixed missing quotes around log_file.txt
    logger <- log4r::logger("DEBUG", appenders = log4r::file_appender(log_file))
    log4r::info(logger, "Query started")
    message(stringr::str_glue('Query started at {query_start}/nFile path to log file:/n{log_file}'))

    # ── Construct SQL 'WHERE' clause components from vectors ──
    mode_type_pro <- paste0("'", mode_type, "'", collapse = ", ")
    links_pro <- paste0("'", query_links, "'", collapse = ", ")
    links_pro_net = paste0("'", query_links_net, "'", collapse = ", ")
    links_where_statement <- stringr::str_glue("where highway in ({links_pro})")

    # ── Final message indicating successful setup ──
    message(stringr::str_glue("Initial set up complete\nNo fatal errors detected{gauntlet::strg_make_space_2(last = F)}"))

    # ── Optional: Uncomment this block for detailed query logging ──
    # log4r::info(logger, stringr::str_glue(
    #   "{gauntlet::strg_make_space_2()}/nLogging Query Inputs/n" %>%
    #   "Path to network boundary file: {bb_network_layer}/n" %>%
    #   "Path to study area boundary file: {bb_sa_layer}/n" %>%
    #   "Customer Name: {customer_name}/n" %>%
    #   "Schema Table: {trip_table}/n" %>%
    #   "Links Provided:{gauntlet::strg_make_space_2('-', n = 10)}/n" %>%
    #   "{paste0(stringr::str_glue('{sort(query_links)}'), collapse = '/n')}" %>%
    #   "{gauntlet::strg_make_space_2('-', n = 10)}"
    # ))
  }

  # link_selections_df #this needs to be checked to make sure this df has a "seq" column



  #sec: Spatial Tables==========================================================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #load_process_boundary_object
  {
    message("Loading and processing boundary objects")

    list_wkt_objects = list(
      list(bb_network_layer)
      ,list("network_layer")) %>%
      purrr::pmap(rplc_layer_extent_loadUnionWkt)

  }

  gauntlet::log_and_info("Starting Google query now...", logger)

  ###sub: network table=========================================================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##create network table and perform checks
  {
    temp_table_object = rplc_make_network_tables(
      logger, wkt_object = list_wkt_objects[[1]]
      ,links_pro, customer_name, network_table)

    table_network = temp_table_object$table_network
    highway_counts = temp_table_object$highway_counts
  }


  #sec: table_query============================================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #NOTE: this section still perfroms functions that old function did
  #performs some additioanl processing needed for the mvmnt links

  {
    temp_mvmnt_trip_subset = rplc_make_mvmnt_trip_subsets(

      link_selections_df = link_selections_df
      ,customer_name = customer_name
      ,trip_table = trip_table
      ,mode_type_pro = mode_type_pro
      ,logger = logger
      ,page_size = NULL
      ,page_limit = Inf)

    table_pro = temp_mvmnt_trip_subset$table_pro
  }



  #sec: data_extraction====================================================
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  list(
    list(table_network, "replica_network")
    # ,list(table_sa_poly_index, "replica_study_area_polys")
    # ,list(table_simple_origin_destination, "replica_trip_origin_destination")
    # ,list(table_agg_by_link_subset_limited, "table_agg_network_vols")
    ,list(table_pro, "table_trips_subset")
  ) %>%
    purrr::map(~{
      # browser()

      temp_table = .x[[1]]
      temp_name = .x[[2]]

      rplc_check_to_download_bqtable(
        bq_table = temp_table
        ,file_name = temp_name
        ,table_name = temp_name
        ,max_record = max_record
        ,auto_table_download = auto_table_download
        ,folder = folder
        ,return_obj = F
        ,write_format = c("both")
      )
    })


  link_selections_df %>% qs::qsave(here::here(folder, "table_link_gates.qs"))
  link_selections_df %>% data.table::fwrite(here::here(folder, "table_link_gates.csv"))



  # table_trips_thru_zone

  if(is.null(check_aux_tables)){
    check_aux_tables = gauntlet::robust_prompt_used("Create/download demo and trip chain tables?")
  }


  if(check_aux_tables){
    table_pop_demo  = bigrquery::bq_project_query(
      customer_name
      ,stringr::str_glue('select *
from {person_table}
where 1 = 1
and person_id in (
  select DISTINCT person_id from {replica_temp_tbl_name(table_trips_subset)}
);'))

    temp_table = table_pop_demo
    temp_name = "table_pop_demo"
    rplc_check_to_download_bqtable(
      bq_table = temp_table
      ,file_name = temp_name
      ,table_name = temp_name
      ,max_record = max_record
      ,folder = folder
      ,return_obj = F
      ,write_format = c("both")
    )

    table_trips_subset_all_persons_trips = bigrquery::bq_project_query(
      customer_name
      ,stringr::str_glue('select * except (network_link_ids, transit_route_ids)
from {trip_table}
where 1 = 1
and person_id in (
  select DISTINCT person_id from {replica_temp_tbl_name(table_trips_subset)}
);'))

    temp_table = table_trips_subset_all_persons_trips
    temp_name = "table_trips_subset_all_persons_trips"
    rplc_check_to_download_bqtable(
      bq_table = temp_table
      ,file_name = temp_name
      ,table_name = temp_name
      ,max_record = max_record
      ,folder = folder
      ,return_obj = F
      ,write_format = c("both")
    )
  }

  message(stringr::str_glue("Reminder - data saved to this folder: {basename(folder)}"))



}
