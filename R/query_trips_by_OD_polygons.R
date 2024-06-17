#' Query Replica trips by origin or destination polygons
#'
#' @details
#' The primary focus of this function is to retrieve trips that either originate or terminate within a specific polygon(s) - these can be custom polygons or normal block groups, etc.
#'
#'
#'
#' It is ideal for capturing trips that have their starting or ending points within these defined polygons.
#' If you need to query trips that pass through a specific zone but don't necessarily start or end in that zone, please refer to the separate function designed for that purpose.
#'
#' This function facilitates the connection to the Replica Mobility Data Platform API, allowing users to query and retrieve transportation data.
#' It requires several inputs for customization, see below.
#'
#' @param network_table A string specifying the network table to use.
#' @param trip_table A string indicating the trip table to use.
#' @param customer_name A string representing the customer name - this should be your own personal access key.
#' @param mode_type A character vector containing the mode types to filter the data (e.g., 'COMMERCIAL', 'PRIVATE_AUTO', 'ON_DEMAND_AUTO').
#' @param query_links A character vector specifying types of links to query (e.g., 'highway', 'corridor', 'road', 'motorway', 'motorway_link', 'trunk', 'primary', 'primary_link', 'secondary', 'secondary_link').
#' @param index_od A vector of two strings indicating locational attributes to query with (e.g., c("origin_long", "origin_lat")).
#' @param study_area A spatial polygon denoting the extent of the study area - this will be used to query network links - should be as small as possible to limit the number of links the user downloads.
#' @param poly_gen_att A spatial data frame (using the SF package) that denotes polygons to be used as origin or destination locations for the query.
#'
#' @return A list containing the results of the Replica Mobility Data Platform query. The list consists of two elements:
#'
#'  1. `table_od_poly`: A BigQuery table string representing the table of OD polygons.
#'  2. `table_trips_links`: A BigQuery table string representing the table of trip links.
#'
#' @seealso More information about the Replica Mobility Data Platform can be found at [Replica Mobility Data Platform](https://www.replicamobility.com/).
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' result <- query_trips_by_OD_polygons(
#'   network_table = "replica-customer.northwest.northwest_2019_Q4_network_segments",
#'   trip_table = "replica-customer.northwest.northwest_2019_Q4_thursday_trip",
#'   customer_name = "replica-customer",
#'   mode_type = c('COMMERCIAL'),
#'   query_links = c("highway", "corridor", "road", "motorway", "motorway_link", "trunk",
#'                   "primary", "primary_link", "secondary", "secondary_link"),
#'   index_od = c("origin_long", "origin_lat"),
#'   study_area = study_area_sf,
#'   poly_gen_att = poly_gen_att_sf
#' )
#' }
#'
#' @export
query_trips_by_OD_polygons = function(
    network_table, trip_table, customer_name
    ,query_links = c('highway','corridor','road'
                     ,'motorway','motorway_link'
                     ,'trunk','trunk_link'
                     ,'primary','primary_link'
                     ,'secondary','secondary_link'
                     ,'tertiary','tertiary_link')
    ,mode_type = c('PASSENGER_CAR','PRIVATE_AUTO'
                   ,'COMMERCIAL','CARPOOL'
                   ,'WALKING','BIKING','PUBLIC_TRANSIT'
                   ,'ON_DEMAND_AUTO','OTHER_TRAVEL_MODE')
    ,index_od, study_area, poly_gen_att){

  stopifnot("Name not in poly_gen_att --- Run terminated!!!!\nPlease change the attribute that names polys to 'Name'" = "Name" %in% colnames(poly_gen_att ))
  stopifnot("index_od must have two entries --- Run terminated!!!!\nPlease check this " = length(index_od) == 2)

  index_od_pro = paste0(index_od, collapse = ", ")
  tmp_dir_check = (gsub("_.*", "\\1", index_od[1]))
  type = ifelse( tmp_dir_check%in% c("start", "origin"),"outgoing", "incoming")
  message(str_glue("User supplied index_od with ///{tmp_dir_check}/// prefix"))
  message(str_glue("{str_to_title(type)} trip will be queried for each provided polygon..."))

  study_area_wkt_union = wellknown::sf_convert(st_union(study_area))
  poly_gen_att_wkt_union = wellknown::sf_convert(st_union(poly_gen_att))
  links_pro = paste0("'", query_links, "'", collapse = ", ")
  mode_type_pro = paste0("'", mode_type, "'", collapse = ", ")


  message(str_glue("{gauntlet::make_space()}\nBegining trip prefiltering with unioned poly_gen_att polys\nStep perfromed to limit Replica computation cost...."))

  table_network_subset = bigrquery::bq_project_query(
    customer_name
    ,stringr::str_glue("select * from (
select *,
ST_INTERSECTS(
ST_GEOGFROMTEXT('{study_area_wkt_union}')
,geometry) as flag_contains
from `{network_table}`
where highway in ({links_pro})
)
where flag_contains = TRUE"))

#this step pre-filters the trip data using the with a union of the provided poly_gen_att
table_inital_spatial_filtering = bigrquery::bq_project_query(
  customer_name
  ,stringr::str_glue("select * from (
select *,
ST_CONTAINS(
ST_GEOGFROMTEXT('{poly_gen_att_wkt_union}')
,ST_GeogPoint({index_od_pro})) as flag_contains
from `{trip_table}`
where mode in ({mode_type_pro})
)
where flag_contains = TRUE"))

table_inital_spatial_filtering_count = bigrquery::bq_table_nrow(table_inital_spatial_filtering)

message(str_glue("Query results in {table_inital_spatial_filtering_count} trip records{gauntlet::make_space()}"))

message(str_glue("{gauntlet::make_space()}\nMaking table_trip_custom_poly_list - process maps trips to correct polygon: "))
table_trip_custom_poly_list =
  list(
    poly_gen_att$Name
    ,wellknown::sf_convert(poly_gen_att)
  ) %>%
  pmap(function(x, y) {

    print(str_glue('Processing: {x}'))

    #This uses supplied custom polygons to spatially filter trips
    temp_table_poly_subset = bigrquery::bq_project_query(
      customer_name
      ,stringr::str_glue("select *, '{x}' as poly_name from (
select *,
ST_CONTAINS(
ST_GEOGFROMTEXT('{y}')
,ST_GeogPoint({index_od_pro})) as flag_contains_poly
from `{replica_temp_tbl_name(table_inital_spatial_filtering)}`
where mode in ({mode_type_pro})
)
where flag_contains_poly = TRUE"))

print(str_glue('There are {bigrquery::bq_table_nrow(temp_table_poly_subset)} rows in subset...'))

return(temp_table_poly_subset)
  })

table_trip_custom_poly_list_comb = bigrquery::bq_project_query(
  customer_name
  ,stringr::str_glue('{paste0("select * from `", unlist(map(table_trip_custom_poly_list, replica_temp_tbl_name)), "`",  collapse = " union all ")};')) #need space at end

table_inital_spatial_filtering_count = bigrquery::bq_table_nrow(table_inital_spatial_filtering)

message(str_glue("The table ratio is {round(table_inital_spatial_filtering_count/table_inital_spatial_filtering_count, 3)}\nIf this value is less than 1 then leakage has occured..."))

#agg by either origin or destination
table_od_poly = bigrquery::bq_project_query(
  customer_name
  ,stringr::str_glue("select poly_name, mode, vehicle_type
  ,{index_od[1]} as point_lng, {index_od[2]} as point_lat
  ,count(*) as count
  ,'{type}' as dir_flow
from `{replica_temp_tbl_name(table_trip_custom_poly_list_comb)}`
group by poly_name, mode, vehicle_type, {index_od_pro}"))

#still breaks links links out and aggs
#counts by network link , mode, vehicle type
table_ord_trip_links = bigrquery::bq_project_query(
  customer_name
  ,stringr::str_glue("select poly_name, mode, vehicle_type, network_link_ids_unnested, count(*) as count from
         (
        select activity_id, mode, vehicle_type, network_link_ids_unnested, poly_name
    from `{replica_temp_tbl_name(table_trip_custom_poly_list_comb)}`
,unnest(network_link_ids) as network_link_ids_unnested)
group by poly_name, mode, vehicle_type, network_link_ids_unnested;"))

#joins network link data to broken out links
table_ord_trip_links_sf = bigrquery::bq_project_query(
  customer_name
  ,stringr::str_glue("select *
  from `{replica_temp_tbl_name(table_ord_trip_links)}` table_left
  left join `{replica_temp_tbl_name(table_network_subset)}` table_right
  on table_left.network_link_ids_unnested = table_right.stableEdgeId;"))

#spatially filters for only study area WKT
#note: this step is actually super redundant --- there is already a flag to filter on
#----- keeping it in here since I dont want to mess anything up
table_ord_trip_links_sf_pro = bigrquery::bq_project_query(
  customer_name
  ,stringr::str_glue("select * from (
select *,
ST_INTERSECTS(
ST_GEOGFROMTEXT('{study_area_wkt_union}')
,geometry) as flag_contains_1
from `{replica_temp_tbl_name(table_ord_trip_links_sf)}`
where mode in ({mode_type_pro})
and flag_contains = TRUE and flag_contains is not NULL
)
where flag_contains_1 = TRUE and flag_contains is not NULL" )
)

return(
  list(
    table_od_poly = table_od_poly
    ,table_ord_trip_links_sf = table_ord_trip_links_sf_pro
  )
)

}
