#' Replica Auxiliary Network Layers
#'
#' @format A data frame with 8 variables.
#' \describe{
#'   \item{network_link_ids_unnested}{Replica stable edge IDs.}
#'   \item{count}{Trip count associated with each link.}
#'   \item{split_col_type}{Name of grouping variable used (e.g., "time_group").}
#'   \item{split_col_value}{Grouping variable value for that slice.}
#'   \item{data_set_location}{Replica dataset region.}
#'   \item{data_set_period}{Replica dataset period.}
#'   \item{data_set_day}{Day type queried.}
#'   \item{context}{User-supplied descriptor for scenario.}
#' }
#'
#' @details
#' These auxiliary layers are created by splitting trips into mutually exclusive
#' groups (e.g., by hour) and re-querying Replica for link volumes.
#' Each row represents aggregated counts for a specific time slice or grouping.
#' The `split_col_*` fields indicate how trips were partitioned prior to volume extraction.
#' Used to generate multiple network usage layers from one trip table.
"seattleFreight_replica_network_aux_layers"


# -----------------------------------------------------------------------------


#' Replica Network Links (Polyline)
#'
#' @format An `sf` object with 8 variables.
#' \describe{
#'   \item{stableEdgeId}{Replica link ID.}
#'   \item{streetName}{Assigned street name.}
#'   \item{distance}{Link length in meters.}
#'   \item{osmid}{OSM roadway ID.}
#'   \item{highway}{Highway class label.}
#'   \item{flag_contains}{TRUE if inside network study area.}
#'   \item{flag_highway}{Internal tag identifying freight-relevant links.}
#'   \item{geometry}{LINESTRING geometry.}
#' }
#'
#' @details
#' This is the polyline representation of the queried Replica network.
#' It contains only a subset of attributes from the raw Replica table, enough for mapping and joining.
#' Used for visualizing links and merging aggregated network volumes.
#' Created by processing the raw queried network table into sf lines.
"seattleFreight_replica_network_links"


# -----------------------------------------------------------------------------


#' Replica Network Points (Centroids)
#'
#' @format An `sf` object with 8 variables.
#' \describe{
#'   \item{stableEdgeId}{Replica link ID.}
#'   \item{streetName}{Assigned street name.}
#   \item{distance}{Link length.}
#'   \item{osmid}{OSM roadway ID.}
#'   \item{highway}{Highway class.}
#'   \item{flag_contains}{TRUE if inside network study area.}
#'   \item{flag_highway}{Internal freight-highway tag.}
#'   \item{geometry}{POINT geometry representing link midpoint.}
#' }
#'
#' @details
#' These points represent the centroid of each Replica link.
#' They are used because point layers work better with Crosstalk/Leaflet widgets than polylines.
#' Contains the same minimal attributes as the network links table but with point geometry.
#' Helpful for interactive filtering, dashboards, and map popups.
"seattleFreight_replica_network_points"


# -----------------------------------------------------------------------------


#' Raw Queried Replica Network Table (QS)
#'
#' @format A tibble with 14 variables.
#' \describe{
#'   \item{stableEdgeId}{Replica link ID.}
#'   \item{startLat, startLon}{Starting coordinates.}
#'   \item{endLat, endLon}{Ending coordinates.}
#'   \item{streetName}{Street name.}
#'   \item{distance}{Length in meters.}
#'   \item{osmid}{OSM ID.}
#'   \item{speed}{Assigned link speed.}
#'   \item{flags}{Allowed-mode flags.}
#'   \item{lanes}{Number of lanes.}
#'   \item{highway}{Highway class.}
#'   \item{geometry}{WKT LINESTRING.}
#'   \item{flag_contains}{TRUE if link is inside study area.}
#' }
#'
#' @details
#' This is the direct output from Replica’s network table for the user-defined study area.
#' It serves as the base table for building link-sf objects and extracting midpoints.
#' Includes geometry, roadway attributes, and link metadata needed for downstream processing.
#' All spatial network products originate from this dataset.
"seattleFreight_replica_network_qs"


# -----------------------------------------------------------------------------


#' Replica Study Area Polygons (Block Groups)
#'
#' @format A tibble with 8 variables.
#' \describe{
#'   \item{raw_id}{Census block group ID.}
#'   \item{layer}{Always `"bgrp"`.}
#'   \item{id}{Fully qualified Replica polygon ID.}
#'   \item{name}{Human-readable label.}
#'   \item{geom}{Polygon geometry (WKT).}
#'   \item{surface_point}{Interior point.}
#'   \item{primary_timezone}{Timezone.}
#'   \item{flag_contains}{TRUE if polygon is inside analysis area.}
#' }
#'
#' @details
#' These polygons define the OD study area used to classify trips.
#' Trips originating/ending outside these polygons are marked “out of study area.”
#' The polygons also support OD joins and mapping workflows.
#' Used heavily in OD aggregation and movement-type classification.
"seattleFreight_replica_study_area_polys_qs"


# -----------------------------------------------------------------------------


#' Replica Origin–Destination Summary (QS)
#'
#' @format A tibble with 7 variables.
#' \describe{
#'   \item{mode}{Trip mode.}
#'   \item{vehicle_type}{Vehicle subtype.}
#'   \item{origin_poly}{Origin block group or `"out of study area"`.}
#'   \item{flag_sa_origin}{Origin internal/external tag.}
#'   \item{destination_poly}{Destination block group or `"out of study area"`.}
#'   \item{flag_sa_destination}{Destination internal/external tag.}
#'   \item{count}{Trip count.}
#' }
#'
#' @details
#' This table aggregates total trips by OD polygon and vehicle type.
#' It is built from the raw trip subset and collapses external areas into a single category.
#' Used to understand directional freight flows and OD patterns in the study area.
#' Supports both tabular and mapped OD analyses.
"seattleFreight_replica_trip_origin_destination_qs"


# -----------------------------------------------------------------------------


#' Aggregated Network Volumes (QS)
#'
#' @format A tibble with 6 variables.
#' \describe{
#'   \item{mode}{Trip mode.}
#'   \item{vehicle_type}{Vehicle classification.}
#'   \item{flag_sa_origin}{Origin internal/external type.}
#'   \item{flag_sa_destination}{Destination internal/external type.}
#'   \item{network_link_ids_unnested}{Replica link ID.}
#'   \item{count}{Aggregated link volume.}
#' }
#'
#' @details
#' This summarizes how many trips used each link for each movement category.
#' It is a convenient starting layer for network mapping and quick volume checks.
#' Created by aggregating the trip table after combining link-usage logs.
#' Often merged back to link geometry for visualization.
"seattleFreight_table_agg_network_vols_qs"



