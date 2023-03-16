#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This script contains a basic replicaToolkitR workflow.
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: This is a basic workflow that contains all the general steps
#-------- one will want to execute when acquiring data from Replica.
#--------
#-------- Reminder: This script is supposed to be barebones. ReplicaToolkitR
#-------- suggests that you limit computation and variable creation to
#-------- to scripts and/or `targets` and then load those variables into
#-------- Rmakrdown or other data products.
#
# *please use 80 character margins
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#library set-up=================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
library(crosstalk)
library(data.table)
library(dplyr)
library(forcats)
library(gauntlet)
library(here)
library(leafem)
library(leaflet)
library(leaflet.extras2)
library(log4r)
library(magrittr)
library(mapview)
library(purrr)
library(reactable)
library(readr)
library(sf)
library(sfhotspot)
library(SpatialKDE)
library(stringr)
library(tigris)
library(tidyr)
library(wellknown)

library(replicaToolkitR)

#source helpers/utilities=======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
#none currently here - place your own source code here as you see fit

#source data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
#area to upload data with and to perform initial munging

# query_network_trip_using_bbox(
#   bb_network_layer = 'req_detroit/data/network_poly.gpkg'
#   ,bb_sa_layer = 'req_detroit/data/network_poly.gpkg'
#   ,network_table = "replica-customer.great_lakes.great_lakes_2021_Q4_network_segments"
#   ,trip_table = "replica-customer.great_lakes.great_lakes_2021_Q4_thursday_trip"
#   ,customer_name = "replica-customer"
#   ,file_destination = "req_detroit/data"
#   ,max_record = Inf
#   ,mode_type = c('COMMERCIAL')
#   ,query_links = c("highway", "corridor", "road", "motorway", "motorway_link", "trunk"
#                    ,"primary", "primary_link", "secondary", "secondary_link", "tertiary", "tertiary_link")
# )

#path set-up====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

# #should be the same as *file_destination* input above
# location = 'req_detroit/data'
#
# #this is the folder that the data is written to
# #it is a sub-folder to *file_destination* and will have a data_[datetime] format
# folder = 'data_20230215_113203'

#process acquired data==========================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#the following functions process the data that you acquired from previous section
#by default they save the data automatically
#you can override this and save the returned objects to whatever variable


##create gis layers=============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# acquired_sa_polys = get_tigris_polys_from_replica_index(
#   location = location
#   ,folder = folder
#   ,states = "MI"
#   ,auto_save = T
# )
#
# make_network_link_layer(
#   location = location
#   ,folder = folder
#   ,auto_save = T
# )
#
# replica_queried_network_cntds = make_network_centroid_layer(
#   location = location
#   ,folder = folder
#   ,auto_save = T
# )

##aggregate data================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# make_trip_origin_point_layer(
#   location = location
#   ,folder = folder
#   ,auto_save = T
# )
#
#
#make aggregated link
# {
#   table_agg_by_link_subset_limited  = fread(
#     here(location, folder, "table_agg_by_link_subset_limited.csv"))
#
#   replica_queried_network_links = read_sf(
#     here(location, folder ,"replica_queried_network_links.gpkg"))
#
#   aggregated_network_links = aggregate_network_links(
#     location = location
#     ,folder = folder
#     ,auto_save = T
#     ,agg_count_object = table_agg_by_link_subset_limited
#     ,network_object = replica_queried_network_links
#   )
#
#   export_agg_links_to_spatial(
#     data = aggregated_network_links
#     ,location = location
#     ,folder = folder
#   )
# }
#
# #make aggregated link centroids
# {
#   table_agg_by_link_subset_limited  = fread(
#     here(location, folder, "table_agg_by_link_subset_limited.csv"))
#
#   replica_queried_network_cntds = read_sf(
#     here(location, folder ,"replica_queried_network_cntds.gpkg"))
#
#   aggregated_network_links_cntds = aggregate_network_links(
#     location = location
#     ,folder = folder
#     ,auto_save = F
#     ,agg_count_object = table_agg_by_link_subset_limited
#     ,network_object = replica_queried_network_cntds
#   )
# }
