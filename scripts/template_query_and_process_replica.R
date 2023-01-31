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
library(data.table)
library(dplyr)
library(gauntlet)
library(here)
library(leaflet)
library(leaflet.extras2)
library(log4r)
library(magrittr)
library(mapview)
library(purrr)
library(sf)
library(sfhotspot)
library(SpatialKDE)
library(stringr)
library(tigris)
library(wellknown)

#source helpers/utilities=======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
#none currently here - place your own source code here as you see fit

#source data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
#area to upload data with and to perform initial munging
query_network_trip_using_bbox(
  bb_network_layer = 'data/req_seattle_everett/study_network.gpkg'
  ,bb_sa_layer = 'data/req_seattle_everett/study_area_poly.gpkg'
  ,network_table = "replica-customer.northwest.northwest_2019_Q4_network_segments"
  ,trip_table = "replica-customer.northwest.northwest_2019_Q4_thursday_trip"
  ,customer_name = "replica-customer"
  ,file_destination = "data/req_seattle_everett"
  ,max_record = Inf
  ,query_links = c("highway", "corridor", "road", "motorway", "motorway_link", "trunk",
                   "trunk_link", "primary", "primary_link", "secondary", "secondary_link")
)

#path set-up====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

#should be the same as *file_destination* input above
# location =

#this is the folder that the data is written to
#it is a sub-folder to *file_destination* and will have a data_[datetime] format
#folder =

#process acquired data==========================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#the following functions process the data that you acquired from previous section
#by default they save the data automatically
#you can override this and save the returned objects to whatever variable

get_tigris_polys_from_replica_index(
  location = location
  ,folder = folder
  ,states = "STATES THAT YOUR PROJECT TAKES PLACE IN"
  ,auto_save = F
)

make_network_link_layer(
  location = location
  ,folder = folder
  ,auto_save = F
)

make_network_centroid_layer(
  location = location
  ,folder = folder
  ,auto_save = F
)

make_trip_origin_point_layer(
  location = location
  ,folder = folder
  ,auto_save = F
)

aggregate_network_links(
  location = location
  ,folder = folder
  ,auto_save = F
)

make_agg_network_shapefile_links(
  location = location
  ,folder = folder
  ,auto_save = F
)

#inspect processed data=========================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#this section contains functions that make it easy to inspect the data made above
inspect_queried_network(
  location = location
  ,folder = folder
)


