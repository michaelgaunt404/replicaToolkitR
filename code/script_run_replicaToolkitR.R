library(tidyverse)
library(sf)
library(here)
library(log4r)
library(bigrquery)
library(gauntlet)
library(replicaToolkitR)
library(wellknown)

# usethis::use_package("data.table")

location = "data/req_dev"
folder = 'data_20230125_162034'

##acquire data==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

query_network_trip_using_bbox(
  bb_network_layer = 'data/req_dev/study_area_network.shp'
  ,bb_sa_layer = 'data/req_dev/study_area_network.shp'
  ,network_table = "replica-customer.northwest.northwest_2021_Q4_network_segments"
  ,trip_table = "replica-customer.northwest.northwest_2021_Q4_thursday_trip"
  ,customer_name = "replica-customer"
  ,file_destination = "data/req_dev"
  ,max_record = Inf
  ,query_links = c("highway", "corridor", "road", "motorway", "motorway_link", "trunk",
                   "trunk_link", "primary", "primary_link", "secondary", "secondary_link")
)

##get tigris data================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# data("replica_sa_poly_index")

get_tigris_polys_from_replica_index(
  location = location
  ,folder = folder
  ,states = "WA"
  ,auto_save = T
)

##make network links============================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data("replica_queried_network")

make_network_link_layer(
  location = location
  ,folder = folder
  ,auto_save = T
)

##make centroids from network links=============================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data('replica_queried_network_links')

make_network_centroid_layer(
  location = location
  ,folder = folder
  ,auto_save = T
)

##make points of first links=====================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data("replica_trip_origin_links")

make_trip_origin_point_layer(
  location = location
  ,folder = folder
  ,auto_save = T
)

##make network link aggregations================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aggregate_network_links(
  location = location
  ,folder = folder
  ,auto_save = T
)

