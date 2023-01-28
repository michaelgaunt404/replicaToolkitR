library(tidyverse)
library(sf)
library(here)
library(log4r)
library(bigrquery)
library(gauntlet)
library(replicaToolkitR)
library(wellknown)
library(data.table)

# usethis::use_package("data.table")

location = "data/req_seattle_everett"
folder = 'data_20230127_135530'

#acquire data==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##acquire data==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

network_raw = make_network_link_layer(
  location = location
  ,folder = folder
  ,auto_save = T
)


library(leaflet)
library(crosstalk)











