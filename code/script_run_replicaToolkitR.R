library(tidyverse)
library(sf)
library(here)
library(log4r)
library(bigrquery)
library(gauntlet)
library(replicaToolkitR)
library(wellknown)



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

replicaToolkitR::get_tigris_polys_from_replica_index(
  location = "data/req_dev"
  ,folder = "data_20230112_143236"
  ,states = "WA"
  ,auto_save = T
)



temp = "data/req_dev/data_20230112_143236/replica_trip_agg_by_link_subset.csv" %>%
  here::here() %>%
  data.table::fread()

length(unique(temp$origin_poly))
length(unique(temp$network_link_ids_unnested))

replica_queried_network = "data/req_dev/data_20230112_143236/replica_queried_network.csv" %>%
  here::here() %>%
  data.table::fread()

length(unique(replica_queried_network$stableEdgeId))
