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

#acquire data==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

#make package===================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

study_area_network = here(location, 'study_area_network') %>%
  paste0(".shp") %>%  read_sf() %>%  st_transform(crs = 4326)
usethis::use_data(study_area_network, overwrite = TRUE)

poi_list = here(location, 'poi_list') %>%
  paste0(".csv") %>%  read.csv()
usethis::use_data(poi_list, overwrite = TRUE)


acquired_sa_polys = here(location, folder, 'acquired_sa_polys') %>%
  paste0(".gpkg") %>%  read_sf() %>%  st_transform(crs = 4326)
usethis::use_data(acquired_sa_polys, overwrite = TRUE)

aggregated_network_links = here(location, folder, 'aggregated_network_links') %>%
  paste0(".rds")
usethis::use_data(aggregated_network_links, overwrite = TRUE)

replica_queried_network = here(location, folder, 'replica_queried_network') %>%
  paste0(".csv") %>%  read.csv()
usethis::use_data(replica_queried_network, overwrite = TRUE)

replica_queried_network_cntds = here(location, folder, 'replica_queried_network_cntds') %>%
  paste0(".gpkg") %>%  read_sf() %>%  st_transform(crs = 4326)
usethis::use_data(replica_queried_network_cntds, overwrite = TRUE)

replica_queried_network_links = here(location, folder, 'replica_queried_network_links') %>%
  paste0(".gpkg") %>%  read_sf() %>%  st_transform(crs = 4326)
usethis::use_data(replica_queried_network_links, overwrite = TRUE)

replica_sa_poly_index = here(location, folder, 'replica_sa_poly_index') %>%
  paste0(".csv") %>%  read.csv()
usethis::use_data(replica_sa_poly_index, overwrite = TRUE)

replica_trip_origin_destination = here(location, folder, 'replica_trip_origin_destination') %>%
  paste0(".csv") %>%  read.csv()
usethis::use_data(replica_trip_origin_destination, overwrite = TRUE)

replica_trip_origin_links = here(location, folder, 'replica_trip_origin_links') %>%
  paste0(".csv") %>%  read.csv()
usethis::use_data(replica_trip_origin_links, overwrite = TRUE)

replica_trip_origin_links_gpkg = here(location, folder, 'replica_trip_origin_links') %>%
  paste0(".gpkg") %>%  read_sf() %>%  st_transform(crs = 4326)
usethis::use_data(replica_trip_origin_links_gpkg, overwrite = TRUE)

table_agg_by_link_subset_limited = here(location, folder, 'table_agg_by_link_subset_limited') %>%
  paste0(".csv") %>%  read.csv()
usethis::use_data(table_agg_by_link_subset_limited, overwrite = TRUE)





















