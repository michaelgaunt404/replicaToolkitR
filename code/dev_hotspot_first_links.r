#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# This is script [[insert brief readme here]]
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: [[insert brief readme here]]
#-------- [[insert brief readme here]]
#
# *please use 80 character margins
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#library set-up=================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
library(tidyverse)
library(gauntlet)
library(tigris)
library(sf)
library(mapview)
library(here)
library(SpatialKDE)
library(sfhotspot)

#path set-up====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

#source helpers/utilities=======================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev

#source data====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#content in this section should be removed if in production - ok for dev
#area to upload data with and to perform initial munging
#please add test data here so that others may use/unit test these scripts

##spatial data==================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#data for query
base_geo =  here("data/req_ri", "GIS/bgrp_RI4_a.shp") %>%
  read_sf() %>%
  st_transform(4326)


#queried data
raw_first_link_network = here("data/req_dc", "raw_first_link_network.csv") %>%
  read.csv()

block_groups_2010 = here("data/req_dc",
                         "block_groups_2010.gpkg") %>%
  sf::read_sf()

#starting locations for vehicles
network_gis = raw_first_link_network %>%
  st_as_sf(wkt = "geometry", crs = 4326)

network_gis_cntrds = network_gis %>%
  gauntlet::st_true_midpoint()

network_gis_cntrds_pro = network_gis_cntrds %>%
  mutate(stableEdgeId_trunc = str_trunc(stableEdgeId   , 14, "right", ""))

#tabular trip data

#queried data
first_link_trips_data_agg = here("data/req_dc"
                                 ,"first_link_trips_data_agg.csv") %>%
  read.csv()

#processing location data for origin and netowkr lins
origin_bg = first_link_trips_data_agg %>%
  group_by(origin_bgrp) %>%
  summarise(count = sum(count))

origin_link = first_link_trips_data_agg %>%
  mutate(network_link_ids_unnested = as.character(network_link_ids_unnested)) %>%
  group_by(network_link_ids_unnested) %>%
  summarise(count = sum(count)) %>%
  mutate(network_link_ids_unnested_trunc = str_trunc(network_link_ids_unnested   , 14, "right", ""))

#data where vehicles cannot possibly go
# manually made but other datasets can be used
dc_no = here("data/req_dc", "locations_without_trucks.gpkg") %>%
  read_sf()

#main header====================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bounding_box = st_bbox(c(xmin = -77.370418, ymin = 38.644433
                         ,xmax = -76.704438, ymax = 39.107938), crs = st_crs(4326)) %>%
  st_as_sfc()

##combined data=================================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# block_groups_comb = block_groups_2010 %>%
#   merge(origin_bg
#         ,by.x = 'GEOID10', by.y = "origin_bgrp", all.x = T) %>%
#   mutate(count = replace_na(count, 0)
#          ,count_area = count/(ALAND10/1000^2))
#
# block_groups_comb %>%
#   filter(!is.infinite(count_area)) %>%
#   st_filter(bounding_box) %>%
#   mapview(zcol = "count_area")

##legit hotspot analysis========================================================
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#combine data
netwrok_comb_just_dc = network_gis_cntrds_pro %>%
  merge(origin_link,
        by.x = "stableEdgeId_trunc", by.y = "network_link_ids_unnested_trunc") %>%
  st_filter(dc_bg %>%
              st_transform(4326))

#need transform so KDE can be made
link_hotspot_df_dc = netwrok_comb_just_dc %>%
  st_transform(crs = crs)

grid_sm = create_grid_hexagonal(link_hotspot_df_dc,
                                cell_size = 200)

grid_sm_rm = grid_sm %>%
  st_join(dc_no %>%
            st_transform(crs = crs)
          ,join = st_within) %>%
  filter(is.na(X_leaflet_id))

kde_hs_wt_500_1500_just_dc = hotspot_kde(
  link_hotspot_df_dc
  ,grid = grid_sm
  ,bandwidth = 1500
  ,weights = count)

kde_hs_wt_500_1500_just_dc_rm = hotspot_kde(
  link_hotspot_df_dc
  ,grid = grid_sm_rm
  ,bandwidth = 800
  ,weights = count)

kde_hs_wt_500_1500_just_dc_rm_gi = hotspot_gistar(
  link_hotspot_df_dc
  # ,nb_dist = 600
  ,grid = grid_sm_rm
  ,bandwidth = 800
  ,weights = count)

map_sm = kde_hs_wt_500_1500_just_dc %>%
  st_transform(4326) %>%
  mapview(zcol = "kde")

kde_hs_wt_500_1500_just_dc_rm %>%
  st_transform(4326) %>%
  mapview(zcol = "kde")

#important - this is the key
(kde_hs_wt_500_1500_just_dc_rm_gi %>%
  filter(#gistar < 0
         pvalue <= .01) %>%
  st_transform(4326) %>%
  mapview(zcol = "gistar")) + mapview(dc_no)

  mapview(zcol = "kde")



60/9/12.97
library(leafsync)
install.packages("leafsync")
sync(map_lrg, map_sm)
mapview(grid_sm)

library(SpatialKDE)

grid_sm = create_grid_hexagonal(link_hotspot_df_dc,
                                cell_size = 200)

grid_sm_new = grid_sm %>%
  st_join(dc_no %>%
            st_transform(crs = crs)
          ,join = st_within) %>%
  filter(is.na(X_leaflet_id)) %>%
  mapview()


kde_spkde = kde(link_hotspot_df_dc
                ,band_width = 1500,
                kernel = "quartic", grid = grid_sm_new)

map_new = kde_spkde %>%
  st_transform(4326) %>%
  mapview(zcol = "kde_value")


sync(map_new, map_sm)




























