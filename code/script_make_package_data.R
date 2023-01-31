#
# This is script is used to write example data to package data folder
#
# By: mike gaunt, michael.gaunt@wsp.com
#
# README: package data is required for function examples
#-------- package data is required for vignettes
#
# *please use 80 character margins
# *please go to https://pkgs.rstudio.com/flexdashboard/articles/layouts.html
# to explore the different layouts available for the flexdashboard framework
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


location = "data/req_dev"
folder = 'data_20230117_092037'


study_area_network = here(location, 'study_area_network') %>%
  paste0(".shp") %>%  read_sf() %>%  st_transform(crs = 4326)
usethis::use_data(study_area_network, overwrite = TRUE)

poi_list = here(location, 'poi_list') %>%
  paste0(".csv") %>%  read.csv()
usethis::use_data(poi_list, overwrite = TRUE)

replica_queried_network = here(location, folder, 'replica_queried_network') %>%
  paste0(".csv") %>%  read.csv()
usethis::use_data(replica_queried_network, overwrite = TRUE)

replica_sa_poly_index = here(location, folder, 'replica_sa_poly_index') %>%
  paste0(".csv") %>%  read.csv()
usethis::use_data(replica_sa_poly_index, overwrite = TRUE)

replica_trip_origin_destination = here(location, folder, 'replica_trip_origin_destination') %>%
  paste0(".csv") %>%  read.csv()
usethis::use_data(replica_trip_origin_destination, overwrite = TRUE)

replica_trip_origin_links = here(location, folder, 'replica_trip_origin_links') %>%
  paste0(".csv") %>%  read.csv()
usethis::use_data(replica_trip_origin_links, overwrite = TRUE)

table_agg_by_link_subset_limited = here(location, folder, 'table_agg_by_link_subset_limited') %>%
  paste0(".csv") %>%  read.csv()
usethis::use_data(table_agg_by_link_subset_limited, overwrite = TRUE)

replica_trip_origin_links_gpkg = here(location, folder, 'replica_trip_origin_links') %>%
  paste0(".gpkg") %>%  read_sf() %>%  st_transform(crs = 4326)
usethis::use_data(replica_trip_origin_links_gpkg, overwrite = TRUE)

acquired_sa_polys = here(location, folder, 'acquired_sa_polys') %>%
  paste0(".gpkg") %>%  read_sf() %>%  st_transform(crs = 4326)
usethis::use_data(acquired_sa_polys, overwrite = TRUE)

aggregated_network_links = here(location, folder, 'aggregated_network_links') %>%
  paste0(".rds") %>%  read_rds()
usethis::use_data(aggregated_network_links, overwrite = TRUE)













