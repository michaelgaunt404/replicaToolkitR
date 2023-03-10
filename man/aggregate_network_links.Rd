% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers_gis_processing.R
\name{aggregate_network_links}
\alias{aggregate_network_links}
\title{Quickly make network link aggregate objects.}
\usage{
aggregate_network_links(
  location,
  folder,
  auto_save = F,
  agg_count_object = NULL,
  network_object = NULL
)
}
\arguments{
\item{location}{character string pointing to top level location where data acquired from google was saved to.}

\item{folder}{character string of name where data was automatically saved to from google data download.}

\item{auto_save}{Boolean (T/F - default F) indicating if you want the GIS layer to be saved. Default just creates an object without saving.}

\item{agg_count_object}{data frame object containing raw link volumes.}

\item{network_object}{spatial object of either the network as represented with poly-lines or with network link mid-points.}
}
\value{
a data frame and/or saved RDS file
}
\description{
This function makes an RDS list object containing network link layers that have been aggreated three different pre-set ways.
The data contained in `table_agg_by_link_subset_limited` is aggregated by:
+ network_link and flag_sa_destination
+ network_link_ids_unnested and vehicle_type
+ network_link_ids_unnested, origin_poly, and vehicle_type
}
\examples{

data("table_agg_by_link_subset_limited")
data("replica_queried_network_cntds")

aggregate_network_links(
  agg_count_object = table_agg_by_link_subset_limited
  ,network_object = replica_queried_network_cntds
)
}
