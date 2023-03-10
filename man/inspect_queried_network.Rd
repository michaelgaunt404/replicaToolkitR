% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers_vis_objects.R
\name{inspect_queried_network}
\alias{inspect_queried_network}
\title{Quickly map and inspect queried network links}
\usage{
inspect_queried_network(location, folder, network_links_object = NULL)
}
\arguments{
\item{location}{character string pointing to top level location where data acquired from google was saved to.}

\item{folder}{character string of name where data was automatically saved to from google data download.}

\item{network_links_object}{network object containing links. Default is NULL or input left empty - function will use location and folder inputs to load object and then convert.}
}
\value{
an HTML widget map of acquired network links
}
\description{
This is a convenience function which maps the network queried given user inputs.
Replica uses uncommon naming conventions for links and link types. It can be very beneficial to inspect these links once they are queried to ensure you are only including the links you car about in you analyis.

This function creates a **crosstalk object** - a client-side, interactive, filterable HTML widget. It's effectively a bite-size dashboard that helps you better understand your network. You can filter links by link type and by street name.

> **_NOTE:_** Network links are mapped as link _centroids_ - this is due to a quirk given the packages needed to create this feature.
}
\examples{
data("replica_queried_network_links")

inspect_queried_network(
  network_links_object = replica_queried_network_links
)
}
