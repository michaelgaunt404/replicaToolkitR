% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/view_replica_study_area_network.R
\name{view_replica_study_area_network}
\alias{view_replica_study_area_network}
\title{Query Replica's roadway network given a user defined map extent}
\usage{
view_replica_study_area_network(network_table, customer_name, links_pro = NA)
}
\arguments{
\item{network_table}{The name of the table containing the roadway network data to be queried.}

\item{customer_name}{The name of the customer whose project contains the \code{network_table} table.}
}
\value{
A data frame of the roadway network data that intersects with the study area.
}
\description{
This function prompts the user to draw a study area using the \code{mapedit::drawFeatures()} function, and returns a data frame of the roadway network data that intersects with the study area.
}
\examples{
\dontrun{
view_replica_study_area_network("network_table", "customer_name")
}
}
