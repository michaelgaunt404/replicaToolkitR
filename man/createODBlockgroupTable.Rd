% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sql_createODBlockgroupTable.R
\name{createODBlockgroupTable}
\alias{createODBlockgroupTable}
\title{Perform Origin and Destination Aggregations}
\usage{
createODBlockgroupTable(customer_name, table_trips_thru_zone, mvmnt_query = F)
}
\arguments{
\item{customer_name}{The customer's name.}

\item{table_trips_thru_zone}{The name of the trips table.}

\item{replica_temp_tbl_name}{The name of the replica temp table.}
}
\value{
A message indicating the aggregation process.
}
\description{
This function logs a message and performs the aggregation of origin and destination data.
}
