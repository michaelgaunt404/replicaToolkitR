% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers_file_make.R
\name{make_query_and_process_replica_script}
\alias{make_query_and_process_replica_script}
\title{Make new **Query and Process Replica** script from template.}
\usage{
make_query_and_process_replica_script(folder = "code", file_name = NULL)
}
\arguments{
\item{folder}{a string indicating where this file should be saved.}

\item{file_name}{a string name (include .r extension)}
}
\value{
a script in the location and with the name choosen by the user
}
\description{
This function copies an R script template from this package and saves it to usesr specified location.
The template is a basic workflow for acquiring and processing replica data.
}
\examples{
#none
}
