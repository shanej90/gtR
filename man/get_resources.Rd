% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_resources.R
\name{get_resources}
\alias{get_resources}
\title{Function to streamline searching for resource records either in isolation or records associated with another resource.}
\usage{
get_resources(
  resource,
  output,
  resource_id,
  page_nums,
  size = 20,
  search_term,
  search_fields
)
}
\arguments{
\item{resource}{The name of the resource you would like to query/return, as per \code{gtr_endpoints}.}

\item{output}{The name of the resource you would like to return (linked to a specific resource_id), as per \code{gtr_endpoints}.}

\item{resource_id}{The id used to link the resources}

\item{page_nums}{The numbers of results you would like to see, leaving blank will return all pages bound together. Enter as numeric vector.}

\item{size}{The number of results you would like to return (min 10, max 100).}

\item{search_term}{Term you want to check against search_fields. Search works on a 'contains' basis. Note search_term applies to the \code{resource}, NOT the \code{output}.}

\item{search_fields}{Fields you want to check search_term against. Enter as character vector. Note fields must be referred to by their code, and some fields may be searched by default. Use \code{get_configs()} for details of codes and default search fields. Note \code{search_fields} relates to the resource, NOT the output.}
}
\value{
A dataframe holding the results for your chosen query, or a list holding the dataframe plus corresponding metadata..
}
\description{
Sends a query to return all records for your chosen resource, with the option to query a combination of resources.
}
