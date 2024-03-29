% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reference_data.R
\name{reference_data_exchanges}
\alias{reference_data_exchanges}
\title{Exchanges}
\usage{
reference_data_exchanges(
  exchanges = NULL,
  page_size = NULL,
  paging_from = "start"
)
}
\arguments{
\item{exchanges}{Character vector of exchanges. By default, all exchanges are returned.}

\item{page_size}{Number of items per single page of results, default is \code{100}.}

\item{paging_from}{Enum: \code{start} or \code{end}. Where does the first page start, at the start of the interval or at the end.}
}
\value{
Data table of exchanges metadata.
}
\description{
Exchanges
}
