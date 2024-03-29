% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_mining.R
\name{get_mempool_feerates}
\alias{get_mempool_feerates}
\title{Get Mempool feerates}
\usage{
get_mempool_feerates(
  assets,
  start_time = NULL,
  end_time = NULL,
  start_inclusive = TRUE,
  end_inclusive = TRUE,
  timezone = "UTC",
  page_size = NULL,
  paging_from = "end",
  as_list = FALSE
)
}
\arguments{
\item{assets}{character vector of assets. Use "*" to get metrics for all supported assets.}

\item{start_time}{Start time of the timeseries. Multiple formats of ISO 8601 are supported.}

\item{end_time}{End time of the timeseries. Multiple formats of ISO 8601 are supported.}

\item{start_inclusive}{Flag to define if start timestamp must be included in the timeseries if present. True by default.}

\item{end_inclusive}{Flag to define if end timestamp must be included in the timeseries if present. True by default.}

\item{timezone}{Timezone for \code{start_time} and \code{end_time} timestamps. Default is UTC. Example: America/New_York.}

\item{page_size}{Number of items per single page of results. Default is 100.}

\item{paging_from}{Where first page starts: start or end of interval.}

\item{as_list}{Return the response as (nested) list or tibble.}
}
\value{
Mempool feerates for specified assets. This is useful for those who want to explore the history of the feerate distribution and
potentially select the time period with the lowest commissions.
}
\description{
Get Mempool feerates
}
