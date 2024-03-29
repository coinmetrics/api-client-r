% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_market_vols.R
\name{get_market_greeks}
\alias{get_market_greeks}
\title{Get Market Greeks}
\usage{
get_market_greeks(
  markets,
  start_time = NULL,
  end_time = NULL,
  start_inclusive = TRUE,
  end_inclusive = TRUE,
  timezone = "UTC",
  page_size = NULL,
  paging_from = "end",
  limit_per_market = NULL
)
}
\arguments{
\item{markets}{Vector of markets.}

\item{start_time}{Start time of the timeseries. Multiple formats of ISO 8601 are supported.}

\item{end_time}{End time of the timeseries. Multiple formats of ISO 8601 are supported.}

\item{start_inclusive}{Flag to define if start timestamp must be included in the timeseries if present. True by default.}

\item{end_inclusive}{Flag to define if end timestamp must be included in the timeseries if present. True by default.}

\item{timezone}{Timezone for \code{start_time} and \code{end_time} timestamps. Default is UTC. Example: America/New_York.}

\item{page_size}{Number of items per single page of results. Default is 100.}

\item{paging_from}{Where first page starts: start or end of interval.}

\item{limit_per_market}{Number of entries per market.}
}
\value{
Data table of greeks for option markets
}
\description{
Get Market Greeks
}
