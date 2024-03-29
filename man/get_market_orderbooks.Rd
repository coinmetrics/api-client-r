% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_market_orderbooks.R
\name{get_market_orderbooks}
\alias{get_market_orderbooks}
\title{Get Market Orderbooks}
\usage{
get_market_orderbooks(
  markets,
  start_time = NULL,
  end_time = NULL,
  start_inclusive = TRUE,
  end_inclusive = TRUE,
  timezone = "UTC",
  page_size = NULL,
  depth_limit = NULL,
  paging_from = "end",
  limit_per_market = NULL
)
}
\arguments{
\item{markets}{Vector of markets or market patterns like \verb{exchange-*} or \verb{exchange-*-spot} or \verb{*USDT-future}.}

\item{start_time}{Start time of the timeseries. Multiple formats of ISO 8601 are supported.}

\item{end_time}{End time of the timeseries. Multiple formats of ISO 8601 are supported.}

\item{start_inclusive}{Flag to define if start timestamp must be included in the timeseries if present. True by default.}

\item{end_inclusive}{Flag to define if end timestamp must be included in the timeseries if present. True by default.}

\item{timezone}{Timezone for \code{start_time} and \code{end_time} timestamps. Default is UTC. Example: America/New_York.}

\item{page_size}{Number of items per single page of results. Default is 100.}

\item{depth_limit}{Book depth limit. Supported values are between 1 and 30000, \verb{10pct_mid_price} or \code{full_book}.}

\item{paging_from}{Where first page starts: start or end of interval.}

\item{limit_per_market}{Number of entries per market.}
}
\value{
Tibble of orderbooks for specified markets, ordered by tuple \verb{(market, time)}. \code{asks} and \code{bids} are nested list-columns.
}
\description{
Get Market Orderbooks
}
