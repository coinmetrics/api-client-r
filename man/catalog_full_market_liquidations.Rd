% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/catalog_full.R
\name{catalog_full_market_liquidations}
\alias{catalog_full_market_liquidations}
\title{Supported Market Liquidations}
\usage{
catalog_full_market_liquidations(
  markets = NULL,
  exchange = NULL,
  type = NULL,
  base = NULL,
  quote = NULL,
  asset = NULL,
  symbol = NULL
)
}
\arguments{
\item{markets}{Vector of markets. By default all markets are returned.}

\item{exchange}{Unique name of an exchange.}

\item{type}{Type of markets. Available inputs are \code{spot}, \code{future}, or \code{option}.}

\item{base}{Base asset of markets.}

\item{quote}{Quote asset of markets.}

\item{asset}{Any asset of markets.}

\item{symbol}{Symbol of derivative markets; the full instrument name.}
}
\value{
Tibble of all markets with liquidations support along with the time ranges of available data.
}
\description{
Supported Market Liquidations
}
