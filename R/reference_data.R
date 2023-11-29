#' Asset Metrics
#' @param metrics Vector of metrics. By default all metrics are returned.
#' @param reviewable Boolean, limit to human-reviewable metrics. By default all metrics are returned.
#' @param page_size Number of items per single page of results, default is `100`.
#' @param paging_from Enum: `start` or `end`. Where does the first page start, at the start of the interval or at the end.
#' @return Data table of asset metrics metadata.
#' @export
reference_data_asset_metrics <- function(metrics = NULL,
                                         reviewable = NULL,
                                         page_size = NULL,
                                         paging_from = "start") {
  query_args <- list(
    metrics = metrics,
    reviewable = reviewable,
    page_size = page_size,
    paging_from = paging_from
  )

  resp <- send_coinmetrics_request(
    endpoint = "/reference-data/asset-metrics",
    query_args = query_args
  )
  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "reference-data",
    paging_from = paging_from
  )
}

#' Exchange Metrics
#' @inheritParams reference_data_asset_metrics
#' @return Data table of exchange metrics metadata.
#' @export
reference_data_exchange_metrics <- function(metrics = NULL,
                                            page_size = NULL,
                                            paging_from = "start") {
  query_args <- list(
    metrics = metrics,
    page_size = page_size,
    paging_from = paging_from
  )

  resp <- send_coinmetrics_request(
    endpoint = "/reference-data/exchange-metrics",
    query_args = query_args
  )

  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "reference-data",
    paging_from = paging_from
  )
}

#' Exchange-Asset Metrics
#' @inheritParams reference_data_asset_metrics
#' @return Data table of exchange asset metrics metadata.
#' @export
reference_data_exchange_asset_metrics <- function(metrics = NULL,
                                                  page_size = NULL,
                                                  paging_from = "start") {
  query_args <- list(
    metrics = metrics,
    page_size = page_size,
    paging_from = paging_from
  )

  resp <- send_coinmetrics_request(
    endpoint = "/reference-data/exchange-asset-metrics",
    query_args = query_args
  )

  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "reference-data",
    paging_from = paging_from
  )
}

#' Pair Metrics
#' @inheritParams reference_data_asset_metrics
#' @return Data table of pair metrics metadata.
#' @export
reference_data_pair_metrics <- function(metrics = NULL,
                                        page_size = NULL,
                                        paging_from = "start") {
  query_args <- list(
    metrics = metrics,
    page_size = page_size,
    paging_from = paging_from
  )

  resp <- send_coinmetrics_request(
    endpoint = "/reference-data/pair-metrics",
    query_args = query_args
  )

  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "reference-data",
    paging_from = paging_from
  )
}

#' Institution Metrics
#' @inheritParams reference_data_asset_metrics
#' @return Data table of institution metrics metadata.
#' @export
reference_data_institution_metrics <- function(metrics = NULL,
                                               page_size = NULL,
                                               paging_from = "start") {
  query_args <- list(
    metrics = metrics,
    page_size = page_size,
    paging_from = paging_from
  )

  resp <- send_coinmetrics_request(
    endpoint = "/reference-data/institution-metrics",
    query_args = query_args
  )

  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "reference-data",
    paging_from = paging_from
  )
}

#' Assets
#' @param assets Character vector of assets. By default, all assets are returned.
#' @param page_size Number of items per single page of results, default is `100`.
#' @param paging_from Enum: `start` or `end`. Where does the first page start, at the start of the interval or at the end.
#' @return Data table of assets metadata.
#' @export
reference_data_assets <- function(assets = NULL,
                                  page_size = NULL,
                                  paging_from = "start") {
  query_args <- list(
    assets = assets,
    page_size = page_size,
    paging_from = paging_from
  )
  
  resp <- send_coinmetrics_request(
    endpoint = "/reference-data/assets",
    query_args = query_args
  )
  
  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "reference-data",
    paging_from = paging_from
  )
}

#' Exchanges
#' @param exchanges Character vector of exchanges. By default, all exchanges are returned.
#' @inheritParams reference_data_asset_metrics
#' @return Data table of exchanges metadata.
#' @export
reference_data_exchanges <- function(exchanges = NULL,
                                     page_size = NULL,
                                     paging_from = "start") {
  query_args <- list(
    exchanges = exchanges,
    page_size = page_size,
    paging_from = paging_from
  )
  
  resp <- send_coinmetrics_request(
    endpoint = "/reference-data/exchanges",
    query_args = query_args
  )
  
  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "reference-data",
    paging_from = paging_from
  )
}

#' Markets
#' @param markets Character vector of markets. By default, all markets are returned.
#' @param exchange Unique name of an exchange.
#' @param type Enum: `"spot"`, `"future"`, or "`"option"`. Type of markets.
#' @param base Base asset of markets.
#' @param quote Quote asset of markets.
#' @param asset Any asset of markets.
#' @param symbol Symbol of derivative markets, full instrument name.
#' @inheritParams reference_data_asset_metrics
#' @return Data table of markets metadata.
#' @export
reference_data_markets <- function(markets = NULL,
                                   exchange = NULL,
                                   type = NULL,
                                   base = NULL,
                                   quote = NULL,
                                   asset = NULL,
                                   symbol = NULL,
                                   page_size = 2000,
                                   paging_from = "start") {
  query_args <- list(
    markets = markets,
    exchange = exchange,
    type = type,
    base = base,
    quote = quote,
    asset = asset,
    symbol = symbol,
    page_size = page_size,
    paging_from = paging_from
  )
  
  resp <- send_coinmetrics_request(
    endpoint = "/reference-data/markets",
    query_args = query_args
  )
  
  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "reference-data",
    paging_from = paging_from
  )
}

#' Indexes
#' @param indexes Character vector of indexes. By default, all indexes are returned.
#' @inheritParams reference_data_asset_metrics
#' @return Data table of indexes metadata.
#' @export
reference_data_indexes <- function(indexes = NULL,
                                   page_size = NULL,
                                   paging_from = "start") {
  query_args <- list(
    indexes = indexes,
    page_size = page_size,
    paging_from = paging_from
  )
  
  resp <- send_coinmetrics_request(
    endpoint = "/reference-data/indexes",
    query_args = query_args
  )
  
  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "reference-data",
    paging_from = paging_from
  )
}

#' Pairs
#' @param pairs Character vector of asset pairs. By default, all asset pairs are returned.
#' @inheritParams reference_data_asset_metrics
#' @return Data table of pairs metadata.
#' @export
reference_data_pairs <- function(pairs = NULL,
                                 page_size = NULL,
                                 paging_from = "start") {
  query_args <- list(
    pairs = pairs,
    page_size = page_size,
    paging_from = paging_from
  )
  
  resp <- send_coinmetrics_request(
    endpoint = "/reference-data/pairs",
    query_args = query_args
  )
  
  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "reference-data",
    paging_from = paging_from
  )
}