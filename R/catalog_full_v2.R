#' Supported Asset Metrics
#' @inheritParams catalog_v2_asset_metrics
#' @return Data table of all supported asset metrics, along with the time ranges of available data.
#' @export
catalog_full_v2_asset_metrics <- function(assets = NULL,
                                          metrics = NULL,
                                          reviewable = NULL,
                                          page_size = 2000,
                                          paging_from = "start") {
  
  query_args <- list(
    assets = assets,
    metrics = metrics,
    reviewable = reviewable,
    page_size = page_size,
    paging_from = paging_from
  )
  
  resp <- send_coinmetrics_request(
    endpoint = "/catalog-all-v2/asset-metrics",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2MetricsData(catalog_data)
}

#' Supported Exchange Metrics
#' @inheritParams catalog_v2_exchange_metrics
#' @return Data table of all supported exchange metrics, along with the time ranges of available data.
#' @export
catalog_full_v2_exchange_metrics <- function(exchanges = NULL,
                                             metrics = NULL,
                                             page_size = 2000,
                                             paging_from = "start") {
  
  query_args <- list(
    exchanges = exchanges,
    metrics = metrics,
    page_size = page_size,
    paging_from = paging_from
  )
  
  resp <- send_coinmetrics_request(
    endpoint = "/catalog-all-v2/exchange-metrics",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2MetricsData(catalog_data)
}

#' Supported Exchange-Asset Metrics
#' @inheritParams catalog_v2_exchange_asset_metrics
#' @return Data table of all supported exchange-asset metrics, along with the time ranges of available data.
#' @export
catalog_full_v2_exchange_asset_metrics <- function(exchange_assets = NULL,
                                                   metrics = NULL,
                                                   page_size = 2000,
                                                   paging_from = "start") {
  query_args <- list(
    exchange_assets = exchange_assets,
    metrics = metrics,
    page_size = page_size,
    paging_from = paging_from
  )
  
  resp <- send_coinmetrics_request(
    endpoint = "/catalog-all-v2/exchange-asset-metrics",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2MetricsData(catalog_data)
}

#' Supported Pair Metrics
#' @inheritParams catalog_v2_pair_metrics
#' @param pairs Character vector of asset pairs. By default, all asset pairs are returned.
#' @return Data table of all supported pair metrics, along with the time ranges of available data.
#' @export
catalog_full_v2_pair_metrics <- function(pairs = NULL,
                                         metrics = NULL,
                                         page_size = 2000,
                                         paging_from = "start") {
  query_args <- list(
    pairs = pairs,
    metrics = metrics,
    page_size = page_size,
    paging_from = paging_from
  )
  
  resp <- send_coinmetrics_request(
    endpoint = "/catalog-all-v2/pair-metrics",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2MetricsData(catalog_data)
}

#' Supported Institutions Metrics
#' @inheritParams catalog_v2_institution_metrics
#' @return Data table of all supported institution metrics, along with the time ranges of available data.
#' @export
catalog_full_v2_institution_metrics <- function(institutions = NULL,
                                                metrics = NULL,
                                                page_size = 2000,
                                                paging_from = "start") {
  query_args <- list(
    institutions = institutions,
    metrics = metrics,
    page_size = page_size,
    paging_from = paging_from
  )
  
  resp <- send_coinmetrics_request(
    endpoint = "/catalog-all-v2/institution-metrics",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2MetricsData(catalog_data)
}

#' Supported Market Trades
#' @inheritParams catalog_v2_market_trades
#' @return Data table of all markets with trades support, along with the time ranges of available data.
#' @export
catalog_full_v2_market_trades <- function(markets = NULL,
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
    endpoint = "/catalog-all-v2/market-trades",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2MarketsData(catalog_data)
}

#' Available Market Candles
#' @inheritParams catalog_v2_market_candles
#' @return Data table of all markets with candles support, 
#' along with the time ranges of available data per candle duration.
#' @export
catalog_full_v2_market_candles <- function(markets = NULL,
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
    endpoint = "/catalog-all-v2/market-candles",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2MarketCandlesData(catalog_data)
}

#' Supported Market Orderbooks
#' @inheritParams catalog_v2_market_orderbooks
#' @return Data table of all markets with orderbooks support, 
#' along with the time ranges of available data per candle duration.
#' @export
catalog_full_v2_market_orderbooks <- function(markets = NULL,
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
    endpoint = "/catalog-all-v2/market-orderbooks",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2OrderbooksData(catalog_data)
}

#' Supported Market Quotes
#' @inheritParams catalog_v2_market_quotes
#' @return Data table of all markets with quotes support, along with the time ranges of available data.
#' @export
catalog_full_v2_market_quotes <- function(markets = NULL,
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
    endpoint = "/catalog-all-v2/market-quotes",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2MarketsData(catalog_data)
}

#' Supported Market Funding Rates
#' @inheritParams catalog_v2_market_funding_rates
#' @return Data table of all markets with funding rates support, along with the time ranges of available data.
#' @export
catalog_full_v2_market_funding_rates <- function(markets = NULL,
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
    endpoint = "/catalog-all-v2/market-funding-rates",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2MarketsData(catalog_data)
}

#' Supported Market Contract Prices
#' @inheritParams catalog_v2_market_contract_prices
#' @return Data table of all contract prices for option market.
#' @export
catalog_full_v2_market_contract_prices <- function(markets = NULL,
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
    endpoint = "/catalog-all-v2/market-contract-prices",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2MarketsData(catalog_data)
}

#' Supported Market Implied Volatility
#' @inheritParams catalog_v2_market_implied_volatility
#' @return Data table of all implied volatility for option market.
#' @export
catalog_full_v2_market_implied_volatility <- function(markets = NULL,
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
    endpoint = "/catalog-all-v2/market-implied-volatility",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2MarketsData(catalog_data)
}

#' Supported Market Greeks
#' @inheritParams catalog_v2_market_greeks
#' @return Data table of all market greeks volatility for option market.
#' @export
catalog_full_v2_market_greeks <- function(markets = NULL,
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
    endpoint = "/catalog-all-v2/market-greeks",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2MarketsData(catalog_data)
}

#' Supported Market Open Interest
#' @inheritParams catalog_v2_market_open_interest
#' @return Data table of all markets with open interest support,
#' along with the time ranges of available data.
#' @export
catalog_full_v2_market_open_interest <- function(markets = NULL,
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
    endpoint = "/catalog-all-v2/market-openinterest",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2MarketsData(catalog_data)
}


#' Supported Market Liquidations
#' @inheritParams catalog_v2_market_liquidations
#' @return Data table of all markets with liquidations support, along with the time ranges of available data.
#' @export
catalog_full_v2_market_liquidations <- function(markets = NULL,
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
    endpoint = "/catalog-all-v2/market-liquidations",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2MarketsData(catalog_data)
}

#' Supported Market Metrics
#' @inheritParams catalog_v2_market_metrics
#' @return Data table of all markets with market metrics support, 
#' along with the time ranges of available data per metric.
#' @export
catalog_full_v2_market_metrics <- function(markets = NULL,
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
    endpoint = "/catalog-all-v2/market-metrics",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2MarketMetricsData(catalog_data)
}

#' Supported Pair Candles
#' @inheritParams catalog_v2_pair_candles
#' @return Data table of all supported asset pair candles,
#' along with the time ranges of available data per candle duration.
#' @export
catalog_full_v2_pair_candles <- function(pairs = NULL,
                                         page_size = 2000,
                                         paging_from = "start") {
  query_args <- list(
    pairs = pairs,
    page_size = page_size,
    paging_from = paging_from
  )
  
  resp <- send_coinmetrics_request(
    endpoint = "/catalog-all-v2/pair-candles",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2PairCandlesData(catalog_data)
}

#' Supported Index Levels
#' @inheritParams catalog_v2_index_levels
#' @return Data table of all supported index levels, along with time ranges of available data.
#' @export
catalog_full_v2_index_levels <- function(indexes = NULL,
                                         page_size = 2000,
                                         paging_from = "start") {
  
  query_args <- list(
    indexes = indexes,
    page_size = page_size,
    paging_from = paging_from
  )
  
  resp <- send_coinmetrics_request(
    endpoint = "/catalog-all-v2/index-levels",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2IndexData(catalog_data)
}

#' Supported Index Candles
#' @inheritParams catalog_v2_index_candles
#' @return Data table of all supported index candles, 
#' along with time ranges of available data per candle duration.
#' @export
catalog_full_v2_index_candles <- function(indexes = NULL,
                                          page_size = 2000,
                                          paging_from = "start") {
  
  query_args <- list(
    indexes = indexes,
    page_size = page_size,
    paging_from = paging_from
  )
  
  resp <- send_coinmetrics_request(
    endpoint = "/catalog-all-v2/index-candles",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2IndexData(catalog_data)
}

#' Supported Asset Chains
#' @inheritParams catalog_v2_asset_chains
#' @return Data table of all supported assets for the asset-chains endpoint,
#'  along with time ranges of available data.
#' @export
catalog_full_v2_asset_chains <- function(assets = NULL,
                                         page_size = 2000,
                                         paging_from = "start") {
  
  query_args <- list(
    assets = assets,
    page_size = page_size,
    paging_from = paging_from
  )
  
  resp <- send_coinmetrics_request(
    endpoint = "/catalog-all-v2/asset-chains",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2MarketsData(catalog_data)
}

#' Supported Mempool Feerates
#' @inheritParams catalog_v2_mempool_feerates
#' @return Data table of all supported assets for the mempool-feerates endpoint,
#' along with time ranges of available data.
#' @export
catalog_full_v2_mempool_feerates <- function(assets = NULL,
                                             page_size = 2000,
                                             paging_from = "start") {
  
  query_args <- list(
    assets = assets,
    page_size = page_size,
    paging_from = paging_from
  )
  
  resp <- send_coinmetrics_request(
    endpoint = "/catalog-all-v2/mempool-feerates",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2MarketsData(catalog_data)
}

#' Supported Mining Pool Tips Summaries
#' @inheritParams catalog_v2_mining_pool_tips_summaries
#' @return Data table of all supported assets for the mining-pool-tips-summary endpoint,
#' along with time ranges of available data.
#' @export
catalog_full_v2_mining_pool_tips_summaries <- function(assets = NULL,
                                                       page_size = 2000,
                                                       paging_from = "start") {
  
  query_args <- list(
    assets = assets,
    page_size = page_size,
    paging_from = paging_from
  )
  
  resp <- send_coinmetrics_request(
    endpoint = "/catalog-all-v2/mining-pool-tips-summary",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2MarketsData(catalog_data)
}

#' Supported Transaction Tracker Assets
#' @inheritParams catalog_v2_transaction_tracker_assets
#' @return Data table of all supported assets for the transaction-tracker endpoint,
#' along with time ranges of available data.
#' @export
catalog_full_v2_transaction_tracker_assets <- function(assets = NULL,
                                                       page_size = 2000,
                                                       paging_from = "start") {
  
  query_args <- list(
    assets = assets,
    page_size = page_size,
    paging_from = paging_from
  )
  
  resp <- send_coinmetrics_request(
    endpoint = "/catalog-all-v2/transaction-tracker",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2MarketsData(catalog_data)
}