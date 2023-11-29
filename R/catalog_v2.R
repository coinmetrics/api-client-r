#' Available Asset Metrics
#' @param assets Character vector of assets. By default, all assets are returned.
#' @param page_size Number of items per single page of results, default is `100`.
#' @param paging_from Enum: `start` or `end`. Where does the first page start, at the start of the interval or at the end.
#' @inheritParams catalog_asset_metrics
#' @return Data table of available asset metrics, along with the time ranges of available data.
#' @export
catalog_v2_asset_metrics <- function(assets = NULL,
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
    endpoint = "/catalog-v2/asset-metrics",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2MetricsData(catalog_data)
}

#' Available Exchange Metrics
#' @inheritParams catalog_v2_asset_metrics
#' @param exchanges Character vector of exchanges. By default, all exchanges are returned.
#' @return Data table of available exchange metrics, along with the time ranges of available data.
#' @export
catalog_v2_exchange_metrics <- function(exchanges = NULL,
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
    endpoint = "/catalog-v2/exchange-metrics",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2MetricsData(catalog_data)
}

#' Available Exchange-Asset Metrics
#' @inheritParams catalog_v2_asset_metrics
#' @param exchange_assets Character vector of exchange-assets. By default, all exchange-asset pairs are returned.
#' @return Data table of available exchange-asset metrics, along with the time ranges of available data.
#' @export
catalog_v2_exchange_asset_metrics <- function(exchange_assets = NULL,
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
    endpoint = "/catalog-v2/exchange-asset-metrics",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2MetricsData(catalog_data)
}

#' Available Pair Metrics
#' @inheritParams catalog_v2_asset_metrics
#' @param pairs Character vector of asset pairs. By default, all asset pairs are returned.
#' @return Data table of available pair metrics, along with the time ranges of available data.
#' @export
catalog_v2_pair_metrics <- function(pairs = NULL,
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
    endpoint = "/catalog-v2/pair-metrics",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2MetricsData(catalog_data)
}

#' Available Institutions Metrics
#' @inheritParams catalog_v2_asset_metrics
#' @param institutions Character vector of institutions. By default, all institutions are returned.
#' @return Data table of institutions, along with the time ranges of available data.
#' @export
catalog_v2_institution_metrics <- function(institutions = NULL,
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
    endpoint = "/catalog-v2/institution-metrics",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2MetricsData(catalog_data)
}

#' Available Market Trades
#' @inheritParams reference_data_markets
#' @return Data table of available markets with trades support, along with the time ranges of available data.
#' @export
catalog_v2_market_trades <- function(markets = NULL,
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
    endpoint = "/catalog-v2/market-trades",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2MarketsData(catalog_data)
}

#' Available Market Candles
#' @inheritParams reference_data_markets
#' @return Data table of available markets with candles support, 
#' along with the time ranges of available data per candle duration.
#' @export
catalog_v2_market_candles <- function(markets = NULL,
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
    endpoint = "/catalog-v2/market-candles",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2MarketCandlesData(catalog_data)
}

#' Available Market Orderbooks
#' @inheritParams reference_data_markets
#' @return Data table of available markets with orderbooks support, along with the time ranges of available data.
#' @export
catalog_v2_market_orderbooks <- function(markets = NULL,
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
    endpoint = "/catalog-v2/market-orderbooks",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2OrderbooksData(catalog_data)
}

#' Available Market Quotes
#' @inheritParams reference_data_markets
#' @return Data table of available markets with quotes support, along with the time ranges of available data.
#' @export
catalog_v2_market_quotes <- function(markets = NULL,
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
    endpoint = "/catalog-v2/market-quotes",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2MarketsData(catalog_data)
}

#' Available Market Funding Rates
#' @inheritParams reference_data_markets
#' @return Data table of available markets with funding rates support, along with the time ranges of available data.
#' @export
catalog_v2_market_funding_rates <- function(markets = NULL,
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
    endpoint = "/catalog-v2/market-funding-rates",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2MarketsData(catalog_data)
}

#' Available Market Contract Prices
#' @inheritParams reference_data_markets
#' @return Data table of contract prices for option market.
#' @export
catalog_v2_market_contract_prices <- function(markets = NULL,
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
    endpoint = "/catalog-v2/market-contract-prices",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2MarketsData(catalog_data)
}

#' Available Market Implied Volatility
#' @inheritParams reference_data_markets
#' @return Data table of implied volatility for option market.
#' @export
catalog_v2_market_implied_volatility <- function(markets = NULL,
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
    endpoint = "/catalog-v2/market-implied-volatility",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2MarketsData(catalog_data)
}

#' Available Market Greeks
#' @inheritParams reference_data_markets
#' @return Data table of market greeks volatility for option market.
#' @export
catalog_v2_market_greeks <- function(markets = NULL,
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
    endpoint = "/catalog-v2/market-greeks",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2MarketsData(catalog_data)
}

#' Available Market Open Interest
#' @inheritParams reference_data_markets
#' @return Data table of markets with open interest support,
#' along with the time ranges of available data.
#' @export
catalog_v2_market_open_interest <- function(markets = NULL,
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
    endpoint = "/catalog-v2/market-openinterest",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2MarketsData(catalog_data)
}

#' Available Market Liquidations
#' @inheritParams reference_data_markets
#' @return Data table of markets with liquidations support, along with the time ranges of available data.
#' @export
catalog_v2_market_liquidations <- function(markets = NULL,
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
    endpoint = "/catalog-v2/market-liquidations",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2MarketsData(catalog_data)
}

#' Available Market Metrics
#' @inheritParams reference_data_markets
#' @return Data table of markets with metrics support, 
#' along with the time ranges of available data per metric.
#' @export
catalog_v2_market_metrics <- function(markets = NULL,
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
    endpoint = "/catalog-v2/market-metrics",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2MarketMetricsData(catalog_data)
}

#' Available Pair Candles
#' @inheritParams catalog_v2_pair_metrics
#' @return Data table of available asset pair candles,
#' along with the time ranges of available data per candle duration.
#' @export
catalog_v2_pair_candles <- function(pairs = NULL,
                                    page_size = 2000,
                                    paging_from = "start") {
  query_args <- list(
    pairs = pairs,
    page_size = page_size,
    paging_from = paging_from
  )
  
  resp <- send_coinmetrics_request(
    endpoint = "/catalog-v2/pair-candles",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2PairCandlesData(catalog_data)
}

#' Available Index Levels
#' @inheritParams reference_data_indexes
#' @return Data table of available index levels, along with time ranges of available data.
#' @export
catalog_v2_index_levels <- function(indexes = NULL,
                                    page_size = 2000,
                                    paging_from = "start") {
  
  query_args <- list(
    indexes = indexes,
    page_size = page_size,
    paging_from = paging_from
  )
  
  resp <- send_coinmetrics_request(
    endpoint = "/catalog-v2/index-levels",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2IndexData(catalog_data)
}

#' Available Index Candles
#' @inheritParams reference_data_indexes
#' @return Data table of available index candles, 
#' along with time ranges of available data per candle duration.
#' @export
catalog_v2_index_candles <- function(indexes = NULL,
                                     page_size = 2000,
                                     paging_from = "start") {
  
  query_args <- list(
    indexes = indexes,
    page_size = page_size,
    paging_from = paging_from
  )
  
  resp <- send_coinmetrics_request(
    endpoint = "/catalog-v2/index-candles",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2IndexData(catalog_data)
}

#' Available Asset Chains
#' @inheritParams reference_data_assets
#' @return Data table of available assets for the asset-chains endpoint,
#'  along with time ranges of available data.
#' @export
catalog_v2_asset_chains <- function(assets = NULL,
                                    page_size = 2000,
                                    paging_from = "start") {
  
  query_args <- list(
    assets = assets,
    page_size = page_size,
    paging_from = paging_from
  )
  
  resp <- send_coinmetrics_request(
    endpoint = "/catalog-v2/asset-chains",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2MarketsData(catalog_data)
}

#' Available Mempool Feerates
#' @inheritParams reference_data_assets
#' @return Data table of available assets for the mempool-feerates endpoint,
#' along with time ranges of available data.
#' @export
catalog_v2_mempool_feerates <- function(assets = NULL,
                                        page_size = 2000,
                                        paging_from = "start") {
  
  query_args <- list(
    assets = assets,
    page_size = page_size,
    paging_from = paging_from
  )
  
  resp <- send_coinmetrics_request(
    endpoint = "/catalog-v2/mempool-feerates",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2MarketsData(catalog_data)
}

#' Available Mining Pool Tips Summaries
#' @inheritParams reference_data_assets
#' @return Data table of available assets for the mining-pool-tips-summary endpoint,
#' along with time ranges of available data.
#' @export
catalog_v2_mining_pool_tips_summaries <- function(assets = NULL,
                                                  page_size = 2000,
                                                  paging_from = "start") {
  
  query_args <- list(
    assets = assets,
    page_size = page_size,
    paging_from = paging_from
  )
  
  resp <- send_coinmetrics_request(
    endpoint = "/catalog-v2/mining-pool-tips-summary",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2MarketsData(catalog_data)
}

#' Available Transaction Tracker Assets
#' @inheritParams reference_data_assets
#' @return Data table of available assets for the transaction-tracker endpoint,
#' along with time ranges of available data.
#' @export
catalog_v2_transaction_tracker_assets <- function(assets = NULL,
                                                  page_size = 2000,
                                                  paging_from = "start") {
  
  query_args <- list(
    assets = assets,
    page_size = page_size,
    paging_from = paging_from
  )
  
  resp <- send_coinmetrics_request(
    endpoint = "/catalog-v2/transaction-tracker",
    query_args = query_args
  )
  catalog_data <- get_catalog_v2_data(resp, paging_from)
  
  catalogV2MarketsData(catalog_data)
}