#' Supported Assets
#' @param assets Vector of assets. By default, all assets are returned.
#' @return Tibble of supported assets along with information for them like metrics, markets, exchanges, and time ranges of available data.
#' Metrics, markets, and exchanges outputted as list-columns.
#' @export
catalog_full_assets <- function(assets = NULL) {
  query_args <- list(assets = assets)

  resp <- send_coinmetrics_request("catalog-all/assets", query_args)
  
  catalogAssetsData(resp)
}

#' Supported Asset Metrics
#' @param metrics Vector of metrics. By default, all metrics are returned.
#' @param reviewable Limit to human-reviewable metrics. By default, all metrics are returned.
#' @param pretty Human-readable formatting of JSON responses.
#' @return Tibble of supported asset metrics along with information for them
#' like description, category, and assets for which a metric is available.
#' @export
catalog_full_metrics <- function(metrics = NULL, reviewable = NULL, pretty = FALSE) {
  query_args <- list(
    metrics = metrics,
    reviewable = reviewable,
    pretty = pretty
  )

  resp <- send_coinmetrics_request("catalog-all/asset-metrics", query_args)
  api_data <- httr::content(resp)[["data"]]

  api_data %>%
    data.table::rbindlist(fill = TRUE) %>%
    tidyr::unnest_wider("frequencies")
}

## Note, catalog-all/exchange-metrics returns nothing
#' Supported Exchange Metrics
#' @inheritParams catalog_full_metrics
#' @return Tibble of supported exchange metrics along with information for them
#' like description, category, and exchanges for which a metric is available.
catalog_full_exchange_metrics <- function(metrics = NULL, reviewable = NULL, pretty = FALSE) {
  query_args <- list(
    metrics = metrics,
    reviewable = reviewable,
    pretty = pretty
  )

  resp <- send_coinmetrics_request("catalog-all/exchange-metrics", query_args)
  api_data <- httr::content(resp)[["data"]]

  # api_data %>%
  #   data.table::rbindlist(fill = TRUE) %>%
  #   #tidyr::unnest_wider("frequencies")
  #   tibble::as_tibble()
  api_data
}

#' Supported Institution Metrics
#' @inheritParams catalog_full_metrics
#' @return Tibble of supported institution metrics along with information for them
#' like description, category, and institutions for which a metric is available.
# NOTE, this is also empty
catalog_full_institution_metrics <- function(metrics = NULL, reviewable = NULL, pretty = FALSE) {
  query_args <- list(metrics = metrics, reviewable = reviewable, pretty = pretty)

  resp <- send_coinmetrics_request("catalog-all/institution-metrics", query_args)
  api_data <- httr::content(resp)[["data"]]

  api_data %>%
    data.table::rbindlist(fill = TRUE)
}

#' Supported Exchanges
#' @param exchanges Vector of exchanges. By default, all exchanges are returned.
#' @param pretty Human-readable formatting of JSON responses.
#' @return Tibble of all supported exchanges along with available markets for them.
#' @export
catalog_full_exchanges <- function(exchanges = NULL, pretty = FALSE) {
  resp <- send_coinmetrics_request(
    endpoint = "catalog-all/exchanges",
    query_args = list(exchanges = exchanges, pretty = pretty)
  )
  api_data <- httr::content(resp)[["data"]]

  tibble::tibble(
    exchange = purrr::map_chr(api_data, "exchange", .default = NA),
    markets = purrr::map(api_data, "markets", .default = NA),
    min_time = anytime::anytime(purrr::map_chr(api_data, "min_time", .default = NA)),
    max_time = anytime::anytime(purrr::map_chr(api_data, "max_time", .default = NA))
  ) %>%
    tidyr::unnest_longer("markets")
}

#' Supported Exchange-Asset Pairs
#' @param exchange_assets Vector of exchange-assets. By default, all exchange-asset pairs are returned.
#' @param pretty Human-readable formatting of JSON responses.
#' @return Tibble of all supported exchange-asset pairs along with information for them
#' like metrics and time ranges of available data.
#' @export
catalog_full_exchange_assets <- function(exchange_assets = NULL, pretty = FALSE) {
  resp <- send_coinmetrics_request(
    endpoint = "catalog-all/exchange-assets",
    query_args = list(exchange_assets = exchange_assets, pretty = pretty)
  )
  api_data <- httr::content(resp)[["data"]]

  api_data %>%
    data.table::rbindlist(fill = TRUE) %>%
    tidyr::unnest_wider("metrics") %>%
    tidyr::unnest("frequencies") %>%
    tidyr::hoist(
      "frequencies",
      "frequency",
      "min_time",
      "max_time",
      .transform = list(min_time = anytime::anytime, max_time = anytime::anytime)
    )
}

#' Supported Asset Pairs
#' @param pairs Vector of asset pairs. By default, all asset pairs are returned.
#' @param pretty Human-readable formatting of JSON responses.
#' @return Tibble of supported asset pairs along with information for them like
#' metrics and time ranges of available data.
#' @export
catalog_full_asset_pairs <- function(pairs = NULL, pretty = FALSE) {
  resp <- send_coinmetrics_request(
    endpoint = "catalog-all/pairs",
    query_args = list(pairs = pairs, pretty = pretty)
  )
  api_data <- httr::content(resp)[["data"]]

  api_data %>%
    data.table::rbindlist(fill = TRUE) %>%
    tidyr::unnest_wider("metrics") %>%
    tidyr::unnest("frequencies") %>%
    tidyr::hoist(
      "frequencies",
      "frequency",
      "min_time",
      "max_time",
      .transform = list(min_time = anytime::anytime, max_time = anytime::anytime)
    )
}

#' Supported Asset Pair Candles
#' @inheritParams catalog_full_asset_pairs
#' @return Tibble of supported asset pair candles along with the time ranges of available data per candle duration.
#' @export
catalog_full_asset_pair_candles <- function(pairs = NULL, pretty = FALSE) {
  resp <- send_coinmetrics_request(
    endpoint = "catalog-all/pair-candles",
    query_args = list(pairs = pairs, pretty = pretty)
  )
  api_data <- httr::content(resp)[["data"]]

  api_data %>%
    data.table::rbindlist(fill = TRUE) %>%
    tidyr::unnest_wider("frequencies") %>%
    dplyr::mutate(
      dplyr::across(c("min_time", "max_time"), anytime::anytime)
    )
}

#' Supported Institutions
#' @param institutions Vector of institutions. By default, all institutions are returned.
#' @param pretty Human-readable formatting of JSON responses.
#' @return Tibble of supported institutions along with information for them like metrics and time ranges of available data.
#' @export
catalog_full_institutions <- function(institutions = NULL, pretty = FALSE) {
  resp <- send_coinmetrics_request(
    endpoint = "catalog-all/institutions",
    query_args = list(institutions = institutions, pretty = pretty)
  )
  api_data <- httr::content(resp)[["data"]]

  api_data %>%
    data.table::rbindlist(fill = TRUE) %>%
    tidyr::unnest_wider("metrics") %>%
    tidyr::unnest_longer("frequencies") %>%
    tidyr::hoist(
      "frequencies", "frequency", "min_time", "max_time",
      .transform = list(
        min_time = anytime::anytime,
        max_time = anytime::anytime
      )
    )
}

#' Supported Markets
#' @inheritParams catalog_markets
#' @return List of supported markets along with time ranges of available data.
#' @export
catalog_full_markets <- function(markets = NULL,
                                 exchange = NULL,
                                 type = NULL,
                                 base = NULL,
                                 quote = NULL,
                                 asset = NULL,
                                 symbol = NULL) {
  query_args <- list(
    markets = markets,
    exchange = exchange,
    type = type,
    base = base,
    quote = quote,
    asset = asset,
    symbol = symbol
  )

  resp <- send_coinmetrics_request("catalog-all/markets", query_args)
  
  catalogMarketsData(resp)
}

#' Supported Market Trades
#' @inheritParams catalog_markets
#' @return Tibble of all markets with trades support along with the time ranges of available data.
#' @export
catalog_full_market_trades <- function(markets = NULL,
                                       exchange = NULL,
                                       type = NULL,
                                       base = NULL,
                                       quote = NULL,
                                       asset = NULL,
                                       symbol = NULL) {
  query_args <- list(
    markets = markets,
    exchange = exchange,
    type = type,
    base = base,
    quote = quote,
    asset = asset,
    symbol = symbol
  )

  resp <- send_coinmetrics_request(endpoint = "catalog-all/market-trades", query_args = query_args)

  get_coinmetrics_api_data(resp, "market-trades", "end", as_list = F)
}

#' Supported Market Candles
#' @inheritParams catalog_markets
#' @param as_list Return content as list instead of tibble.
#' @return List of all markets with candles support along with the time ranges of available data per candle duration.
#' @export
catalog_full_market_candles <- function(markets = NULL,
                                        exchange = NULL,
                                        type = NULL,
                                        base = NULL,
                                        quote = NULL,
                                        asset = NULL,
                                        symbol = NULL,
                                        as_list = TRUE) {
  query_args <- list(
    markets = markets,
    exchange = exchange,
    type = type,
    base = base,
    quote = quote,
    asset = asset,
    symbol = symbol
  )

  resp <- send_coinmetrics_request("catalog-all/market-candles", query_args)
  api_data <- httr::content(resp)["data"]

  if (isTRUE(as_list)) {
    return(api_data)
  }

  api_data %>%
    data.table::rbindlist(fill = TRUE) %>%
    tidyr::unnest_wider("frequencies") %>%
    dplyr::mutate(
      dplyr::across(c("min_time", "max_time"), anytime::anytime)
    )
}

#' Supported Market Orderbooks
#' @inheritParams catalog_markets
#' @return Tibble of all markets with orderbooks support along with the time ranges of available data.
#' @export
catalog_full_market_orderbooks <- function(markets = NULL,
                                           exchange = NULL,
                                           type = NULL,
                                           base = NULL,
                                           quote = NULL,
                                           asset = NULL,
                                           symbol = NULL) {
  query_args <- list(
    markets = markets,
    exchange = exchange,
    type = type,
    base = base,
    quote = quote,
    asset = asset,
    symbol = symbol
  )

  resp <- send_coinmetrics_request("catalog-all/market-orderbooks", query_args)
  api_data <- httr::content(resp)[["data"]]

  api_data %>%
    data.table::rbindlist(fill = TRUE) %>%
    tidyr::hoist(
      "depths", "depth", "min_time", "max_time",
      .transform = list(min_time = anytime::anytime, max_time = anytime::anytime)
    ) %>%
    tibble::as_tibble()
}

#' Supported Market Quotes
#' @inheritParams catalog_markets
#' @return Tibble of markets with quotes support along with the time ranges of available data.
#' @export
catalog_full_market_quotes <- function(markets = NULL,
                                       exchange = NULL,
                                       type = NULL,
                                       base = NULL,
                                       quote = NULL,
                                       asset = NULL,
                                       symbol = NULL) {
  query_args <- list(
    markets = markets,
    exchange = exchange,
    type = type,
    base = base,
    quote = quote,
    asset = asset,
    symbol = symbol
  )

  resp <- send_coinmetrics_request("catalog-all/market-quotes", query_args)
  api_data <- httr::content(resp)[["data"]]

  tibble::tibble(
    market = purrr::map_chr(api_data, "market", .default = NA),
    min_time = anytime::anytime(purrr::map_chr(api_data, "min_time", .default = NA)),
    max_time = anytime::anytime(purrr::map_chr(api_data, "max_time", .default = NA))
  )
}

#' Supported Market Funding Rates
#' @inheritParams catalog_markets
#' @return Tibble of all markets with funding rates support along with the time ranges of available data.
#' @export
catalog_full_market_funding_rates <- function(markets = NULL,
                                              exchange = NULL,
                                              type = NULL,
                                              base = NULL,
                                              quote = NULL,
                                              asset = NULL,
                                              symbol = NULL) {
  query_args <- list(
    markets = markets,
    exchange = exchange,
    type = type,
    base = base,
    quote = quote,
    asset = asset,
    symbol = symbol
  )

  resp <- send_coinmetrics_request("catalog-all/market-funding-rates", query_args)
  api_data <- httr::content(resp)[["data"]]

  tibble::tibble(
    market = purrr::map_chr(api_data, "market", .default = NA),
    min_time = anytime::anytime(purrr::map_chr(api_data, "min_time", .default = NA)),
    max_time = anytime::anytime(purrr::map_chr(api_data, "max_time", .default = NA))
  )
}

#' Supported Market Greeks
#' @inheritParams catalog_markets
#' @return Tibble of all market greeks for option market.
#' @export
catalog_full_greeks <- function(markets = NULL,
                                exchange = NULL,
                                type = NULL,
                                base = NULL,
                                quote = NULL,
                                asset = NULL,
                                symbol = NULL) {
  query_args <- list(
    markets = markets,
    exchange = exchange,
    type = type,
    base = base,
    quote = quote,
    asset = asset,
    symbol = symbol
  )

  resp <- send_coinmetrics_request("catalog-all/market-greeks", query_args)

  get_coinmetrics_api_data(resp, "market-greeks", "end")
}

#' Supported Market Open Interest
#' @inheritParams catalog_markets
#' @return Tibble of all markets with open interest support along with the time ranges of available data.
#' @export
catalog_full_market_openinterest <- function(markets = NULL,
                                             exchange = NULL,
                                             type = NULL,
                                             base = NULL,
                                             quote = NULL,
                                             asset = NULL,
                                             symbol = NULL) {
  query_args <- list(
    markets = markets,
    exchange = exchange,
    type = type,
    base = base,
    quote = quote,
    asset = asset,
    symbol = symbol
  )

  resp <- send_coinmetrics_request("catalog-all/market-openinterest", query_args)

  get_coinmetrics_api_data(resp, "market-openinterest", "end")
}

#' Supported Market Liquidations
#' @inheritParams catalog_markets
#' @return Tibble of all markets with liquidations support along with the time ranges of available data.
#' @export
catalog_full_market_liquidations <- function(markets = NULL,
                                             exchange = NULL,
                                             type = NULL,
                                             base = NULL,
                                             quote = NULL,
                                             asset = NULL,
                                             symbol = NULL) {
  query_args <- list(
    markets = markets,
    exchange = exchange,
    type = type,
    base = base,
    quote = quote,
    asset = asset,
    symbol = symbol
  )

  resp <- send_coinmetrics_request("catalog-all/market-liquidations", query_args)

  get_coinmetrics_api_data(resp, "market-liquidations", "end")
}

#' Supported Market Metrics
#' @inheritParams catalog_markets
#' @return List of all markets with metrics support along with the time ranges of available data per metric.
#' @export
catalog_full_market_metrics <- function(markets = NULL,
                                        exchange = NULL,
                                        type = NULL,
                                        base = NULL,
                                        quote = NULL,
                                        asset = NULL,
                                        symbol = NULL) {
  query_args <- list(
    markets = markets,
    exchange = exchange,
    type = type,
    base = base,
    quote = quote,
    asset = asset,
    symbol = symbol
  )

  resp <- send_coinmetrics_request(endpoint = "catalog-all/market-metrics", query_args = query_args)
  
  catalogMarketMetricsData(resp)
}

#' Supported Indexes
#' @inheritParams catalog_indexes
#' @return Tibble of all supported indexes along with time ranges of available data.
#' @export
catalog_full_indexes <- function(indexes = NULL) {
  resp <- send_coinmetrics_request("catalog-all/indexes", list(indexes = indexes))
  api_data <- httr::content(resp)[["data"]]

  api_data %>%
    data.table::rbindlist(fill = TRUE) %>%
    tidyr::unnest_wider("frequencies") %>%
    dplyr::mutate(
      dplyr::across(c("min_time", "max_time"), anytime::anytime)
    )
}

#' Supported Index Candles
#' @inheritParams catalog_indexes
#' @return Tibble of all supported index candles along with the time ranges of available data per index candle duration.
#' @export
catalog_full_index_candles <- function(indexes = NULL) {
  resp <- send_coinmetrics_request("catalog-all/index-candles", list(indexes = indexes))
  api_data <- httr::content(resp)[["data"]]

  api_data %>%
    data.table::rbindlist(fill = TRUE) %>%
    tidyr::unnest_wider("frequencies") %>%
    dplyr::mutate(
      dplyr::across(c("min_time", "max_time"), anytime::anytime)
    )
}

#' Supported Asset Alerts
#' @param assets Vector of assets. By default, all assets are returned.
#' @param alerts Vector of alert names. By default, all asset alerts are returned.
#' @return Tibble of all supported asset alerts along with their descriptions, thresholds, and constituents.
#' @export
catalog_full_asset_alerts <- function(assets = NULL, alerts = NULL) {
  query_args <- list(assets = assets, alerts = alerts)

  resp <- send_coinmetrics_request(endpoint = "catalog-all/alerts", query_args = query_args)
  api_data <- httr::content(resp)[["data"]]

  api_data %>%
    data.table::rbindlist(fill = TRUE) %>%
    tidyr::hoist(
      .data$conditions,
      "description",
      "threshold",
      "constituents"
    ) %>%
    tidyr::unnest_longer(.data$constituents)
}
