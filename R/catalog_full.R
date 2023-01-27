#' Supported Assets
#' @param assets Vector of assets. By default, all assets are returned.
#' @param include Vector of fields to include in response. Supported values are `metrics`, `markets`, `exchanges`. 
#' Included by default if omitted.,
#' @param exclude Vector of fields to exclude in response. Supported values are `metrics`, `markets`, `exchanges`. 
#' Included by default if omitted.
#' @param pretty Human-readable formatting of JSON responses.
#' @return Tibble of supported assets along with information for them like metrics, markets, exchanges, and time ranges of available data.
#' Metrics, markets, and exchanges outputted as list-columns.
#' @export
catalog_full_assets <- function(assets = NULL, include = NULL, exclude = NULL, pretty = FALSE) {
  
  query_args <- list(
    assets = assets,
    include = include,
    exclude = exclude,
    pretty = pretty
  )
  
  resp <- send_coinmetrics_request("catalog-all/assets", query_args)
  
  api_data <- httr::content(resp)[["data"]]
  
  catalog <- tibble::tibble(
    asset = purrr::map_chr(api_data, "asset", .default = NA),
    full_name = purrr::map_chr(api_data, "full_name", .default = NA),
    metrics = purrr::map(api_data, "metrics", .default = NA),
    markets = purrr::map(api_data, "markets", .default = NA),
    exchanges = purrr::map(api_data, "exchanges", .default = NA)
  )
  
  all.names <- Reduce(union, sapply(api_data, names))
  
  catalog[, which(colnames(catalog) %in% all.names)]
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
catalog_full_exchange_metrics <- function(metrics=NULL, reviewable=NULL, pretty=FALSE) {
  
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
catalog_full_institution_metrics <- function(metrics=NULL, reviewable=NULL, pretty=FALSE) {
  
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
catalog_full_exchanges <- function(exchanges=NULL, pretty=FALSE) {
  
  resp <- send_coinmetrics_request(endpoint = "catalog-all/exchanges",
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
catalog_full_exchange_assets <- function(exchange_assets=NULL, pretty=FALSE) {
  
  resp <- send_coinmetrics_request(
    endpoint = "catalog-all/exchange-assets",
    query_args = list(exchange_assets=exchange_assets, pretty=pretty)
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
catalog_full_asset_pairs <- function(pairs=NULL, pretty=FALSE) {
  
  resp <- send_coinmetrics_request(
    endpoint = "catalog-all/pairs",
    query_args = list(pairs=pairs, pretty=pretty)
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
catalog_full_asset_pair_candles <- function(pairs=NULL, pretty=FALSE) {
  
  resp <- send_coinmetrics_request(
    endpoint = "catalog-all/pair-candles",
    query_args = list(pairs=pairs, pretty=pretty)
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
catalog_full_institutions <- function(institutions=NULL, pretty=FALSE) {
  
  resp <- send_coinmetrics_request(
    endpoint = "catalog-all/institutions",
    query_args = list(institutions=institutions, pretty=pretty)
  )
  api_data <- httr::content(resp)[["data"]]
  
  api_data %>%
    data.table::rbindlist(fill = TRUE) %>%
    tidyr::unnest_wider("metrics") %>%
    tidyr::unnest_longer("frequencies") %>%
    tidyr::hoist(
      "frequencies", "frequency", "min_time", "max_time",
      .transform = list(min_time = anytime::anytime,
                        max_time = anytime::anytime)
    )
}

#' Supported Markets
#' @inheritParams catalog_markets
#' @return List of supported markets along with time ranges of available data.
#' @export
catalog_full_markets <- function(markets=NULL,
                                 exchange=NULL,
                                 type=NULL,
                                 base=NULL,
                                 quote=NULL,
                                 asset=NULL,
                                 symbol=NULL,
                                 include=NULL,
                                 exclude=NULL,
                                 format="json",
                                 limit=NULL,
                                 pretty=FALSE) {
  
  query_args <- list(
    markets = markets,
    exchange = exchange,
    type = type,
    base = base,
    quote = quote,
    asset = asset,
    symbol = symbol,
    include = include,
    exclude = exclude,
    format = format,
    limit = limit,
    pretty = pretty
  )
  
  resp <- send_coinmetrics_request("catalog-all/markets", query_args)
  api_data <- httr::content(resp)[["data"]]
  
  return(api_data)
}

#' Supported Market Trades
#' @inheritParams catalog_markets
#' @return Tibble of all markets with trades support along with the time ranges of available data.
#' @export
catalog_full_market_trades <- function(markets=NULL,
                                       exchange=NULL,
                                       type=NULL,
                                       base=NULL,
                                       quote=NULL,
                                       asset=NULL,
                                       symbol=NULL,
                                       format="json",
                                       limit=NULL,
                                       pretty=FALSE) {
  
  query_args <- list(
    markets = markets,
    exchange = exchange,
    type = type,
    base = base,
    quote = quote,
    asset = asset,
    symbol = symbol,
    format = format,
    limit = limit,
    pretty = pretty
  )
  
  resp <- send_coinmetrics_request(endpoint = "catalog-all/market-trades", query_args = query_args)
  
  get_coinmetrics_api_data(resp, "market-trades", "end", as_list=F)
  
}

#' Supported Market Candles
#' @inheritParams catalog_markets
#' @return Tibble of all markets with candles support along with the time ranges of available data per candle duration.
#' @export
catalog_full_market_candles <- function(markets=NULL,
                                        exchange=NULL,
                                        type=NULL,
                                        base=NULL,
                                        quote=NULL,
                                        asset=NULL,
                                        symbol=NULL,
                                        format="json",
                                        limit=NULL,
                                        pretty=FALSE) {
  
}