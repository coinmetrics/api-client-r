#' Available Assets
#'
#' @param assets Vector of assets. By default all assets are returned.
#' @param include Vector of fields to include in response. Supported values are `metrics`, `markets`, `exchanges`. Included by default if `NULL`.
#' @param exclude Vector of fields to exclude from response. Supported values are `metrics`, `markets`, `exchanges`. Included by default if `NULL`.
#' @return List of available assets along with information for them (metrics, markets, and exchanges) and time ranges of available data.
#' @export
get_catalog_assets <- function(assets = NULL,
                               include = NULL,
                               exclude = NULL) {
  query_args <- list(
    assets = assets,
    include = include,
    exclude = exclude
  )

  resp <- send_coinmetrics_request(endpoint = "catalog/assets", query_args = query_args)

  assets_content <- httr::content(resp)[["data"]]

  return(assets_content)
}

#' Available Asset Pairs
#' @param pairs Vector of asset pairs. By default, all asset pairs are returned.
#' @param as_list Return list instead of tibble.
#' @return List of available asset pairs, along with key information like metrics and time ranges of available data.
#' @export
get_catalog_asset_pairs <- function(pairs = NULL, as_list = TRUE) {
  resp <- send_coinmetrics_request(endpoint = "catalog/pairs", query_args = list(pairs = pairs))

  pairs_content <- httr::content(resp)[["data"]]

  if (as_list) {
    return(pairs_content)
  } else {
    pairs_tbl <-
      data.table::rbindlist(pairs_content) %>%
      tidyr::hoist(metrics, "metric", "frequencies") %>%
      tidyr::unnest_longer(frequencies) %>%
      tidyr::hoist(
        frequencies,
        "frequency",
        "min_time",
        "max_time",
        .transform = list(min_time = anytime::anytime, max_time = anytime::anytime)
      )

    return(pairs_tbl)
  }
}


#' Get Available Asset Metrics
#' @param metrics Vector of metrics. By default, all metrics are returned.
#' @param reviewable Boolean to limit to human-reviewable metrics. By default, all metrics are returned.
#' @param pretty Human-readable formatting of JSON responses.
#' @param as_list Return list instead of tibble.
#' @return Tibble of available asset metrics along with information like description, category, and assets for which a metric is available.
#' @export
get_catalog_asset_metrics <- function(metrics = NULL, reviewable = TRUE, pretty = FALSE, as_list = TRUE) {
  query_args <- list(metrics = metrics, reviewable = reviewable, pretty = pretty)

  resp <- send_coinmetrics_request(endpoint = "catalog/asset-metrics", query_args = query_args)

  metrics_content <- httr::content(resp)[["data"]]

  if (as_list) {
    return(metrics_content)
  } else {
    metrics_tbl <-
      data.table::rbindlist(metrics_content) %>%
      tidyr::hoist(frequencies, "frequency", "assets") %>%
      tidyr::unnest_longer(assets) %>%
      tibble::as_tibble()

    return(metrics_tbl)
  }
}

#' Get Available Exchange/Asset Pairs
#' @param exchange_assets Vector of exchange-assets. By default, all exchange-asset pairs are returned.
#' @param as_list Return content as list instead of tibble.
#' @return Tibble of Available exchange-asset pairs along with information like metrics and time ranges of available data.
#' @export
get_catalog_exchange_assets <- function(exchange_assets = NULL, as_list = TRUE) {
  query_args <- list(exchange_assets = exchange_assets)

  resp <- send_coinmetrics_request(endpoint = "catalog/exchange-assets", query_args = query_args)

  ea_content <- httr::content(resp)[["data"]]

  if (as_list) {
    return(ea_content)
  } else {
    ea <- data.table::rbindlist(ea_content, fill = TRUE) %>%
      tidyr::hoist(metrics, "metric", "frequencies") %>%
      tidyr::unnest_longer(frequencies) %>%
      tidyr::hoist(
        frequencies,
        "frequency",
        "min_time",
        "max_time",
        .transform = list(min_time = anytime::anytime, max_time = anytime::anytime)
      ) %>%
      tibble::as_tibble()

    return(ea)
  }
}

#' Get Available Indexes
#' @param indexes Vector of index names. By default, all indexes are returned.
#' @return Tibble of available indexes along with time ranges of available data
#' @export
get_catalog_indexes <- function(indexes = NULL) {
  resp <- send_coinmetrics_request(endpoint = "catalog/indexes", query_args = list(indexes = indexes))
  ix_content <- httr::content(resp)[["data"]]

  data.table::rbindlist(ix_content, fill = TRUE) %>%
    tidyr::unnest_wider(frequencies) %>%
    dplyr::mutate(
      min_time = anytime::anytime(min_time),
      max_time = anytime::anytime(max_time)
    )
}

#' Get Available Pair Metrics
#' @param metrics Vector of metrics. By default all metrics are returned.
#' @return Available pair metrics along with information for them like descripion, category, and pairs for which a metric is available.
#' @export
get_catalog_pair_metrics <- function(metrics = NULL, as_list = FALSE) {
  query_args <- list(metrics = metrics)
  resp <- send_coinmetrics_request(endpoint = "catalog/pair-metrics", query_args = query_args)

  api_data <- httr::content(resp)[["data"]]

  if (as_list) {
    return(api_data)
  }

  api_data %>%
    data.table::rbindlist(fill = TRUE) %>%
    tidyr::hoist(frequencies, "frequency", "pairs") %>%
    tidyr::unnest_longer(pairs) %>%
    tibble::as_tibble()
}

#' Get Available Institution Metrics
#' @inheritParams get_catalog_pair_metrics
#' @return Available institution metrics along with information for them like description, category, and institutions for which a metric is available.
#' @export
get_catalog_institution_metrics <- function(metrics = NULL, as_list = FALSE) {
  query_args <- list(metrics = metrics)

  resp <- send_coinmetrics_request(endpoint = "catalog/institution-metrics", query_args = query_args)
  api_data <- httr::content(resp)[["data"]]
  if (as_list) {
    return(api_data)
  }

  api_data %>%
    data.table::rbindlist(fill = TRUE) %>%
    tidyr::hoist(frequencies, "frequency", "institutions") %>%
    tidyr::unnest_longer(institutions) %>%
    tibble::as_tibble()
}

#' Get Available Exchanges
#' @param exchanges Vector of exchanges. By default all exchanges are returned.
#' @param as_list Return content as list or tibble.
#' @return Available exchanges along with available markets and metrics for them.
#' @export
get_catalog_exchanges <- function(exchanges = NULL, as_list = FALSE) {
  query_args <- list(exchanges = exchanges)
  resp <- send_coinmetrics_request(endpoint = "catalog/exchanges", query_args = query_args)
  api_data <- httr::content(resp)[["data"]]

  if (as_list) {
    return(api_data)
  }

  api_data %>%
    data.table::rbindlist(fill = TRUE) %>%
    tidyr::unnest_longer(markets)

  tibble::tibble(
    exchange = purrr::map_chr(api_data, "exchange", .default = NA),
    markets = purrr::map(api_data, "markets"),
    min_time = purrr::map_chr(api_data, "min_time"),
    max_time = purrr::map_chr(api_data, "max_time")
  ) %>%
    dplyr::mutate(
      dplyr::across(c("min_time", "max_time"), anytime::anytime)
    ) %>%
    tidyr::unnest_longer(markets)
}

#' Get Available Institutions
#' @param institutions Vector of institutions. By default all institutions are returned.
#' @param as_list Return content as list or tibble.
#' @return Available institutions along with information for them like metrics and time ranges of available data.
#' @export
get_catalog_institutions <- function(institutions = NULL, as_list = FALSE) {
  query_args <- list(institutions = institutions)

  resp <- send_coinmetrics_request(endpoint = "catalog/institutions", query_args = query_args)
  api_data <- httr::content(resp)[["data"]]
  if (as_list) {
    return(api_data)
  }

  api_data %>%
    data.table::rbindlist(fill = TRUE) %>%
    tidyr::hoist(metrics, "metric", "frequencies") %>%
    tidyr::unnest(frequencies) %>%
    tidyr::hoist(
      frequencies,
      "frequency",
      "min_time",
      "max_time",
      .transform = list(min_time = anytime::anytime, max_time = anytime::anytime)
    )
}

#' Get Available Markets
#' @param markets Vector of markets. By default all markets are returned.
#' @param exchange Unique name of an exchange.
#' @param type Type of markets. Available inputs are `spot`, `future`, or `option`.
#' @param base Base asset of markets.
#' @param quote Quote asset of markets.
#' @param asset Any asset of markets.
#' @param symbol Symbol of derivative markets; the full instrument name.
#' @param include Vector of fields to include in response.
#' Supported values are `trades`, `orderbooks`, `quotes`, `funding_rates`, `openinterest`, `liquidations`.
#' Included by default if omitted.
#' @param exclude Vector of fields to exclude from response.
#' Supported values are `trades`, `orderbooks`, `quotes`, `funding_rates`, `openinterest`, `liquidations`.
#' Included by default if omitted.
#' @param format Format of the response. Supported values are `json`, `json_stream`. Default is `json`.
#' @param limit Number of response items.
#' @param pretty Human-readable formatting of JSON responses.
#' @return List of available markets along with time ranges of available data.
#' @export
get_catalog_markets <- function(markets = NULL,
                                exchange = NULL,
                                type = NULL,
                                base = NULL,
                                quote = NULL,
                                asset = NULL,
                                symbol = NULL,
                                include = NULL,
                                exclude = NULL,
                                format = "json",
                                limit = NULL,
                                pretty = FALSE) {
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

  resp <- send_coinmetrics_request(endpoint = "catalog/markets", query_args = query_args)
  api_data <- httr::content(resp)[["data"]]

  return(api_data)
}

#' Get Available Market Greeks
#' @inheritParams get_catalog_markets
#' @return Available option greeks.
#' @export
get_catalog_greeks <- function(markets = NULL,
                               exchange = NULL,
                               type = NULL,
                               base = NULL,
                               quote = NULL,
                               asset = NULL,
                               symbol = NULL,
                               format = "json",
                               limit = NULL,
                               pretty = FALSE) {
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

  resp <- send_coinmetrics_request(endpoint = "catalog/market-greeks", query_args = query_args)

  get_coinmetrics_api_data(resp, "market-greeks", "end", F)
}

#' Available Asset Alerts
#' @param assets Vector of asets. By default all assets are returned.
#' @param alerts Vector of asset alert names. By default all asset alerts are returned.
#' @return available asset alerts along with their descriptions, thresholds, and constituents.
#' @export
get_catalog_asset_alerts <- function(assets = NULL, alert = NULL) {
  query_args <- list(assets = assets, alert = alert)

  resp <- send_coinmetrics_request(endpoint = "catalog/alerts", query_args = query_args)
  api_data <- httr::content(resp)[["data"]]

  api_data %>%
    data.table::rbindlist(fill = TRUE) %>%
    tidyr::hoist(
      conditions,
      "description",
      "threshold",
      "constituents"
    ) %>%
    tidyr::unnest_longer(constituents)
}
