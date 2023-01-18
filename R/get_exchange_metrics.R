#' Exchange Metrics
#' @inheritParams get_pair_metrics
#' @param exchanges Vector of exchange names.
#' @param metrics Vector of metrics to request. Information on all available metrics can be found on page https://docs.coinmetrics.io/info/pair_metrics.
#' @param limit_per_exchange Number of entries per institution.
#' @return Tibble or list of metrics for specified exchanges. Results are ordered by tuple (exchange, time).
#' @export
get_exchange_metrics <- function(exchanges,
                                 metrics,
                                 frequency = "1d",
                                 start_time = NULL,
                                 end_time = NULL,
                                 start_inclusive = TRUE,
                                 end_inclusive = TRUE,
                                 timezone = "UTC",
                                 page_size = NULL,
                                 paging_from = "end",
                                 sort = "exchange",
                                 limit_per_exchange = NULL,
                                 pretty = FALSE,
                                 format = "json",
                                 as_list = FALSE) {
  query_args <- list(
    exchanges = paste0(exchanges, collapse = ","),
    metrics = paste0(metrics, collapse = ","),
    start_time = start_time,
    end_time = end_time,
    start_inclusive = start_inclusive,
    end_inclusive = end_inclusive,
    timezone = timezone,
    page_size = page_size,
    paging_from = paging_from,
    sort = sort,
    limit_per_exchange = limit_per_exchange,
    pretty = pretty,
    format = format
  )

  resp <- send_coinmetrics_request(endpoint = "timeseries/exchange-metrics", query_args = query_args)

  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "exchange-metrics",
    paging_from = paging_from,
    as_list = as_list
  )
}

#' Exchange-asset Metrics
#' @inheritParams get_exchange_metrics
#' @param exchange_assets Vector of exchange-asset pairs, e.g. binance-btc
#' @param frequency Frequency of exchange-asset metrics. Supported values are `5m`, `1h`, `1d`.
#' @param sort How results will be sorted. Metrics are sorted by `(exchange_asset, time)` by default. Posible values are *exchange_asset* or *time*.
#' @param limit_per_exchange_asset Number of entries per exchange_asset.
#' @return Tibble or list of metrics for specified exchange-asset.
#' @export
get_exchange_asset_metrics <- function(exchange_assets,
                                       metrics,
                                       frequency = "1d",
                                       start_time = NULL,
                                       end_time = NULL,
                                       start_inclusive = TRUE,
                                       end_inclusive = TRUE,
                                       timezone = "UTC",
                                       page_size = NULL,
                                       sort = "exchange_asset",
                                       limit_per_exchange_asset = NULL,
                                       pretty = FALSE,
                                       format = "json",
                                       as_list = FALSE) {
  query_args <- list(
    exchange_assets = paste0(exchange_assets, collapse = ","),
    metrics = paste0(metrics, collapse = ","),
    frequency = frequency,
    start_time = start_time,
    end_time = end_time,
    start_inclusive = start_inclusive,
    end_inclusive = end_inclusive,
    timezone = timezone,
    page_size = page_size,
    sort = sort,
    limit_per_exchange_asset = limit_per_exchange_asset,
    pretty = pretty,
    format = format
  )

  resp <- send_coinmetrics_request(endpoint = "timeseries/exchange-asset-metrics", query_args = query_args)

  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "exchange-asset-metrics",
    paging_from = paging_from,
    as_list = as_list
  )
}
