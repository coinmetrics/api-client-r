#' Get Market Metrics
#' @inheritParams get_exchange_metrics
#' @param markets Vector of markets.
#' @param metrics Vector of metrics to request time series data
#' @param frequency Frequency of the market metrics. Supported values are `1m`, `5m`, `1h`, `1d`.
#' @param limit_per_market Number of entries per market.
#' @return Tibble or list of metrics for specified markets.
#' @export
get_market_metrics <- function(markets,
                               metrics,
                               frequency = "1d",
                               start_time = NULL,
                               end_time = NULL,
                               start_inclusive = TRUE,
                               end_inclusive = TRUE,
                               timezone = "UTC",
                               page_size = NULL,
                               paging_from = "end",
                               sort = "market",
                               limit_per_market = NULL,
                               pretty = FALSE,
                               format = "json",
                               as_list = FALSE) {
  query_args <- list(
    markets = paste0(markets, collapse = ","),
    metrics = paste0(metrics, collapse = ","),
    frequency = frequency,
    start_time = start_time,
    end_time = end_time,
    start_inclusive = start_inclusive,
    end_inclusive = end_inclusive,
    timezone = timezone,
    page_size = page_size,
    paging_from = paging_from,
    sort = sort,
    limit_per_market = limit_per_market,
    pretty = pretty,
    format = format
  )

  resp <- send_coinmetrics_request(endpoint = "timeseries/market-metrics", query_args = query_args)

  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "market-metrics",
    paging_from = paging_from,
    as_list = as_list
  )
}
