#' Get Market Candles
#' @inheritParams get_market_metrics
#' @param frequency Candle duration. Supported values are `1m`, `5m`, `10m`, `15m`, `30m`, `1h`, `4h`, `1d`.
#' @return Tibble or list of candles for specified markets
#' @export
get_market_candles <- function(markets,
                               frequency = "1d",
                               start_time = NULL,
                               end_time = NULL,
                               start_inclusive = TRUE,
                               end_inclusive = TRUE,
                               timezone = "UTC",
                               page_size = NULL,
                               paging_from = "end",
                               limit_per_market = NULL,
                               pretty = FALSE,
                               as_list = FALSE) {
  
  query_args <- list(
    markets = paste0(markets, collapse = ","),
    frequency = frequency,
    start_time = start_time,
    end_time = end_time,
    start_inclusive = start_inclusive,
    end_inclusive = end_inclusive,
    timezone = timezone,
    page_size = page_size,
    paging_from = paging_from,
    limit_per_market = limit_per_market,
    pretty = pretty,
  )
  
  resp <- send_coinmetrics_request(endpoint = "timeseries/market-candles", query_args = query_args)
  
  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "market-candles",
    paging_from = paging_from,
    as_list = as_list
  )
  
}

#' Get Pair Candles
#' @inheritParams get_market_candles
#' @inheritParams get_pair_metrics
#' @return Tibble or list of candles for specified asset pairs.
#' @export
get_pair_candles <- function(pairs,
                             frequency = "1d",
                             start_time = NULL,
                             end_time = NULL,
                             start_inclusive = TRUE,
                             end_inclusive = TRUE,
                             timezone = "UTC",
                             page_size = NULL,
                             paging_from = "end",
                             limit_per_pair = NULL,
                             pretty = FALSE,
                             as_list = FALSE) {
  
  query_args <- list(
    pairs = paste0(pairs, collapse = ","),
    frequency = frequency,
    start_time = start_time,
    end_time = end_time,
    start_inclusive = start_inclusive,
    end_inclusive = end_inclusive,
    timezone = timezone,
    page_size = page_size,
    paging_from = paging_from,
    limit_per_pair = limit_per_pair,
    pretty = pretty
  )
  
  resp <- send_coinmetrics_request(endpoint = "timeseries/pair-candles", query_args = query_args)
  
  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "pair-candles",
    paging_from = paging_from,
    as_list = as_list
  )
  
}

#' Get Index Candles
#' @inheritParams get_index_levels
#' @param frequency Candle duration. Supported values are `1m`, `5m`, `10m`, `15m`, `30m`, `1h`, `4h`, `1d`.
#' @return Tibble or list of candles for specified indexes.
#' @export
get_index_candles <- function(indexes,
                              frequency = "1d",
                              start_time = NULL,
                              end_time = NULL,
                              start_inclusive = TRUE,
                              end_inclusive = TRUE,
                              timezone = "UTC",
                              page_size = NULL,
                              paging_from = "end",
                              limit_per_index = NULL,
                              pretty = FALSE,
                              as_list = FALSE
                              ) {
  
  query_args <- list(
    indexes = paste0(indexes, collapse = ","),
    frequency = frequency,
    start_time = start_time,
    end_time = end_time,
    start_inclusive = start_inclusive,
    end_inclusive = end_inclusive,
    timezone = timezone,
    page_size = page_size,
    paging_from = paging_from,
    limit_per_index = limit_per_index,
    pretty = pretty
  )
  
  resp <- send_coinmetrics_request(endpoint = "timeseries/index-candles", query_args = query_args)
  
  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "index-candles",
    paging_from = paging_from,
    as_list = as_list
  )
  
}