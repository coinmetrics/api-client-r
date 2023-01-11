#' @title  Get Coin Metrics API Data
#' @description Parses API response into tibble of metrics data
#' @inheritParams get_index_levels
#' @param assets character vector of assets. Use "*" to get metrics for all supported assets.
#' @param metrics 
#' @param frequency 
#' @param start_height 
#' @param end_height 
#' @param start_hash 
#' @param end_hash 
#' @param min_confirmations 
#' @param sort Sort tabular data by asset, time, or metric
#' @param status 
#' @param limit_per_asset 
#' @param format JSON or CSV
#' @param null_as_zero 
#'
#' @export
get_asset_metrics <- function(assets,
                              metrics,
                              frequency = "1d",
                              start_time = NULL,
                              end_time = NULL,
                              start_height = NULL,
                              end_height = NULL,
                              start_hash = NULL,
                              end_hash = NULL,
                              start_inclusive = TRUE,
                              end_inclusive = TRUE,
                              min_confirmations = NULL,
                              timezone = "UTC",
                              page_size = 100,
                              paging_from = "end",
                              sort = "asset",
                              status = NULL,
                              limit_per_asset = NULL,
                              pretty = FALSE,
                              format = "json",
                              null_as_zero = NULL,
                              as_list = FALSE
                              ) {
  
  query_args <- list(
    assets = paste0(assets, collapse = ","),
    metrics = paste0(metrics, collapse = ","),
    frequency = frequency,
    status = status,
    start_time = start_time,
    end_time = end_time,
    start_height = start_height,
    end_height = end_height,
    start_hash = start_hash,
    end_hash = end_hash,
    start_inclusive = start_inclusive,
    end_inclusive = end_inclusive,
    min_confirmations = min_confirmations,
    timezone = timezone,
    page_size = page_size,
    paging_from = paging_from,
    sort = sort,
    limit_per_asset = limit_per_asset,
    pretty = pretty,
    format = format,
    null_as_zero = null_as_zero
  )
  
  resp <- send_coinmetrics_request(endpoint = "timeseries/asset-metrics", query_args = query_args)
  
  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "asset-metrics",
    paging_from = paging_from,
    as_list = as_list
  )

}