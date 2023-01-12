#' @title  Get Coin Metrics API Data
#' @description Parses API response into tibble of metrics data
#' @inheritParams get_index_levels
#' @param assets character vector of assets. Use "*" to get metrics for all supported assets.
#' @param metrics Vector of metrics to request
#' @param frequency Frequency of metrics. Supported values are `1b` (block by block), `1s`, `1m`, `10m`, `1h`, `1d`, `1d-ny-close`.
#' @param start_height The beginning block height for the set of data returned. Mutually exclusive with `start_time` and `start_hash`.
#' @param end_height The ending block height for the set of data returned. Mutually exclusive with `end_time` and `end_hash`.
#' @param start_hash The beginning block height for the set of data returned. Mutually exclusive with `start_time` and `start_height`.
#' @param end_hash The ending block height for the set of data returned. Mutually exclusive with `end_time` and `end_height`.
#' @param min_confirmations [0, 1, ... 99] How many blocks behind the block by block metrics (`1b` frequency) are the data based
#' @param sort Sort tabular data by asset, time, or metric
#' @param status String for which metric values you want to see: "all", "flash", "reviewed", "revised"
#' @param limit_per_asset Number of entries per asset
#' @param format JSON or CSV
#' @param null_as_zero Represent nulls as zeros in the response (TRUE or FALSE)
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
                              null_as_zero = FALSE,
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