#' @title  Get Coin Metrics API Data
#' @description Parses API response into tibble of metrics data
#' @param assets character vector of assets. Use "*" to get metrics for all supported assets.
#' @param metrics 
#' @param frequency 
#' @param start_time 
#' @param end_time 
#' @param start_height 
#' @param end_height 
#' @param start_hash 
#' @param end_hash 
#' @param start_inclusive 
#' @param end_inclusive 
#' @param min_confirmations 
#' @param timezone 
#' @param page_size 
#' @param paging_from 
#' @param sort 
#' @param status 
#' @param limit_per_asset 
#' @param pretty 
#' @param format 
#' @param next_page_token 
#' @param pagination 
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
                              page_size = NULL,
                              paging_from = "end",
                              sort = "asset",
                              status = NULL,
                              limit_per_asset = NULL,
                              pretty = FALSE,
                              format = "json",
                              next_page_token = NULL,
                              pagination = TRUE,
                              null_as_zero = NULL
                              ) {
  
  # read API key
  cm_api_key <- import_api_key()
  # API Request
  if(identical(cm_api_key, "")) {
    api_environment <- 'community'
    cm_api_key <- NULL
  } else {
    api_environment <- "production"
  }
  
  query_args <- list(
    api_key = cm_api_key,
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
    page_size = format(page_size, scientific = FALSE),
    paging_from = paging_from,
    sort = sort,
    limit_per_asset = limit_per_asset,
    pretty = pretty,
    format = format,
    null_as_zero = null_as_zero,
    next_page_token = next_page_token
  ) |> purrr::discard(.p = is.null)
  
  resp <- httr::GET(url = construct_coinmetrics_api_http_url("timeseries/asset-metrics", api_environment),
                    query = query_args)
  
  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "asset-metrics",
    pagination = TRUE,
    paging_from = paging_from
  )

}