#' Supported Assets
#' @param assets Vector of assets. By default all assets are returned.
#' @param include Vector of fields to include in response. Supported values are `metrics`, `markets`, `exchanges`. 
#' Included by default if omitted.,
#' @param exclude Vector of fields to exclude in response. Supported values are `metrics`, `markets`, `exchanges`. 
#' Included by default if omitted.
#' @param pretty Human-readable formatting of JSON responses.
#' @return Tibble of supported assets along with information for them like metrics, markets, exchanges, and time ranges of available data.
#' Metrics, markets, and exchanges outputted as list-columns.
#' @export
get_catalog_all_assets <- function(assets = NULL, include = NULL, exclude = NULL, pretty = FALSE) {
  
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
#' @param metrics Vector of metrics. By default all metrics are returned.
#' @param reviewable Limit to human-reviewable metrics. By default all metrics are returned.
#' @param pretty Human-readable formatting of JSON responses.
#' @return Tibble of supported asset metrics along with information for them 
#' like description, category, and assets for which a metric is available.
#' @export
get_catalog_all_metrics <- function(metrics = NULL, reviewable = NULL, pretty = FALSE) {
  
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
#' @inheritParams get_catalog_all_metrics
#' @return Tibble of supported exchange metrics along with information for them 
#' like description, category, and exchanges for which a metric is available.
get_catalog_all_exchange_metrics <- function(metrics=NULL, reviewable=NULL, pretty=FALSE) {
  
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
