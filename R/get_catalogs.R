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
get_catalog_asset_pairs <- function(pairs=NULL, as_list=TRUE) {
  
  resp <- send_coinmetrics_request(endpoint = "catalog/pairs", query_args = list(pairs = pairs))
  
  pairs_content <- httr::content(resp)[["data"]]
  
  if(as_list) {
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
get_catalog_asset_metrics <- function(metrics=NULL, reviewable=TRUE, pretty=FALSE, as_list=TRUE) {
  
  query_args <- list(metrics = metrics, reviewable = reviewable, pretty = pretty)
  
  resp <- send_coinmetrics_request(endpoint = "catalog/asset-metrics", query_args = query_args)
  
  metrics_content <- httr::content(resp)[["data"]]
  
  if(as_list) {
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
#' @param pretty Human-readable formatting of JSON responses.
#' @param as_list Return content as list instead of tibble. 
#' @return Tibble of Available exchange-asset pairs along with information like metrics and time ranges of available data.
#' @export
get_catalog_exchange_assets <- function(exchange_assets=NULL, pretty=FALSE, as_list=TRUE) {
  
  query_args <- list(exchange_assets = exchange_assets, pretty = pretty)
  
  resp <- send_coinmetrics_request(endpoint = "catalog/exchange-assets", query_args = query_args)
  
  ea_content <- httr::content(resp)[["data"]]
  
  if(as_list) {
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
get_catalog_indexes <- function(indexes=NULL) {
  
  resp <- send_coinmetrics_request(endpoint = "catalog/indexes", query_args = list(indexes=indexes))
  ix_content <- httr::content(resp)[["data"]]
  
  data.table::rbindlist(ix_content, fill = TRUE) %>%
    tidyr::unnest_wider(frequencies) %>%
    dplyr::mutate(min_time = anytime::anytime(min_time),
                  max_time = anytime::anytime(max_time))
}