#' Available Assets
#'
#' @param assets Vector of assets. By default all assets are returned.
#' @param include Vector of fields to include in response. Supported values are `metrics`, `markets`, `exchanges`. Included by default if `NULL`.
#' @param exclude Vector of fields to exclude from response. Supported values are `metrics`, `markets`, `exchanges`. Included by default if `NULL`.
#' @param pretty Human-readable formatting of JSON responses.
#' @param as_list Return data as list
#' @return List of available assets along with information for them (metrics, markets, and exchanges) and time ranges of available data.
#' @export
get_catalog_assets <- function(assets = NULL,
                               include = NULL,
                               exclude = NULL,
                               pretty = FALSE,
                               as_list = TRUE) {
  
  query_args <- list(
    assets = assets,
    include = include,
    exclude = exclude,
    pretty = pretty
  )
  
  resp <- send_coinmetrics_request(endpoint = "catalog/assets", query_args = query_args)
  
  httr::content(resp)
    
}