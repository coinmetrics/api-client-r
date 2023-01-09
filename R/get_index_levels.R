#' Get Index Levels
#'
#' @param indexes vector of index names, e.g. 'CMBI10'
#' @param frequency Frequency of the index. Supported values are 1s, 15s, 1h, 1d-ny-close, 1d-sg-close, 1d, 1d-HH:00
#' @param start_time Start time of the timeseries. Multiple formats of ISO 8601 are supported.
#' @param end_time End time of the timeseries. Multiple formats of ISO 8601 are supported.
#' @param start_inclusive Flag to define if start timestamp must be included in the timeseries if present. True by default.
#' @param end_inclusive Flag to define if end timestamp must be included in the timeseries if present. True by default.
#' @param timezone Timezone for `start_time` and `end_time` timestamps. Default is UTC. Example: America/New_York.
#' @param page_size Number of items per single page of results. Default is 100.
#' @param paging_from Where first page starts: start or end of interval.
#' @param limit_per_index How many entries per index result.
#' @param pretty Human-readable JSON formatting. Default is false.
#' @param next_page_token Token for receiving the results from the next page of a query. Should not be used directly.
#'
#' @return Tibble of index levels for specified indexes and date range
#' @export
get_index_levels <- function(indexes,
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
                             next_page_token = NULL) {
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
    indexes = paste0(indexes, collapse = ','),
    frequency = frequency,
    start_time = start_time,
    end_time = end_time,
    start_inclusive = start_inclusive,
    end_inclusive = end_inclusive,
    timezone = timezone,
    page_size = format(page_size, scientific = FALSE),
    paging_from = paging_from,
    limit_per_index = limit_per_index,
    pretty = pretty,
    next_page_token = next_page_token
  ) |> purrr::discard(.p = is.null)
  
  resp <- httr::GET(url = construct_coinmetrics_api_http_url("timeseries/index-levels", api_environment),
                    query = query_args)
  
  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "index-levels",
    pagination = TRUE,
    paging_from = paging_from
  )
  
}
