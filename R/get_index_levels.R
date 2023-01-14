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
#' @param as_list Should API return list instead of tabular data
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
                             page_size = 100,
                             paging_from = "end",
                             limit_per_index = NULL,
                             pretty = FALSE,
                             as_list = FALSE) {
  
  query_args <- list(
    indexes = paste0(indexes, collapse = ','),
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
  
  resp <- send_coinmetrics_request(endpoint = "timeseries/index-levels", query_args = query_args)
  
  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "index-levels",
    paging_from = paging_from,
    as_list = as_list
  )
  
}
