#' Get Index Constituents
#'
#' @inheritParams get_index_levels 
#'
#' @return Tibble of constituents and weights for specified indexes.
#' @export
get_index_constituents <- function(indexes,
                                   frequency = "1d",
                                   start_time = NULL,
                                   end_time = NULL,
                                   start_inclusive = TRUE,
                                   end_inclusive = TRUE,
                                   timezone = "UTC",
                                   page_size = NULL,
                                   paging_from = "end",
                                   pretty = FALSE,
                                   as_list = FALSE) {
  
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
    pretty = pretty
  )
  
  resp <- send_coinmetrics_request(endpoint = 'timeseries/index-constituents', query_args = query_args)
  
  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "index-constituents",
    paging_from = paging_from,
    as_list = as_list
  )
  
}