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
                                   start_inclusive = NULL,
                                   end_inclusive = NULL,
                                   timezone = NULL,
                                   page_size = NULL,
                                   pretty = FALSE,
                                   next_page_token = NULL
) {
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
    indexes = paste0(indexes, collapse = ","),
    frequency = frequency,
    start_time = start_time,
    end_time = end_time,
    start_inclusive = start_inclusive,
    end_inclusive = end_inclusive,
    timezone = timezone,
    page_size = page_size,
    pretty = pretty,
    next_page_token = next_page_token
  ) |> purrr::discard(.p = is.null)
  
  resp <- httr::GET(url = construct_coinmetrics_api_http_url("timeseries/index-constituents", api_environment),
                    query = query_args)
  
  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "index-constituents",
    pagination = TRUE,
    paging_from = paging_from
  )
}