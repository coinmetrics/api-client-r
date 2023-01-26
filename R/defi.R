#' DeFi Balance Sheets (experimental)
#' Data is updated on an end-of-day frequency
#' @param defi_protocols Vector of DeFi protocols like `aave_v2_eth` or protocol patterns `aave_v2_*` or `aave_*_eth` or `*_eth`
#' @param start_time Start of the time interval. If omitted, response starts from the earliest time available.
#' @param end_time End of the time interval. If omitted, response includes data up to the latest time available.
#' @param start_height The beginning block height for the set of data returned.
#' @param end_height The ending block height for the set of data returned.
#' @param start_inclusive Inclusive or exclusive corresponding `start_*` parameters.
#' @param end_inclusive Inclusive or exclusive corresponding `end_*` parameters.
#' @param timezone Timezone name for `start_time` and `end_time` timestamps. Note, the output times are always in `UTC`.
#' @param page_size Number of items per single page of results. Default is 100.
#' @param paging_from Where the fist page starts, at the beginning or end of the interval.
#' @param pretty Human-readable formatting of JSON responses.
#' @return List of DeFi balance sheet records for specified DeFi protocols
#' @export
get_defi_balance_sheets <- function(defi_protocols,
                                    start_time = NULL,
                                    end_time = NULL,
                                    start_height = NULL,
                                    end_height = NULL,
                                    start_inclusive = TRUE,
                                    end_inclusive = TRUE,
                                    timezone = "UTC",
                                    page_size = NULL,
                                    paging_from = "end",
                                    pretty = FALSE) {
  query_args <- list(
    defi_protocols = paste0(defi_protocols, collapse = ","),
    start_time = start_time,
    end_time = end_time,
    start_height = start_height,
    end_height = end_height,
    start_inclusive = start_inclusive,
    end_inclusive = end_inclusive,
    timezone = timezone,
    page_size = page_size,
    paging_from = paging_from,
    pretty = pretty
  )

  resp <- send_coinmetrics_request(endpoint = "timeseries/defi-balance-sheets", query_args = query_args)

  get_coinmetrics_api_data(api_response = resp, endpoint = "defi-balance-sheets", paging_from = paging_from, as_list = T)
}
