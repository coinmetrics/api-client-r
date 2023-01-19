#' Get Market Open Interest
#' @param markets Vector of markets or market patterns like `exchange-*` or `exchange-*-spot` or `*USDT-future`.
#' @inheritParams get_index_levels
#' @param limit_per_market Number of entries per market.
#' @return Tibble of open interest for specified futures markets, ordered by tuple `(market, time)`.
#' @export
get_market_open_interest <- function(markets,
                                     start_time = NULL,
                                     end_time = NULL,
                                     start_inclusive = TRUE,
                                     end_inclusive = TRUE,
                                     timezone = "UTC",
                                     page_size = NULL,
                                     paging_from = "end",
                                     limit_per_market = NULL,
                                     pretty = FALSE) {
  query_args <- list(
    markets = paste0(markets, collapse = ","),
    start_time = start_time,
    end_time = end_time,
    start_inclusive = start_inclusive,
    end_inclusive = end_inclusive,
    timezone = timezone,
    page_size = page_size,
    paging_from = paging_from,
    limit_per_market = limit_per_market
  )

  resp <- send_coinmetrics_request(endpoint = "timeseries/market-openinterest", query_args = query_args)

  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "market-openinterest",
    paging_from = paging_from,
    as_list = FALSE
  )
}
