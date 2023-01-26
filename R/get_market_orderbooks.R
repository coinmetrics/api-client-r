#' Get Market Orderbooks
#' @inheritParams get_market_open_interest
#' @param depth_limit Book depth limit. Supported values are between 1 and 30000, `10pct_mid_price` or `full_book`.
#' @return Tibble of orderbooks for specified markets, ordered by tuple `(market, time)`. `asks` and `bids` are nested list-columns.
#' @export
get_market_orderbooks <- function(markets,
                                  start_time = NULL,
                                  end_time = NULL,
                                  start_inclusive = TRUE,
                                  end_inclusive = TRUE,
                                  timezone = "UTC",
                                  page_size = NULL,
                                  depth_limit = NULL,
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
    depth_limit = depth_limit,
    paging_from = paging_from,
    limit_per_market = limit_per_market
  )

  resp <- send_coinmetrics_request(endpoint = "timeseries/market-orderbooks", query_args = query_args)

  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "market-orderbooks",
    paging_from = paging_from
  )
}
