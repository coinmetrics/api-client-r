#' Get Market Quotes
#' @inheritParams get_market_open_interest
#' @param include_one_sided Include one-side and empty books in quotes response.
#' @return Tibble of quotes for specified markets, ordered by tuple `(market, time)`.
#' @export
get_market_quotes <- function(markets,
                              start_time = NULL,
                              end_time = NULL,
                              start_inclusive = TRUE,
                              end_inclusive = TRUE,
                              timezone = "UTC",
                              page_size = NULL,
                              paging_from = "end",
                              limit_per_market = NULL,
                              pretty = FALSE,
                              include_one_sided = FALSE) {
  query_args <- list(
    markets = paste0(markets, collapse = ","),
    start_time = start_time,
    end_time = end_time,
    start_inclusive = start_inclusive,
    timezone = timezone,
    page_size = page_size,
    paging_from = paging_from,
    limit_per_market = limit_per_market,
    pretty = pretty,
    include_one_sided = include_one_sided
  )

  resp <- send_coinmetrics_request(endpoint = "timeseries/market-quotes", query_args = query_args)

  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "market-quotes",
    paging_from = paging_from
  )
}
