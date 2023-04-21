#' Get Market Liquidations
#' @inheritParams get_market_openinterest
#' @return Tible of liquidations for specified futures markets, ordered by tuple `(market, time)`.
#' @export
get_market_liquidations <- function(markets,
                                    start_time = NULL,
                                    end_time = NULL,
                                    start_inclusive = TRUE,
                                    end_inclusive = TRUE,
                                    timezone = "UTC",
                                    page_size = NULL,
                                    paging_from = "end",
                                    limit_per_market = NULL) {
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

  resp <- send_coinmetrics_request(endpoint = "timeseries/market-liquidations", query_args = query_args)

  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "market-liquidations",
    paging_from = paging_from
  )
}
