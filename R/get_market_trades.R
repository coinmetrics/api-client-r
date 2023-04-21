#' Get Market Trades
#' @inheritParams get_market_metrics
#' @param min_confirmations Integer in `[0, 99]` with 2 as default. Specifies how many blocks behind the chain tip trades are based on.
#' Currently only available for DeFi markets.
#' @return Tibble or list of selected market trades.
#' @export
get_market_trades <- function(markets,
                              start_time = NULL,
                              end_time = NULL,
                              start_inclusive = TRUE,
                              end_inclusive = TRUE,
                              timezone = "UTC",
                              page_size = 10000,
                              paging_from = "end",
                              limit_per_market = NULL,
                              min_confirmations = NULL) {
  query_args <- list(
    markets = paste0(markets, collapse = ","),
    start_time = start_time,
    end_time = end_time,
    start_inclusive = start_inclusive,
    end_inclusive = end_inclusive,
    timezone = timezone,
    page_size = page_size,
    paging_from = paging_from,
    limit_per_market = limit_per_market,
    min_confirmations = min_confirmations
  )

  resp <- send_coinmetrics_request(endpoint = "timeseries/market-trades", query_args = query_args)

  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "market-trades",
    paging_from = paging_from
  )
}
