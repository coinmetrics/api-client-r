#' Get Implied Volatility
#' @inheritParams get_market_metrics
#' @return Data table of implied volatility for specified markets
#' @export
get_market_implied_vol <- function(markets,
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

  resp <- send_coinmetrics_request(endpoint = "timeseries/market-implied-volatility", query_args = query_args)

  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "market-implied-volatility",
    paging_from = paging_from
  )
}

#' Get Market Greeks
#' @inheritParams get_market_implied_vol
#' @return Data table of greeks for option markets
#' @export
get_market_greeks <- function(markets,
                              start_time = NULL,
                              end_time = NULL,
                              start_inclusive = TRUE,
                              end_inclusive = TRUE,
                              timezone = "UTC",
                              page_size = NULL,
                              paging_from = "end",
                              limit_per_market = NULL) {
  query_args <- list(
    markets = markets,
    start_time = start_time,
    end_time = end_time,
    start_inclusive = start_inclusive,
    end_inclusive = end_inclusive,
    timezone = timezone,
    page_size = page_size,
    paging_from = paging_from,
    limit_per_market = limit_per_market
  )

  resp <- send_coinmetrics_request(endpoint = "timeseries/market-greeks", query_args = query_args)

  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "market-greeks",
    paging_from = paging_from
  )
}
