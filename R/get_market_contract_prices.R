#' Get Market Contract Prices
#' @inheritParams get_market_open_interest
#' @return Tibble of contract prices for specified markets. This includes index price and mark price that are used by the exchange for settlement and risk management purposes.
#' @export
get_market_contract_prices <- function(markets,
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
    limit_per_market = limit_per_market,
    pretty = pretty
  )

  resp <- send_coinmetrics_request(endpoint = "timeseries/market-contract-prices", query_args = query_args)

  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "market-contract-prices",
    paging_from = paging_from
  )
}
