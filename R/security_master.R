#' Security Master Assets
#' @param assets Character vector of assets to query. Mutually exclusive with `codes`.
#' @param codes Character vector of ten-digit alphanumeric identifying codes. Mutually exclusive with `assets`.
#' @param page_size Integer number of items per single page of results. Default is 100.
#' @param paging_from  Enum: `start`, `end`. Where does the first page start, at the start of the interval or at the end.
#' @return Data table of all assets and their metadata in security master.
#' @export
security_master_assets <- function(assets = NULL,
                                   codes = NULL,
                                   page_size = NULL,
                                   paging_from = "start") {
  if (!is.null(assets) && !is.null(codes)) {
    stop("`assets` and `codes` are mutually exclusive and can\'t be specified in the same request.")
  }

  query_args <- list(
    assets = assets,
    codes = codes,
    page_size = page_size,
    paging_from = paging_from
  )

  resp <- send_coinmetrics_request(
    endpoint = "/security-master/assets",
    query_args = query_args
  )

  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "security-master",
    paging_from = paging_from
  )
}

#' Security Master Markets
#' @param type Enum: `spot`, `future`, `option`.Type of markets.
#' If `type` is NULL, then ALL markets are selected, and the API response could become unreasonably large.
#' @param markets Character vector of markets.
#' @param symbol Symbol of derivative markets, full instrument name.
#' @param exchange Unique name of an exchange.
#' @param base Base asset of markets.
#' @param quote Quote asset of markets.
#' @inheritParams security_master_assets
#' @return Data table of metadata on all the markets offered (spot, options, futures), sorted alphabetically by market.
#' @export
security_master_markets <- function(type = NULL,
                                    markets = NULL,
                                    symbol = NULL,
                                    exchange = NULL,
                                    base = NULL,
                                    quote = NULL,
                                    page_size = NULL,
                                    paging_from = "start") {
  if (is.null(type)) {
    warning("Parameter `type` is unspecified. API response may be large.")
  }
  query_args <- list(
    type = type,
    markets = markets,
    symbol = symbol,
    exchange = exchange,
    base = base,
    quote = quote,
    page_size = page_size,
    paging_from = paging_from
  )

  resp <- send_coinmetrics_request(
    endpoint = "/security-master/markets",
    query_args = query_args
  )

  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "security-master",
    paging_from = paging_from
  )
}
