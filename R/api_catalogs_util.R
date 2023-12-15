catalogMarketsData <- function(api_response) {
  df_markets <-
    RcppSimdJson::fparse(httr::content(api_response, "raw"), "/data") %>%
    data.table::setDT()

  df_markets <- suppressMessages(readr::type_convert(df_markets))

  tibble::as_tibble(df_markets)
}

catalogAssetsData <- function(api_response) {
  RcppSimdJson::fparse(httr::content(api_response, "raw"), "/data") %>% tibble::as_tibble()
}

catalogMarketMetricsData <- function(api_response) {
  RcppSimdJson::fparse(httr::content(api_response, "raw"), "/data") %>% tibble::as_tibble()
}

#' @importFrom data.table .SD ":="
catalogExchangesData <- function(api_response) {
  df_exchanges <-
    RcppSimdJson::fparse(httr::content(api_response, "raw"), "/data") %>%
    data.table::setDT()

  df_exchanges[,
    c("min_time", "max_time") := lapply(.SD, lubridate::as_datetime),
    .SDcols = c("min_time", "max_time")
  ]

  tibble::as_tibble(df_exchanges)
}

catalogExchangeAssetsData <- function(api_response) {
  df_ea <-
    RcppSimdJson::fparse(httr::content(api_response, "raw"), "/data") %>%
    data.table::setDT()

  df_ea %>%
    tidyr::unnest("metrics") %>%
    tidyr::unnest("frequencies") %>%
    dplyr::mutate(dplyr::across(c("min_time", "max_time"), lubridate::as_datetime))
}

catalogPairsData <- function(api_response) {
  df_pairs <-
    RcppSimdJson::fparse(httr::content(api_response, "raw"), "/data") %>%
    data.table::setDT()

  df_pairs %>%
    tidyr::unnest("frequencies") %>%
    dplyr::mutate(dplyr::across(c("min_time", "max_time"), lubridate::as_datetime))
}

catalogMetricsData <- function(api_response, level) {
  df_metrics <-
    RcppSimdJson::fparse(httr::content(api_response, "raw"), "/data") %>%
    data.table::setDT()

  df_metrics %>%
    tidyr::unnest("frequencies") %>%
    tidyr::unnest({{ level }})
}

catalogInstData <- function(api_response) {
  df_inst <-
    RcppSimdJson::fparse(httr::content(api_response, "raw"), "/data") %>%
    data.table::setDT()

  df_inst %>%
    tidyr::unnest("metrics") %>%
    tidyr::unnest("frequencies") %>%
    dplyr::mutate(dplyr::across(c("min_time", "max_time"), lubridate::as_datetime))
}

catalogMarketCandlesData <- function(api_response) {
  frequencies <- NULL

  df_candles <-
    RcppSimdJson::fparse(httr::content(api_response, "raw"), "/data") %>%
    data.table::setDT()

  df_candles <- df_candles[, unlist(frequencies, recursive = FALSE), by = "market"]
  df_candles[, c("min_time", "max_time") := lapply(.SD, lubridate::as_datetime), .SDcols = c("min_time", "max_time")]

  tibble::as_tibble(df_candles)
}
