catalogMarketsData <- function(api_response) {
  
  df_markets <-
    RcppSimdJson::fparse(httr::content(api_response, "raw"), "/data") %>%
    data.table::setDT()
  
  df_markets <- suppressMessages(readr::type_convert(df_markets))
  
  tibble::as_tibble(df_markets)
}

catalogAssetsData <- function(api_response) {
  
  df_assets <- 
    RcppSimdJson::fparse(httr::content(api_response, "raw"), "/data") %>%
    data.table::setDT()
  df_assets <- suppressMessages(readr::type_convert(df_assets))
  
  tibble::as_tibble(df_assets)
}

catalogMarketMetricsData <- function(api_response) {
  RcppSimdJson::fparse(httr::content(api_response, "raw"), "/data") %>% tibble::as_tibble()
}