get_catalog_v2_data <- function(api_response, paging_from) {
  
  api_content <- api_response |>
    httr::content(as = "raw") |>
    RcppSimdJson::fparse(
      query = c(data = "/data", next_page_url = "/next_page_url"),
      query_error_ok = TRUE,
      on_query_error = character(),
      max_simplify_lvl = 0L
    )
  
  api_data <- api_content[["data"]]
  
  while (length(api_content[["next_page_url"]]) > 0) {
    api_content <-
      httr::GET(api_content[["next_page_url"]]) |>
      httr::content(as = "raw") |>
      RcppSimdJson::fparse(
        query = c(data = "/data", next_page_url = "/next_page_url"),
        query_error_ok = TRUE,
        on_query_error = character(),
        max_simplify_lvl = 0L
      )
    
    api_data <- switch(
      paging_from,
      end = purrr::list_c(list(api_content[["data"]], api_data)),
      start = purrr::list_c(list(api_data, api_content[["data"]]))
    )
  }
  
  tibble::new_tibble(api_data)
  
}

catalogV2MetricsData <- function(api_data) {

  df_metrics <- 
    api_data |>
    tidyr::unnest('metrics') |>
    tidyr::unnest('frequencies') |>
    data.table::setDT()
  
  df_metrics[, c('min_time', 'max_time') := lapply(.SD, lubridate::ymd_hms), .SDcols = c('min_time', 'max_time')]
  
  return(df_metrics[])
}

catalogV2MarketsData <- function(api_data) {
  
  data.table::setDT(api_data)
  api_data[, c('min_time', 'max_time') := lapply(.SD, lubridate::ymd_hms), .SDcols = c('min_time', 'max_time')]
  
  return(api_data[])
}

catalogV2MarketCandlesData <- function(api_data) {
  
  data.table::setDT(api_data)
  api_data <- api_data[, unlist(get('frequencies'), recursive = FALSE, use.names = TRUE), by = 'market']
  
  api_data[, c('min_time', 'max_time') := lapply(.SD, lubridate::ymd_hms), .SDcols = c('min_time', 'max_time')]
  
  return(api_data[])
}

catalogV2PairCandlesData <- function(api_data) {
  
  data.table::setDT(api_data)
  api_data <- api_data[, unlist(get('frequencies'), recursive = FALSE, use.names = TRUE), by = 'pair']
  
  api_data[, c('min_time', 'max_time') := lapply(.SD, lubridate::ymd_hms), .SDcols = c('min_time', 'max_time')]
  
  return(api_data[])
}

catalogV2OrderbooksData <- function(api_data) {
  
  data.table::setDT(api_data)
  api_data <- api_data[, unlist(get('depths'), recursive = FALSE, use.names = TRUE), by = 'market']
  
  api_data[, c('min_time', 'max_time') := lapply(.SD, lubridate::ymd_hms), .SDcols = c('min_time', 'max_time')]
  
  return(api_data[])
}

catalogV2MarketMetricsData <- function(api_data) {
  
  df_market_metrics <-
    api_data |>
    tidyr::unnest('metrics') |>
    tidyr::unnest('frequencies') |>
    data.table::setDT()
  
  df_market_metrics[, c('min_time', 'max_time') := lapply(.SD, lubridate::ymd_hms), .SDcols = c('min_time', 'max_time')]
  
  return(df_market_metrics[])
}

catalogV2IndexData <- function(api_data) {
  
  data.table::setDT(api_data)
  
  api_data <- api_data[, unlist(get('frequencies'), recursive = F, use.names = T), by = 'index']
  api_data[, c('min_time', 'max_time') := lapply(.SD, lubridate::ymd_hms), .SDcols = c('min_time', 'max_time')]
  
  return(api_data[])
}