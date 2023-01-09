import_api_key <- function() {
  key <- Sys.getenv("CM_API_KEY")
  if (identical(key, "")) {
    #stop("`CM_API_KEY` env var has not been set")
    warning("`CM_API_KEY` environment variable not found.\nDefaulting to Community API")
  }
  
  return(key)
}


get_coinmetrics_api_data <- function(api_response,
                                        endpoint,
                                        paging_from) {
  
  api_content <- api_response %>%
    httr::content()
  
  api_data <- api_response %>%
    httr::content() %>%
    purrr::pluck("data")
  
    
  while (is.null(api_content[["next_page_url"]]) == FALSE) {
    # print(api_content[["next_page_url"]])
    
    api_content <-
      httr::GET(url = api_content[["next_page_url"]]) %>%
      httr::content()
    
    if (paging_from == "end")
      api_data <- c(api_content[["data"]], api_data)
    
    else if (paging_from == "start")
      api_data <- c(api_data, api_content[["data"]])
    
  }
    
  
  if (endpoint %in% c("asset-metrics", "pair-metrics", "exchange-metrics", "exchange-asset-metrics", "institution-metrics", "market-trades", "market-openinterest", "market-liquidations", "market-funding-rates", "market-quotes", "market-candles", "index-levels", "asset/blocks", "asset/accounts", "asset/transactions", "asset/balance-updates", "taxonomy/assets")) {
    
    api_data <- api_data %>%
      data.table::rbindlist(fill = TRUE) %>%
      purrr::map_df(readr::parse_guess)
    
  }
  
  
  if (endpoint == "index-constituents") {
    
    api_data <- tibble::tibble(
      index = purrr::map_chr(api_data, "index", .default = NA),
      time = purrr::map_chr(api_data, "time", .default = NA),
      constituents = purrr::map(api_data, "constituents", .default = NA)
    ) %>%
      tidyr::unnest(constituents) %>%
      dplyr::mutate(
        asset = purrr::map_chr(constituents, "asset", .default = NA),
        weight = purrr::map_chr(constituents, "weight", .default = NA)
      ) %>%
      dplyr::mutate(
        time = anytime::anytime(time),
        weight = as.numeric(weight)
      ) %>%
      dplyr::select(-constituents)
    
  }
  
  return(api_data)
  
}

construct_coinmetrics_api_http_url <- function(endpoint_path, environment = "production", api_version = "v4") {
  
  root_url <- switch(environment,
                     production = "https://api.coinmetrics.io/",
                     community = "https://community-api.coinmetrics.io/")
  
  
  httr::modify_url(url = root_url,
                   path = c(api_version, endpoint_path))
  
}
