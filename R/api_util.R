import_api_key <- function() {
  key <- Sys.getenv("CM_API_KEY")
  if (identical(key, "")) {
    #stop("`CM_API_KEY` env var has not been set")
    warning("`CM_API_KEY` environment variable not found.\nDefaulting to Community API")
  }
  
  return(key)
}

sanitize_query_params <- function(...) {
  function_arguments <- list(...)
  for(i in 1:length(function_arguments)){
    if(class(function_arguments[[i]]) == "list") {
      function_arguments[[i]] <- paste0(function_arguments[[i]], collapse = ",")
    } else if(class(function_arguments[[i]]) == "numeric") {
      function_arguments[[i]] <- format(function_arguments[[i]], scientific = FALSE)
    } else if(class(function_arguments[[i]]) == "logical") {
      if(function_arguments[[i]] == TRUE) {
        function_arguments[[i]] <- "true"
      } else {
        function_arguments[[i]] <- "false"
      }
    } else if(class(function_arguments[[i]]) == "character" && !assertthat::is.string(function_arguments[[i]])) {
      function_arguments[[i]] <- paste0(function_arguments[[i]], collapse = ",")
    }
  }
  result_arguments <- function_arguments |> purrr::discard(.p = is.null)
  return(result_arguments)
}

send_coinmetrics_request <- function(endpoint, query_args = NULL) {
  
  # read API key
  cm_api_key <- import_api_key()
  # API Request
  if(identical(cm_api_key, "")) {
    api_environment <- 'community'
    cm_api_key <- NULL
  } else {
    api_environment <- "production"
  }
  
#  query_args <- c(query_args, "api_key" = cm_api_key)
#  query_args <- sanitize_query_params(list(...))
  message(stringr::str_interp("CM_API_KEY: ${cm_api_key}"))
  if(!is.null(query_args)) {
    query_args <- do.call(sanitize_query_params, query_args)
    query_args <- c(query_args, api_key=cm_api_key)
  } else {
    query_args <- list(api_key=cm_api_key)
  }

  headers <- c("Content-Type" = "application/json", "R-API-Client-Version" = packageVersion("coinmetrics"))
  
  response <- httr::GET(url = construct_coinmetrics_api_http_url(endpoint_path = endpoint, api_environment),
                        query = query_args, headers = headers)
  
  if (response$status_code != 200) {
    stop(stringr::str_interp("HTTP Error ${response$status_code} returned when calling url: ${response$url}"), response)
  }
  
  return(response)
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

