import_api_key <- function() {
  key <- Sys.getenv("CM_API_KEY")
  if (identical(key, "")) {
    # stop("`CM_API_KEY` env var has not been set")
    warning("`CM_API_KEY` environment variable not found.\nDefaulting to Community API")
  }

  return(key)
}

sanitize_query_params <- function(...) {
  function_arguments <- list(...)
  for (i in seq_along(function_arguments)) {
    if (is.list(function_arguments[[i]]) || is.character(function_arguments[[i]])) {
      function_arguments[[i]] <- paste0(function_arguments[[i]], collapse = ",")
    } else if (is.numeric(function_arguments[[i]])) {
      function_arguments[[i]] <- format(function_arguments[[i]], scientific = FALSE)
    } else if (is.logical(function_arguments[[i]])) {
      if (isTRUE(function_arguments[[i]])) {
        function_arguments[[i]] <- "true"
      } else {
        function_arguments[[i]] <- "false"
      }
    }
  }
  result_arguments <- purrr::discard(function_arguments, .p = is.null)

  return(result_arguments)
}

send_coinmetrics_request <- function(endpoint, query_args = NULL) {
  # read API key
  cm_api_key <- import_api_key()
  # API Request
  if (identical(cm_api_key, "")) {
    api_environment <- "community"
    cm_api_key <- NULL
  } else {
    api_environment <- "production"
  }

  if (!is.null(query_args)) {
    query_args <- do.call(sanitize_query_params, query_args)
    query_args <- c(query_args, api_key = cm_api_key)
  } else {
    query_args <- list(api_key = cm_api_key)
  }

  headers <- c("Content-Type" = "application/json", "R-API-Client-Version" = utils::packageVersion("coinmetrics"))

  response <- httr::GET(
    url = construct_coinmetrics_api_http_url(endpoint_path = endpoint, api_environment),
    query = query_args, headers = headers
  )

  if (response$status_code != 200) {
    stop(sprintf("HTTP Error %s returned when calling url: %s", response$status_code, response$url), response)
  }

  return(response)
}

#' Get API Data
#' @param api_response API response as received from `httr::GET`
#' @param endpoint Corresponding API endpoint (i.e. `market-candles` or `index-levels`)
#' @param paging_from Enum: (`start`, `end`) Corresponding pagination in original request.
#' @param as_list Deprecated, included only for compatibility, to be removed later.
#' @return Data table
get_coinmetrics_api_data <- function(api_response,
                                     endpoint,
                                     paging_from,
                                     as_list = NULL) {
  api_content <- api_response |>
    httr::content(as = "raw") |>
    RcppSimdJson::fparse(
      query = c(data = "/data", next_page_url = "/next_page_url"),
      query_error_ok = TRUE,
      on_query_error = character(),
      max_simplify_lvl = 3L
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
        max_simplify_lvl = 3L
      )

    api_data <- switch(paging_from,
      end = purrr::list_c(list(api_content[["data"]], api_data)),
      start = purrr::list_c(list(api_data, api_content[["data"]]))
    )
  }

  rm(api_content)

  out <- data.table::rbindlist(api_data, fill = T)

  rm(api_data)

  if (endpoint %in% c("profile-assets", "reference-data")) {
    return(out)
  }

  time_cols <- grep(x = colnames(out), pattern = "time$", value = TRUE)
  out[, (time_cols) := lapply(.SD, anytime::anytime), .SDcols = time_cols]

  switch(endpoint,
    "index-levels" = {
      out[, c("level") := sapply(.SD, as.numeric), .SDcols = "level"]
    },
    "index-constituents" = {
      constituents <- NULL # due to NSE notes in R CMD check (from data.table)

      out[,
        c("asset", "weight") := list(
          asset = vapply(
            constituents,
            "asset",
            FUN = "[[",
            FUN.VALUE = character(1)
          ),
          weight = vapply(
            constituents,
            FUN = function(x) as.numeric(x[["weight"]]),
            FUN.VALUE = numeric(1)
          )
        ),
        by = c("time", "index")
      ]

      out[, c("constituents") := NULL]
    },
    "market-candles" = {
      candle_cols <- c(
        "price_open",
        "price_close",
        "price_high",
        "price_low",
        "vwap",
        "volume",
        "candle_usd_volume",
        "candle_trades_count"
      )

      out[, (candle_cols) := lapply(.SD, as.numeric), .SDcols = candle_cols]
    },
    "index-candles" = {
      candle_cols <- c(
        "price_open",
        "price_close",
        "price_high",
        "price_low",
        "candle_trades_count"
      )

      out[, (candle_cols) := lapply(.SD, as.numeric), .SDcols = candle_cols]
    },
    "market-trades" = {
      out[, c("coin_metrics_id", "amount", "price") := lapply(.SD, as.numeric), .SDcols = c("coin_metrics_id", "amount", "price")]
    },
    # "asset-metrics" = {
    #   metric_cols <- setdiff(colnames(out), c("time", "asset"))
    #   out[, (metric_cols) := lapply(.SD, readr::parse_guess), .SDcols = metric_cols]
    # },
    "market-metrics" = {
      metric_cols <- setdiff(colnames(out), c("time", "market"))
      out[, (metric_cols) := lapply(.SD, readr::parse_guess), .SDcols = metric_cols]
    },
    "exchange-asset-metrics" = {
      metric_cols <- setdiff(colnames(out), c("time", "exchange_asset"))
      out[, (metric_cols) := lapply(.SD, readr::parse_guess), .SDcols = metric_cols]
    },
    "taxonomy-assets" = {
      out[,
        c("class_id", "sector_id", "subsector_id") := lapply(.SD, as.integer),
        .SDcols = c("class_id", "sector_id", "subsector_id")
      ]
    },
    "taxonomy-metadata" = {
      subsectors <- NULL
      out[, c("class_id", "class", "sector_id", "sector", "subsector_id", "subsector") := data.table::rbindlist(subsectors)]
      out[, subsectors := NULL]
      out[,
        c("class_id", "sector_id", "subsector_id") := lapply(.SD, as.integer),
        .SDcols = c("class_id", "sector_id", "subsector_id")
      ]
    },
    {
      # Default option
      data_cols <- which(sapply(out, is.character))

      out[, (data_cols) := lapply(.SD, readr::parse_guess), .SDcols = data_cols]
    }
  )

  return(out[])
}

get_coinmetrics_api_data_legacy <- function(api_response,
                                            endpoint,
                                            paging_from,
                                            as_list = FALSE) {
  api_content <- api_response %>%
    httr::content()

  api_data <- api_response %>%
    httr::content() %>%
    purrr::pluck("data")


  while (is.null(api_content[["next_page_url"]]) == FALSE) {
    api_content <-
      httr::GET(url = api_content[["next_page_url"]]) %>%
      httr::content()

    if (paging_from == "end") {
      api_data <- c(api_content[["data"]], api_data)
    } else {
      api_data <- c(api_data, api_content[["data"]])
    }
  }

  if (as_list) {
    return(api_data)
  } else {
    if (endpoint == "market-orderbooks") {
      api_data <- tibble::tibble(
        time = anytime::anytime(purrr::map_chr(api_data, "time", .default = NA)),
        market = purrr::map_chr(api_data, "market", .default = NA),
        coin_metrics_id = purrr::map_chr(api_data, "coin_metrics_id", .default = NA),
        asks = purrr::map(api_data, "asks", .default = NA),
        bids = purrr::map(api_data, "bids", .default = NA)
      )
      return(api_data)
    }

    if (endpoint == "taxonomy-metadata/assets") {
      api_data <- api_data %>%
        data.table::rbindlist(fill = TRUE) %>%
        tidyr::hoist(
          .data$subsectors,
          "class_id", "class", "sector_id", "sector", "subsector_id", "subsector"
        ) %>%
        purrr::map_df(readr::parse_guess)

      return(api_data)
    }

    if (endpoint == "mining-pool-tips-summary") {
      api_data <- tibble::tibble(
        asset = purrr::map_chr(api_data, "asset", .default = NA),
        time = purrr::map_chr(api_data, "time", .default = NA),
        tips_count = purrr::map_chr(api_data, "tips_count", .default = NA),
        block_hashes_at_tip = purrr::map_chr(api_data, "block_hashes_at_tip", .default = NA),
        tips = purrr::map(api_data, "tips", .default = NA)
      ) %>%
        dplyr::mutate(
          time = anytime::anytime(.data$time),
          dplyr::across(c("tips_count", "block_hashes_at_tip"), as.numeric)
        ) %>%
        tidyr::unnest_longer(.data$tips) %>%
        tidyr::hoist(.data$tips, "last_time", "height", "hash", "pool_count",
          .transform = list(
            last_time = anytime::anytime,
            height = as.numeric,
            pool_count = as.numeric
          )
        )
      return(api_data)
    }

    if (endpoint == "mempool-feerates") {
      api_data <-
        data.table::rbindlist(api_data) %>%
        tidyr::hoist(.data$feerates, "feerate", "count", "consensus_size", "fees") %>%
        purrr::map_df(readr::parse_guess)

      return(api_data)
    }

    if (endpoint == "asset-chains") {
      api_data <- api_data %>%
        data.table::rbindlist(fill = TRUE) %>%
        tidyr::unnest(.data$chains) %>%
        tidyr::hoist(.data$chains, "hash", "height", "time", .transform = list(time = anytime::anytime))

      return(api_data)
    }

    if (endpoint == "index-constituents") {
      api_data <- tibble::tibble(
        index = purrr::map_chr(api_data, "index", .default = NA),
        time = purrr::map_chr(api_data, "time", .default = NA),
        constituents = purrr::map(api_data, "constituents", .default = NA)
      ) %>%
        tidyr::unnest(.data$constituents) %>%
        dplyr::mutate(
          asset = purrr::map_chr(.data$constituents, "asset", .default = NA),
          weight = purrr::map_chr(.data$constituents, "weight", .default = NA)
        ) %>%
        dplyr::mutate(
          time = anytime::anytime(.data$time),
          weight = as.numeric(.data$weight)
        ) %>%
        dplyr::select(-.data$constituents)
    } else {
      api_data <- api_data %>%
        data.table::rbindlist(fill = TRUE) %>%
        purrr::map_df(readr::parse_guess)
    }

    return(api_data)
  }
}

construct_coinmetrics_api_http_url <- function(endpoint_path, environment = "production", api_version = "v4") {
  root_url <- switch(environment,
    production = "https://api.coinmetrics.io/",
    community = "https://community-api.coinmetrics.io/"
  )


  httr::modify_url(
    url = root_url,
    path = c(api_version, endpoint_path)
  )
}
