api_response <- send_coinmetrics_request(
  endpoint = "timeseries/index-constituents",
  query_args = list(
    indexes = c(
      "CMBI10", "CMBIBE", "CMBIMTAE", "CMBIDFIE", "CMBISCPE",
      "CMBI10E", "CMBI10M"
    ),
    frequency = "1d-ny-close",
    page_size = 10000
  )
)
paging_from <- "end"

get_data_v1 <- function(api_response, paging_from = "end") {
  api_content <- httr::content(api_response, as = "raw") %>%
    RcppSimdJson::fparse(
      query = c(data = "/data", next_page_url = "/next_page_url"),
      query_error_ok = TRUE,
      max_simplify_lvl = 3L
    )

  api_data <- api_content[["data"]]

  while (!is.null(api_content[["next_page_url"]])) {
    api_content <- httr::GET(api_content[["next_page_url"]]) %>%
      httr::content(as = "raw") %>%
      RcppSimdJson::fparse(
        query = c(data = "/data", next_page_url = "/next_page_url"),
        query_error_ok = TRUE,
        max_simplify_lvl = 3L
      )

    # if (paging_from == "end") {
    #   api_data <- c(api_content[["data"]], api_data)
    # } else {
    #   api_data <- c(api_data, api_content[["data"]])
    # }

    api_data <- switch(paging_from,
      start = c(api_data, api_content[["data"]]),
      end = c(api_content[["data"]], api_data)
    )
  }

  df <- data.table::rbindlist(api_data)
  # df[, "time" := lapply(.SD, lubridate::ymd_hms), .SDcols = "time"]
  df[, c("asset", "weight") := data.table::rbindlist(constituents, use.names = T, fill = T), by = c("index", "time")]
  # df[, weight := lapply(.SD, as.numeric), .SDcols = "weight"]
  df[, constituents := NULL]
  df[, c("time", "weight") := lapply(.SD, readr::parse_guess), .SDcols = c("time", "weight")]

  tibble::as_tibble(df)
}


# bench::mark(
#   dt = get_data_v1(api_response),
#   current = get_coinmetrics_api_data(api_response, "index-constituents", "end"),
#   check = FALSE, filter_gc = F
# )

system.time(dt <- get_data_v1(api_response))
system.time(cm <- get_coinmetrics_api_data(api_response, "index-constituents", "end"))

d1 <- RcppSimdJson::fload(api_content$next_page_url, compressed_download = T) |> data.table::rbindlist()
d2 <- RcppSimdJson::fload(api_content$next_page_url, compressed_download = F) |> data.table::rbindlist()

a <- RcppSimdJson::fload(api_response$url, compressed_download = TRUE)

api_response <- send_coinmetrics_request(endpoint = paste("blockchain-v2", asset, "balance-updates", sep = "/"))
