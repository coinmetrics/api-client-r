#' Asset Metrics
#' @param metrics Vector of metrics. By default all metrics are returned.
#' @param reviewable Boolean, limit to human-reviewable metrics. By default all metrics are returned.
#' @param page_size Number of items per single page of results, default is 100.
#' @param paging_from Enum: `start` or `end`. Where does the first page start, at the start of the interval or at the end.
#' @return Data table of asset metrics metadata.
#' @export
reference_data_asset_metrics <- function(metrics = NULL,
                                         reviewable = NULL,
                                         page_size = NULL,
                                         paging_from = "start") {
  query_args <- list(
    metrics = metrics,
    reviewable = reviewable,
    page_size = page_size,
    paging_from = paging_from
  )

  resp <- send_coinmetrics_request(
    endpoint = "/reference-data/asset-metrics",
    query_args = query_args
  )
  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "reference-data",
    paging_from = paging_from
  )
}

#' Exchange Metrics
#' @inheritParams reference_data_asset_metrics
#' @return Data table of exchange metrics metadata.
#' @export
reference_data_exchange_metrics <- function(metrics = NULL,
                                            page_size = NULL,
                                            paging_from = "start") {
  query_args <- list(
    metrics = metrics,
    page_size = page_size,
    paging_from = paging_from
  )

  resp <- send_coinmetrics_request(
    endpoint = "/reference-data/exchange-metrics",
    query_args = query_args
  )

  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "reference-data",
    paging_from = paging_from
  )
}

#' Exchange-Asset Metrics
#' @inheritParams reference_data_asset_metrics
#' @return Data table of exchange asset metrics metadata.
#' @export
reference_data_exchange_asset_metrics <- function(metrics = NULL,
                                                  page_size = NULL,
                                                  paging_from = "start") {
  query_args <- list(
    metrics = metrics,
    page_size = page_size,
    paging_from = paging_from
  )

  resp <- send_coinmetrics_request(
    endpoint = "/reference-data/exchange-asset-metrics",
    query_args = query_args
  )

  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "reference-data",
    paging_from = paging_from
  )
}

#' Pair Metrics
#' @inheritParams reference_data_asset_metrics
#' @return Data table of pair metrics metadata.
#' @export
reference_data_pair_metrics <- function(metrics = NULL,
                                        page_size = NULL,
                                        paging_from = "start") {
  query_args <- list(
    metrics = metrics,
    page_size = page_size,
    paging_from = paging_from
  )

  resp <- send_coinmetrics_request(
    endpoint = "/reference-data/pair-metrics",
    query_args = query_args
  )

  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "reference-data",
    paging_from = paging_from
  )
}

#' Institution Metrics
#' @inheritParams reference_data_asset_metrics
#' @return Data table of institution metrics metadata.
#' @export
reference_data_institution_metrics <- function(metrics = NULL,
                                               page_size = NULL,
                                               paging_from = "start") {
  query_args <- list(
    metrics = metrics,
    page_size = page_size,
    paging_from = paging_from
  )

  resp <- send_coinmetrics_request(
    endpoint = "/reference-data/institution-metrics",
    query_args = query_args
  )

  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "reference-data",
    paging_from = paging_from
  )
}
