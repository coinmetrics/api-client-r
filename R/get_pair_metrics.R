#' Pair Metrics
#'
#' @inheritParams get_index_levels
#' @param pairs Vector of asset pairs, e.g. btc-usd.
#' @param metrics Vector of metrics to request, e.g. volume_trusted_spot_usd_1d.
#' @param frequency Frequency of the pair metrics. Supported values are `1h` and `1d`.
#' @param limit_per_pair Number of entries per pair the result should contain.
#' @param format Format of the response, `json` or `csv`. Default is `json`.
#' @param sort How results will be sorted. Metrics are sorted by `(pair, time)` by default. 
#' If you want to sort `1d` metrics by `(time, pair)`, choose `sort = "time"`.
#' @return tibble or list of time series asset pairs
#' @export
get_pair_metrics <- function(pairs,
                             metrics,
                             frequency = "1d",
                             start_time = NULL,
                             end_time = NULL,
                             start_inclusive = TRUE,
                             end_inclusive = TRUE,
                             timezone = "UTC",
                             page_size = NULL,
                             paging_from = "end",
                             sort = "pair",
                             limit_per_pair = NULL,
                             pretty = FALSE,
                             format = "json",
                             as_list = FALSE) {
  query_args <- list(
    pairs = paste0(pairs, collapse = ","),
    metrics = paste0(metrics, collapse = ","),
    frequency = frequency,
    start_time = start_time,
    end_time = end_time,
    start_inclusive = start_inclusive,
    end_inclusive = end_inclusive,
    timezone = timezone,
    page_size = page_size,
    paging_from = paging_from,
    sort = sort,
    limit_per_pair = limit_per_pair,
    pretty = pretty,
    format = format
  )

  resp <- send_coinmetrics_request(endpoint = "timeseries/pair-metrics", query_args = query_args)

  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "pair-metrics",
    paging_from = paging_from,
    as_list = as_list
  )
}
