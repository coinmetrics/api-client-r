#' Get Institution Metrics
#'
#' @param institutions Vector of institutions.
#' @param metrics Vector of metrics to request time series data.
#' @param frequency Frequency of the institution metrics. Supported values are `1d`.
#' @param start_time Start of the time interval.
#' @param end_time End of the time interval.
#' @param start_inclusive Inclusive or exclusive corresponding `start_*` parameters.
#' @param end_inclusive Inclusive or exclusive corresponding `end_*` parameters.
#' @param timezone Timezone for `start_time` and `end_time` timestamps. Example: America/New_York.
#' @param page_size Number of items per single page of results.
#' @param paging_from First page start at the start or end of the interval.
#' @param sort How results will be sorted; sorted by `(institution, time)` by default.
#' @param limit_per_institution Number of entries per institution.
#' @param pretty Human-readable formatting of JSON responses.
#' @param format Either `"json"` or "`"csv"` response format.
#' @param as_list Return data as list.
#'
#' @return Tibble or list of metrics for specified institutions.
#' @export
get_institution_metrics <- function(institutions,
                                    metrics,
                                    frequency = "1d",
                                    start_time = NULL,
                                    end_time = NULL,
                                    start_inclusive = TRUE,
                                    end_inclusive = TRUE,
                                    timezone = "UTC",
                                    page_size = NULL,
                                    paging_from = "end",
                                    sort = "institution",
                                    limit_per_institution = NULL,
                                    pretty = FALSE,
                                    format = "json",
                                    as_list = FALSE) {
  query_args <- list(
    institutions = paste0(institutions, collapse = ","),
    metrics = paste0(metrics, collapse = ","),
    start_time = start_time,
    end_time = end_time,
    start_inclusive = start_inclusive,
    end_inclusive = end_inclusive,
    timezone = timezone,
    page_size = page_size,
    paging_from = paging_from,
    sort = sort,
    limit_per_institution = limit_per_institution,
    pretty = pretty,
    format = format
  )

  resp <- send_coinmetrics_request(endpoint = "timeseries/institution-metrics", query_args = query_args)

  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "institution-metrics",
    paging_from = paging_from,
    as_list = as_list
  )
}
