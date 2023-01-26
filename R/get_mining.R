#' Mining pool tips summary
#' @inheritParams get_asset_metrics
#' @param as_list Return the response as (nested) list or tibble.
#' @return Last tips we saw in all active mining pools during the past hour. See [API Documentation](https://docs.coinmetrics.io/api/v4#operation/getTimeseriesMiningPoolTipsSummary) for more information.
#' @export
get_mining_pool_tips <- function(assets,
                                 start_time = NULL,
                                 end_time = NULL,
                                 start_inclusive = TRUE,
                                 end_inclusive = TRUE,
                                 timezone = "UTC",
                                 page_size = NULL,
                                 paging_from = "end",
                                 pretty = TRUE,
                                 as_list = TRUE) {
  query_args <- list(
    assets = paste0(assets, collapse = ","),
    start_time = start_time,
    end_time = end_time,
    start_inclusive = start_inclusive,
    timezone = timezone,
    page_size = page_size,
    paging_from = paging_from,
    pretty = pretty
  )

  resp <- send_coinmetrics_request(endpoint = "timeseries/mining-pool-tips-summary", query_args = query_args)

  get_coinmetrics_api_data(api_response = resp, endpoint = "mining-pool-tips-summary", paging_from = paging_from, as_list = as_list)
}

#' Get Mempool feerates
#' @inheritParams get_mining_pool_tips
#' @return Mempool feerates for specified assets. This is useful for those who want to explore the history of the feerate distribution and
#' potentially select the time period with the lowest commissions.
#' @export
get_mempool_feerates <- function(assets,
                                 start_time = NULL,
                                 end_time = NULL,
                                 start_inclusive = TRUE,
                                 end_inclusive = TRUE,
                                 timezone = "UTC",
                                 page_size = NULL,
                                 paging_from = "end",
                                 pretty = TRUE,
                                 as_list = FALSE) {
  query_args <- list(
    assets = paste0(assets, collapse = ","),
    start_time = start_time,
    end_time = end_time,
    start_inclusive = start_inclusive,
    end_inclusive = end_inclusive,
    timezone = timezone,
    page_size = page_size,
    paging_from = paging_from,
    pretty = pretty
  )

  resp <- send_coinmetrics_request(endpoint = "timeseries/mempool-feerates", query_args = query_args)

  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "mempool-feerates",
    paging_from = paging_from,
    as_list = as_list
  )
}
