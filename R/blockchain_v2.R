#' Get List of Blocks
#' @param asset Asset name.
#' @param block_hashes Optional vector of block hashes to filter a response. 
#' This _must_ contain a single element for Community API users.
#' @param heights Optional vector of block heights to filter a response. 
#' This _must_ contain a single element for Community API users.
#' @param start_time Start of the time interval. Mutually exclusive with `start_height`. 
#' If omitted, response will include time series from the **earliest** time available. 
#' This parameter is disabled for Community API users.
#' @param end_time End of the time interval. Mutually exclusive with `end_height`. 
#' If omitted, response will include time series up to the **latest** time available. 
#' This parameter is disabled for Community API users.
#' @param start_height Beginning block height for the set of data returned, inclusive by default. 
#' Mutually exclusive with `start_time`. This parameter is disabled for Community API users.
#' @param end_height Ending block height for the set of data returned, inclusive by default. 
#' Mutually exclusive with `end_time`. This parameter is disabled for Community API users.
#' @param chain Chain type. Default is `"main"`. Supported values are `"main"` and `"all"` (includes both main and stale). 
#' This parameter is disabled for Community API users.
#' @param start_inclusive Inclusive or exclusive corresponding `start_*` parameters. 
#' This parameter is disabled for Community API users.
#' @param end_inclusive Inclusive or exclusive corresponding `end_*` parameters. 
#' This parameter is disabled for Community API users.
#' @param timezone Timezone name (e.g. `America/New_York`) for `start_time` and `end_time` timestamps. Default is `UTC`. 
#' This parameter does not modify the output times, which are always UTC.
#' @param page_size Number of items per single page of results. Integer between 1 and 10000. 
#' This parameter is disabled for Community API users.
#' @param paging_from First page starts at the beginning or end of the interval. Supported values are `start` and `end`.
#' @return Tibble of blockchain blocks metadata. Results are ordered by tuple `(height, block_hash)`. 
#' Results are limited to the last 30 days for Community API users.
#' @export
get_list_of_blocks_v2 <- function(asset,
                                  block_hashes = NULL,
                                  heights = NULL,
                                  start_time = NULL,
                                  end_time = NULL,
                                  start_height = NULL,
                                  end_height = NULL,
                                  chain = NULL,
                                  start_inclusive = TRUE,
                                  end_inclusive = TRUE,
                                  timezone = "UTC",
                                  page_size = NULL,
                                  paging_from = "end") {
  
  query_args <- list(
    block_hashes = block_hashes,
    heights = heights,
    start_time = start_time,
    end_time = end_time,
    start_height = start_height,
    end_height = end_height,
    chain = chain,
    start_inclusive = start_inclusive,
    end_inclusive = end_inclusive,
    timezone = timezone,
    page_size = page_size,
    paging_from = paging_from
  )
  
  resp <- send_coinmetrics_request(
    endpoint = paste("blockchain-v2", asset, "blocks", sep = "/"),
    query_args = query_args
  )
  
  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "asset/blocks",
    paging_from = paging_from
  )
}

#' Get List of Accounts
#' @inheritParams get_list_of_blocks_v2
#' @param accounts Optional vector of accounts to filter a response. 
#' This _must_ contain a single element for Community API users. 
#' @param start_chain_sequence_number Start of the `chain_sequence_number` interval.  This parameter is disabled for Community API users.
#' @param end_chain_sequence_number End of the `chain_sequence_number` interval. This parameter is disabled for Community API users.
#' @return Tibble of blockchain account with their balances. 
#' Results are ordered by tuple `(creation_chain_sequence_number, account)`.
#' @export
get_list_of_accounts_v2 <- function(asset,
                                    accounts = NULL,
                                    start_time = NULL,
                                    end_time = NULL,
                                    start_height = NULL,
                                    end_height = NULL,
                                    start_chain_sequence_number = NULL,
                                    end_chain_sequence_number = NULL,
                                    start_inclusive = TRUE,
                                    end_inclusive = TRUE,
                                    timezone = "UTC",
                                    page_size = NULL,
                                    paging_from = "end") {
  
  query_args <- list(
    accounts = accounts,
    start_time = start_time,
    end_time = end_time,
    start_height = start_height,
    end_height = end_height,
    start_chain_sequence_number = start_chain_sequence_number,
    end_chain_sequence_number = end_chain_sequence_number,
    start_inclusive = start_inclusive,
    end_inclusive = end_inclusive,
    timezone = timezone,
    page_size = page_size,
    paging_from = paging_from
  )
  
  resp <- send_coinmetrics_request(
    endpoint = paste("blockchain-v2", asset, "accounts", sep = "/"),
    query_args = query_args
  )
  
  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "asset/accounts",
    paging_from = paging_from
  )
}

#' Get List of Sub-accounts
#' @inheritParams get_list_of_accounts_v2
#' @return Tibble of blockchain sub-accounts. Results are ordered by tuple `(creation_chain_sequence_number, account)`.
#' @export
get_list_of_sub_accounts_v2 <- function(asset,
                                        accounts = NULL,
                                        start_time = NULL,
                                        end_time = NULL,
                                        start_height = NULL,
                                        end_height = NULL,
                                        start_chain_sequence_number = NULL,
                                        end_chain_sequence_number = NULL,
                                        start_inclusive = TRUE,
                                        end_inclusive = TRUE,
                                        timezone = "UTC",
                                        page_size = NULL,
                                        paging_from = "end") {
  
  query_args <- list(
    accounts = accounts,
    start_time = start_time,
    end_time = end_time,
    start_height = start_height,
    end_height = end_height,
    start_chain_sequence_number = start_chain_sequence_number,
    end_chain_sequence_number = end_chain_sequence_number,
    start_inclusive = start_inclusive,
    end_inclusive = end_inclusive,
    timezone = timezone,
    page_size = page_size,
    paging_from = paging_from
  )
  
  resp <- send_coinmetrics_request(
    endpoint = paste("blockchain-v2", asset, "sub-accounts", sep = "/"),
    query_args = query_args
  )
  
  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "asset/sub-accounts",
    paging_from = paging_from
  )
  
}

#' Get List of Transactions
#' @inheritParams get_list_of_blocks_v2
#' @param txids Optional vector of transaction identifiers (txid) to filter a response. 
#' For Community API users, this vector must contain a single element.
#' @return Tibble of blockchain transactions metadata. Results are ordered by tuple `(tx_position, txid)`.
#' @export
get_list_of_transactions_v2 <- function(asset,
                                        txids = NULL,
                                        block_hashes = NULL,
                                        start_time = NULL,
                                        end_time = NULL,
                                        start_height = NULL,
                                        end_height = NULL,
                                        chain = NULL,
                                        start_inclusive = TRUE,
                                        end_inclusive = TRUE,
                                        timezone = "UTC",
                                        page_size = NULL,
                                        paging_from = "end") {
  
  query_args <- list(
    txids = txids,
    block_hashes = block_hashes,
    start_time = start_time,
    end_time = end_time,
    start_height = start_height,
    end_height = end_height,
    chain = chain,
    start_inclusive = start_inclusive,
    end_inclusive = end_inclusive,
    timezone = timezone,
    page_size = page_size,
    paging_from = paging_from
  )
  
  resp <- send_coinmetrics_request(
    endpoint = paste("blockchain-v2", asset, "transactions", sep = "/"),
    query_args = query_args
  )
  
  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "asset/transactions",
    paging_from = paging_from
  )
}

#' Get List of Balance Updates
#' @inheritParams get_list_of_transactions_v2
#' @inheritParams get_list_of_accounts_v2
#' @param limit_per_account Number of entries per account the result should contain.  Applicable when multiiple accounts are requested. 
#' For Community API users, this parameter is disabled and the limit is fixed to 100.
#' @param sub_accounts Optional vector of sub-accounts to filter a response. 
#' This parameter is disabled for Community API users.
#' @param include_sub_accounts Boolean indicating if the response should contain sub-accounts. 
#' This parameter is disabled for Community API users.
#' @return List of blockchain accounts balance updates.
#' @export
get_list_of_balance_updates_v2 <- function(asset,
                                           accounts = NULL,
                                           limit_per_account = NULL,
                                           sub_accounts = NULL,
                                           txids = NULL,
                                           block_hashes = NULL,
                                           start_time = NULL,
                                           end_time = NULL,
                                           start_height = NULL,
                                           end_height = NULL,
                                           start_chain_sequence_number = NULL,
                                           end_chain_sequence_number = NULL,
                                           include_sub_accounts = NULL,
                                           chain = NULL,
                                           start_inclusive = TRUE,
                                           end_inclusive = TRUE,
                                           timezone = "UTC",
                                           page_size = NULL,
                                           paging_from = "end") {
  
  query_args <- list(
    accounts = accounts,
    limit_per_account = limit_per_account,
    sub_accounts = sub_accounts,
    txids = txids,
    block_hashes = block_hashes,
    start_time = start_time,
    end_time = end_time,
    start_height = start_height,
    end_height = end_height,
    start_chain_sequence_number = start_chain_sequence_number,
    end_chain_sequence_number = end_chain_sequence_number,
    include_sub_accounts = include_sub_accounts,
    chain = chain,
    start_inclusive = start_inclusive,
    end_inclusive = end_inclusive,
    timezone = timezone,
    page_size = page_size,
    paging_from = paging_from
  )
  
  resp <- send_coinmetrics_request(
    endpoint = paste("blockchain-v2", asset, "balance-updates", sep = "/"),
    query_args = query_args
  )
  
  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "asset/balance-updates",
    paging_from = paging_from, 
    as_list=TRUE
  )
}