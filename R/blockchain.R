#' Get List of Blocks
#' @param asset Asset name.
#' @param block_hashes Optional vector of block hashes to filter a response.
#' @param heights Optional vector of block heights to filter a response.
#' @param start_time Start of the time interval. This field refers to the `time` field in the response.
#' @param end_time End of the time interval. This field refers to the `time` field in the response.
#' @param start_height (int >= 0) Beginning block height for the set of data returned.  Inclusive by default. Mutually exclusive with `start_time`.
#' @param end_height (int >= 0) Ending block height for the set of data returned. Inclusive by default. Mutually exclusive with `end_time`.
#' @param start_inclusive Inclusive or exclusive corresponding `start_*` parameters.
#' @param end_inclusive Inclusive or exclusive corresponding `end_*` parameters.
#' @param timezone Timezone name for `start_time` and `end_time` timestamps, `"UTC"` by default. Output times are always `UTC`.
#' @param page_size Number of items per single page of results.
#' @param paging_from First page at start or end of interval. Enum: `"start"` or `"end"`.
#' @return Tibble of blockchain blocks metadata. Results are ordered by tuple `(height, block_hash)`.
#' @export
get_list_of_blocks <- function(asset,
                               block_hashes = NULL,
                               heights = NULL,
                               start_time = NULL,
                               end_time = NULL,
                               start_height = NULL,
                               end_height = NULL,
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
    start_inclusive = start_inclusive,
    end_inclusive = end_inclusive,
    timezone = timezone,
    page_size = page_size,
    paging_from = paging_from
  )

  resp <- send_coinmetrics_request(endpoint = paste("blockchain", asset, "blocks", sep = "/"), query_args = query_args)

  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "asset/blocks",
    paging_from = paging_from
  )
}

#' Get List of Accounts
#' @inheritParams get_list_of_blocks
#' @param accounts Optional vector of accounts to filter a response.
#' @param start_chain_sequence_number Non-negative integer, start of the `chain_sequence_number` interval.
#' @param end_chain_sequence_number Non-negative integer, end of the `chain_sequence_number` interval.
#' @return Tibble of blockchain accounts with their balances.
#' Results are ordered by tuple `(creation_chain_sequence_number, account)`.
#' @export
get_list_of_accounts <- function(asset,
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

  resp <- send_coinmetrics_request(endpoint = paste("blockchain", asset, "accounts", sep = "/"), query_args = query_args)

  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "asset/accounts",
    paging_from = paging_from
  )
}

#' Get List of Transactions
#' @inheritParams get_list_of_blocks
#' @param transaction_hashes Optional vector of transaction hashes to filter a response.
#' @return Tibble of blockchain transactions metadata.
#' Results are ordered by tuple `(min_chain_sequence_number, transaction_hash)`.
#' @export
get_list_of_transactions <- function(asset,
                                     transaction_hashes = NULL,
                                     block_hashes = NULL,
                                     start_time = NULL,
                                     end_time = NULL,
                                     start_height = NULL,
                                     end_height = NULL,
                                     start_inclusive = TRUE,
                                     end_inclusive = TRUE,
                                     timezone = "UTC",
                                     page_size = NULL,
                                     paging_from = "end") {
  query_args <- list(
    transaction_hashes = transaction_hashes,
    block_hashes = block_hashes,
    start_time = start_time,
    end_time = end_time,
    start_height = start_height,
    end_height = end_height,
    start_inclusive = start_inclusive,
    end_inclusive = end_inclusive,
    timezone = timezone,
    page_size = page_size,
    paging_from = paging_from
  )

  resp <- send_coinmetrics_request(
    endpoint = paste("blockchain", asset, "transactions", sep = "/"),
    query_args = query_args
  )

  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "asset/transactions",
    paging_from = paging_from
  )
}

#' Get List of Balance Updates
#' @inheritParams get_list_of_accounts
#' @param block_hashes Optional vector of block hashes to filter a response.
#' @param transaction_hashes Optional vector of transaction hashes to filter a response.
#' @return Tibble of blockchain accounts balance updates.
#' Results are ordered by tuple `(chain_sequence_number, block_hash)`.
#' @export
get_list_of_balance_updates <- function(asset,
                                        accounts = NULL,
                                        transaction_hashes = NULL,
                                        block_hashes = NULL,
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
    transaction_hashes = transaction_hashes,
    block_hashes = block_hashes,
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
    endpoint = paste("blockchain", asset, "balance-updates", sep = "/"),
    query_args = query_args
  )

  get_coinmetrics_api_data(
    api_response = resp,
    endpoint = "asset/balance-updates",
    paging_from = paging_from
  )
}

#' Get Full Transaction
#' @param asset Asset name.
#' @param txid Transaction identifier.
#' @return Full blockchain transaction with all balance updates. 
#' The result is a list of two tibbles. The first element, `metadata`, contains summary information for the transaction.
#' The second element, `balance_updates` lists the balance updates for the transaction.
#' @export
get_full_transaction <- function(asset, txid) {
  
  resp <- send_coinmetrics_request(
    endpoint = paste("/blockchain", asset, "transactions", txid, sep = "/")
  )
  
  tx_ls <- 
    httr::content(resp, as = 'raw') |>
    RcppSimdJson::fparse()
  
  tx_data <- 
    tibble::new_tibble(tx_ls[-9]) |>
    dplyr::mutate(
      dplyr::across('consensus_time', lubridate::ymd_hms),
      dplyr::across(
        c('min_chain_sequence_number', 
          'max_chain_sequence_number',
          'n_balance_updates', 
          'amount', 
          'height'),
        as.numeric
      )
    )
  
  tx_updates <-
    tibble::new_tibble(tx_ls[[9]]) |>
    dplyr::mutate(
      dplyr::across(-'account', as.numeric)
    )
  
  list(
    metadata = tx_data,
    balance_updates  = tx_updates
  )
}

#' Full block
#' @param asset Asset name
#' @param block_hash Block hash
#' @return Full blockchain blok with all transactions and balance updates.
#' The result is a list of threee tibbles. The first element, `metadata`, contains summary information for the block.
#' The second element, `block_transactions` list the transactions for the block.
#' The third element, `balance_updates` lists the balance updates the block.
#' @export
get_full_block <- function(asset, block_hash) {
  
  resp <- send_coinmetrics_request(
    endpoint = paste("/blockchain", asset, "blocks", block_hash, sep = "/")
  )
  
  block_ls <-
    httr::content(resp, as = 'raw') |>
    RcppSimdJson::fparse()
  
  block_metadata <- tibble::new_tibble(
    block_ls[!names(block_ls) %in% c('transactions', 'balance_updates')]
    ) |>
    dplyr::mutate(dplyr::across(dplyr::any_of(
      c(
        'height',
        'n_transactions',
        'n_balance_updates',
        'difficulty',
        'physical_size',
        'consensus_size',
        'consensus_size_limit'
      )
    ),
    as.numeric),
    dplyr::across(c('consensus_time', 'miner_time'), lubridate::ymd_hms))
  
  block_transactions <- tibble::new_tibble(
    block_ls[["transactions"]]
  ) |>
    dplyr::mutate(
      dplyr::across('consensus_time', lubridate::ymd_hms),
      dplyr::across(3:6, as.numeric)
    )
  
  block_updates <- tibble::new_tibble(
    block_ls[["balance_updates"]]
  ) |>
    dplyr::mutate(
      dplyr::across(-'account', as.numeric)
    )
  
  list(
    metadata = block_metadata,
    transactions = block_transactions,
    balance_updates = block_updates
  )
}

#' Full Transaction for Block
#' @param asset Asset name
#' @param block_hash Block hash
#' @param txid Transaction identifier (txid)
#' @return Full blockchain transaction with all balance updates for a specific block.
#' The result is a list of two tibbles. The first element, `metadata`, contains summary information for the transaction.
#' The second element, `balance_updates` lists the balance updates for the transaction.
#' @export
get_full_transaction_for_block <- function(asset, block_hash, txid) {
  
  resp <- send_coinmetrics_request(
    endpoint = paste('blockchain', asset, 'blocks', block_hash, 'transactions', txid, sep = '/')
  )
  
  tx_ls <-
    httr::content(resp, as = 'raw') |>
    RcppSimdJson::fparse()
  
  tx_data <- 
    tibble::new_tibble(tx_ls[-9]) |>
    dplyr::mutate(
      dplyr::across('consensus_time', lubridate::ymd_hms),
      dplyr::across(
        c('min_chain_sequence_number', 
          'max_chain_sequence_number',
          'n_balance_updates', 
          'amount', 
          'height'),
        as.numeric
      )
    )
  
  tx_updates <-
    tibble::new_tibble(tx_ls[[9]]) |>
    dplyr::mutate(
      dplyr::across(-'account', as.numeric)
    )
  
  list(
    metadata = tx_data,
    balance_updates  = tx_updates
  )
}

#' Block Settlements
#' @param asset Asset name.
#' @return Tibble of block settlements
#' @export
get_block_settlements <- function(asset) {
  
  resp <- send_coinmetrics_request(paste('blockchain', asset, 'settlement', sep = '/'))
  
  settlements <- 
    httr::content(resp, as = 'raw') |>
    RcppSimdJson::fparse(query = '/data') |>
    tibble::new_tibble() |>
    dplyr::mutate(
      dplyr::across('consensus_time', lubridate::ymd_hms),
      dplyr::across(
        dplyr::any_of(c('n_transactions', 'physical_size', 'feerate_min', 'feerate_max', 'feerate_mean')),
        as.numeric
      )
    )
  
  return(settlements)
}