#' Taxonomy Endpoints
#'
#' @param assets vector of assets
#' @param class_ids vector of class identifiers
#' @param sector_ids vector of sector identifiers
#' @param subsector_ids vector of subsector identifiers
#' @param version Taxonomy version. Defaults to `latest` when no `*_time` parameters specified. Specify asterisk `*` to get all versions.
#' @param classification_start_time start time of asset classification
#' @param classification_end_time end time of asset classification
#' @param start_inclusive inclusive or exclusive corresponding `start_*` parameters
#' @param end_inclusive inclusive or exclusive corresponding `end_*` parameters
#' @param page_size number of items per single page of results
#' @param paging_from where the first page starts:  `start` or `end`
#' @param pretty Human-readable formatting of JSON responses
#' @return Taxonomy for assets, ordered by tuple `(asset, classification_start_time)`
#' @export
get_taxonomy <- function(assets = NULL,
                         class_ids = NULL,
                         sector_ids = NULL,
                         subsector_ids = NULL,
                         version = "latest",
                         classification_start_time = NULL,
                         classification_end_time = NULL,
                         start_inclusive = TRUE,
                         end_inclusive = TRUE,
                         page_size = 500,
                         paging_from = "start",
                         pretty = FALSE) {
  query_args <- list(
    assets = assets,
    class_ids = class_ids,
    sector_ids = sector_ids,
    subsector_ids = subsector_ids,
    version = version,
    classification_start_time = classification_start_time,
    classification_end_time = classification_end_time,
    start_inclusive = start_inclusive,
    end_inclusive = end_inclusive,
    page_size = page_size,
    pretty = pretty
  )

  resp <- send_coinmetrics_request(endpoint = "taxonomy/assets", query_args = query_args)

  get_coinmetrics_api_data(resp, "taxonomy/assets", paging_from)
}

#' Taxonomy Metadata
#' @inheritParams get_taxonomy
#' @param start_time Start time of taxonomy version.
#' @param end_time End time of taxonomy version.
#' @return Taxonomy metadata for assets
#' @export
get_taxonomy_metadata <- function(version = "latest",
                                  start_time = NULL,
                                  end_time = NULL,
                                  start_inclusive = TRUE,
                                  end_inclusive = TRUE,
                                  page_size = NULL,
                                  paging_from = "start",
                                  pretty = FALSE) {
  query_args <- list(
    version = version,
    start_time = start_time,
    end_time = end_time,
    start_inclusive = start_inclusive,
    end_inclusive = end_inclusive,
    page_size = page_size,
    paging_from = paging_from,
    pretty = pretty
  )

  resp <- send_coinmetrics_request(endpoint = "taxonomy-metadata/assets", query_args = query_args)

  get_coinmetrics_api_data(resp, "taxonomy-metadata/assets", paging_from)
}

#' Asset profiles data (experimental)
#' @param assets Vector of assets.
#' @param full_names Vector of asset full names. Mutually exclusive with `assets` parameter.
#' @param page_size Number of items per single page of results.
#' @param paging_from where the first page starts:  `start` or `end`.
#' @param pretty Human-readable formatting of JSON responses.
#' @return Profile data for assets. If `supply_cap` is not present then the theoretical maximum supply is infinite for that asset.
#' @export
get_asset_profiles <- function(assets = NULL,
                               full_names = NULL,
                               page_size = 1000,
                               paging_from = "start",
                               pretty = FALSE) {
  query_args <- list(
    assets = assets,
    full_names = full_names,
    page_size = page_size,
    paging_from = paging_from,
    pretty = pretty
  )

  resp <- send_coinmetrics_request("profile/assets", query_args)

  get_coinmetrics_api_data(resp, "profile/assets", paging_from)
}
