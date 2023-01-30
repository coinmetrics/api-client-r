catalogMarketsData <- function(api_data) {
  
  df_markets <- data.table::rbindlist(api_data, fill = TRUE)
  
  metadata <- c("funding_rates", "openinterest", "liquidations")
  
  for(col in metadata) {
    if (col %in% colnames(df_markets)) {
      df_markets <- tidyr::unnest(df_markets, tidyselect::any_of(col))
    }
  }
  
  df_markets <- df_markets %>% 
    dplyr::select(-tidyselect::any_of(c("trades", metadata))) %>%
    tidyr::unnest_longer(tidyselect::any_of(c("orderbooks", "quotes")))
  #list_cols <- which(sapply(df_markets, is.list))
  
  df_markets <- df_markets %>%
    utils::type.convert(as.is = TRUE) %>%
    dplyr::mutate(
      dplyr::across(tidyselect::any_of(c("min_time", "max_time", "listing", "expiration", "orderbooks", "quotes")), 
                    anytime::anytime)
    )
   
  return(df_markets)
}