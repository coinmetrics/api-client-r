# Coin Metrics R API Client

This is an official R API Client for the Coin Metetrics API v4. Please note this is an initial implementation and is considered a beta product, it is not fully tested 
and intended be used for data analysis primarily. The [Coin Metrics Python API Client](https://github.com/coinmetrics/api-client-python) is tested and implemented more 
thoroughly and is better suited for production workflows.  

## Installation

```r
install.packages("devtools")
devtools::install_github("coinmetrics/api-client-r")
```

## Introduction

You can use this client to query data from the [CoinMetrics API V4](https://docs.coinmetrics.io/api/v4).
You can just set your API Key as an environment variable and get Coin Metrics data into R programs. If no API is set it will
default to the community API. 

```r
> library(coinmetrics)
> Sys.setenv("CM_API_KEY"="<YOUR_API_KEY>")
> assets = c("btc", "eth", "avax")
> start_date = "2023-01-01"
> frequency="1h"
> coinmetrics::get_asset_metrics(assets = assets, frequency = frequency, start_time = start_date, metrics = c("ReferenceRateUSD"))
# A tibble: 1,281 × 3
   asset time                ReferenceRateUSD
   <chr> <dttm>                         <dbl>
 1 avax  2023-01-01 00:00:00             10.9
 2 avax  2023-01-01 01:00:00             10.8
 3 avax  2023-01-01 02:00:00             10.8
 4 avax  2023-01-01 03:00:00             10.8
 5 avax  2023-01-01 04:00:00             10.8
 6 avax  2023-01-01 05:00:00             10.7
 7 avax  2023-01-01 06:00:00             10.7
 8 avax  2023-01-01 07:00:00             10.7
 9 avax  2023-01-01 08:00:00             10.7
10 avax  2023-01-01 09:00:00             10.7
# … with 1,271 more rows
# ℹ Use `print(n = ...)` to see more rows
```

By default, data from the API is returned as a tibble so it can be easily manipulated. For every API endpoints supported
in the coinmetrics docs, there is a related function that follows the same format. 

## More information

For more information on Coin Metrics or the API Offering please see [the website](https://coinmetrics.io/api-v4/). 