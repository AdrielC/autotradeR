library(tidyverse)
library(rvest) # read_html functions and the like
library(httr)
library(pbapply) # For progress bars in queries
library(parallel) # For parallel processing

source("autotrader_scrape_main.R")
source("data_clean.R")
source("info_extractors.R")
source("scrape_utils.R")

# This queries autotrader for 1000 maximum results for Jeep Wranglers around 86404 in asceding distance.
# Private sellers are specified, and the tag "Provo" is added as a column value for this query.
# This will use 8 cores available to parallelize the web scraping.

ProvoJeepP <- autoTrader_scrape(make = "Jeep", model = "Wrangler", zip = 84604, pages = "all",
                  sellerType = "p", locationName = "Provo", fork = 8)

ProvoJeepD <- autoTrader_scrape(make = "Jeep", model = "Wrangler", zip = 84604, pages = "all",
                               sellerType = "d", locationName = "Provo", fork = 8)

