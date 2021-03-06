library(tidyverse)
library(rvest) # read_html functions and the like
library(httr)
library(pbapply) # For progress bars in queries
library(parallel) # For parallel processing
library(pbapply)
library(reticulate)
library(corrgram)

print(getwd())
setwd("Data/")

source("../R/Functions/autoTrader_scrape.R")
source("../R/Functions/autoTrader_query.R")
source("../R/Functions/data_clean.R")
source("../R/Functions/info_extractors.R")
source("../R/Functions/scrape_utils.R")

# This queries autotrader for 1000 maximum results for Jeep Wranglers around 86404 in asceding distance.
# Private sellers are specified, and the tag "Provo" is added as a column value for this query.
# This will use 8 cores available to parallelize the web scraping.

cities <- read_csv("major_cities.csv")
cities <- cities %>% 
  mutate(Zip = substr(Zip, 0, 5))

jeepModels <- c("Cherokee", 
               "CJ", 
               "Comanche", 
               "Commander", 
               "Compass", 
               "Grand+Cherokee", 
               "Grand+Wagoneer",
               "Liberty",
               "Patriot",
               "Renegade",
               "Wrangler")

for(model in seq_along(jeepModels)){
  
  for(i in 1:nrow(cities)){
    
    tryCatch(autoTrader_scrape(make = "Jeep", model = jeepModels[model], zip = cities$Zip[i], pages = "all",
                               sellerType = "d", locationName = cities$City[i], fork = 8, write_csv = T),
             
             error = function(e){
               Sys.sleep(abs(as.integer(rnorm(n = 1, mean = 10, sd = 4))))
               
               tryCatch(autoTrader_scrape(make = "Jeep", model = "Wrangler", zip = cities$Zip[i], pages = "all",
                                          sellerType = "d", locationName = cities$City[i], fork = 8, 
                                          write_csv = T),
                        
                        error = function(e){
                          return(Sys.sleep(abs(as.integer(rnorm(n = 1, mean = 20, sd = 4)))))
                        })
               
               Sys.sleep(abs(as.integer(rnorm(n = 1, mean = 10, sd = 5))))
             })
  }
}
