# autotrader_scrape -------------------------------------------------------
# This function is the main function used to make a query and return data.
# It combines all of the functions in generalScrapeFunc.R into one general purpose function.

autoTrader_scrape <- function(locationName = NA, fork, ...)
{
  
  if(!missing(fork)){
    cl <- makeForkCluster(fork)
  }
  
  primaryQuery <- autoTrader_query(...) 
  
  out <- primaryQuery %>% 
    query_URL_reader() %>% 
    scraper_apply(cl) %>%
    clean_result_df(locationName)
  
  if(!missing(fork)){
    stopCluster(cl)
    rm(cl)
  }
  
  return(out)
}

