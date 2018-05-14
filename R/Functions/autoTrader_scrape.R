# autotrader_scrape -------------------------------------------------------
# This function is the main function used to make a query and return data.
# It combines all of the functions in generalScrapeFunc.R into one general purpose function.

autoTrader_scrape <- function(locationName = NA, fork, write_csv = FALSE, path, ...)
{
  
  if(!missing(fork)){
    message(paste0("Using ", fork, " core(s) to scrape links"))
    cl <- makeForkCluster(fork)
  } else { ### If the user doesnt specify how many forks will be chosen, the available cores minus one will be used.
    fork <- parallel::detectCores() - 1
    message(paste0("Default: Using ", fork, " core(s) to scrape links"))
    cl <- makeForkCluster(fork)
  }
  
  primaryQuery <- autoTrader_query(...) 
  
  # This is where the magic happens
  out <- primaryQuery %>% 
    query_URL_reader() %>% 
    scraper_apply(cl) %>%
    clean_result_df(locationName)
  
  stopCluster(cl)
  rm(cl)
  
  if(write_csv != FALSE){
    if(missing(path)){
      Path = paste0("autoTradeR-", Sys.Date(), ".csv")
      
      i <- 1
      while(Path %in% list.files()){
        if(i == 1){
          Path = paste0(substr(Path, 1, nchar(Path) - 4), "_", i, ".csv")
        } else {
          Path = paste0(substr(Path, 1, nchar(Path) - 6), "_", i, ".csv")
        }
      i <- i + 1
      }
      
    } else {
      defaultPath = path
    }
    
    readr::write_csv(x = out, path = Path)
    return(message(paste0("Wrote csv file to: ", Path)))
  }
  
  return(out)
}

