source("/Users/adriel/MAIN/Blog/autotradeR/R/Functions/get_listings.R")
source("/Users/adriel/MAIN/Blog/autotradeR/R/global.R")
#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  rV <- reactiveValues(df = empty_auto_df())
  
  scrape <- function(cores = NULL, retry=TRUE, timeout=200) tryCatch(R.utils::withTimeout({

    df <- get_filtered_listings(max_results = input$max_results,
                                minPrice = input$price[1],
                                maxPrice = input$price[2],
                                maxMileage = input$milage,
                                sellerType = comma_sep(input$seller_type),
                                make = comma_sep(map(input$make, function(m) makes_lookup[[m]])),
                                fork = cores) %>% select(ROW_NAMES)
    
    rV$df %<>%
      rbind(df) %>%
      distinct(VIN, .keep_all = TRUE)
    
    output$columns <- renderText(names(rV$df))
  
    rV$df_select <- rV$df %>% select(input$select_cols)
    
    showNotification(
      ui = paste0("Scraped ", count(rV$df)[1], " listings"),
      type = "message",
      session = session)
    
  }, onTimeout = "error", timeout = timeout),
  error=function(e) {
    
    if(is(e, "TimeoutException") && retry) {
      
      showNotification(
        ui = paste0("Scraping timed out:  ", e, ". Retrying"),
        type = "warning",
        session = session)
      scrape(cores=NULL, retry=FALSE, timeout=timeout * 5)
      
    } else {
      
      showNotification(
        ui = paste0("Scraping error:  ", e),
        type = "error",
        session = session)
    }
  })
  
  #A test action button
  observeEvent(input$refresh,
               withProgress(message = 'Scraping data',value = 0,
                            scrape(cores=32)),
               ignoreNULL = TRUE, ignoreInit = TRUE)
  
  output$listingsData <- renderDataTable(rV$df_select, options = list(lengthChange = FALSE))
  
  observeEvent(input$select_cols, {
    rV$df_select <- rV$df %>% select(input$select_cols)
  }, ignoreNULL = TRUE)
  
  output$listingsData <- renderDataTable(rV$df_select, options = list(lengthChange = FALSE))
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(Sys.time(), "_scrape.csv", sep = "")
    },
    content = function(file) {
      write.csv(rV$df_select, file, row.names = FALSE)
    }
  )
})
