source("/Users/adriel/MAIN/Blog/autotradeR/R/Functions/get_listings.R")
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
  
  rV <- reactiveValues(df = {
    df <- data.frame(NA)
    for(col in ROW_NAMES) df[[col]] <- c(NA)
    df
  })
  
  #A test action button
  observeEvent(input$refresh, withProgress(message = 'Scraping data', value = 0, {
    
    tryCatch({
      
      rV$df <- get_filtered_listings(max_results = input$max_results,
                                     minPrice = input$price[1],
                                     maxPrice = input$price[2],
                                     maxMileage = input$milage,
                                     sellerType = input$seller_type,
                                     fork = 32)
      showNotification(
        ui = paste0("Scraped ", count(rV$df)[1], " listings"),
        type = "message",
        session = session)
      
      rV$df_select <- rV$df %>% select(input$select_cols)
      
    }, error=function(e) showNotification(
      ui = paste0("Scraping Error:  ", e),
      type = "error",
      session = session))
    
  }), ignoreNULL = TRUE, ignoreInit = TRUE)
  
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
