library(shiny)
library(tidyverse)
library(shinymaterial)
library(glue)
library(plotly)
library(pwr) # for power calcs
library(shinyWidgets)
library(flexdashboard)
library(shinydashboard)
library(DT)
library(crosstalk)
library(semantic.dashboard)
source("/Users/adriel/MAIN/Blog/autotradeR/R/Functions/get_listings.R")

ui <- material_page(
  
  title = "EZTrader LLC",
  
  nav_bar_color = "red",
  
  tags$br(),
  
  material_side_nav(
    fixed = FALSE, 
    image_source = "EZTrader.png",
    # Place side-nav tabs within side-nav
    material_side_nav_tabs(
      
      side_nav_tabs = c(
        "Listings Analyzer" = "analyzer",
        "Scheduled Reports" = "scheduler"
      ),
      
      icons = c(
        "assessment",
        "query_builder"
      ),
      
      color = "deep-orange accent-4"
    )
  ),
  
  
  ##### Side Nav Content --------------------------------------------------------
  
  # Analyzer -----------------------------------------------------------
  material_side_nav_tab_content(
    
    side_nav_tab_id = "analyzer",
    
    material_row(
      
      # Inputs
      material_column(width = 3,
                      
                      material_card("Scrape Data",
                        
                        material_card("Filters",

                          material_dropdown("milage", "Milage",
                                      choices = c(
                                        "Any" = "0",
                                        "Under 15,000" = "15000",
                                        "Under 30,000" = "30000",
                                        "Under 45,000" = "45000",
                                        "Under 60,000" = "60000",
                                        "Under 75,000" = "75000",
                                        "Under 100,000" = "100000",
                                        "Under 150,000" = "150000",
                                        "Under 200,000" = "200000",
                                        "Over 200,000" = "200001"
                                      ),
                                      selected = "75000"),

                          sliderInput("price", "Price",
                                      min = 0L,
                                      max = 100000L,
                                      value = c(0L, 25000L)),
# 
                          material_dropdown("seller_type", "Seller Type",
                                            choices = list(
                                              Private = 'p',
                                              Dealer = 'd',
                                              Any = c('p', 'd')
                                              ),
                                            selected = 'p',
                                            multiple = FALSE),

                          material_number_box("max_results", "Max Results",
                            min_value = 25L,
                            max_value = 3000L,
                            initial_value = 1000L
                          )
                        ),
                        
                        material_button("refresh","Refresh", icon = icon("fa fa-refresh"))
                        )
                      ),
      material_column(width = 8,
                      material_card("Data Viewer",
                                    material_row(
                                      dropdownButton(
                                        selectInput('select_cols', 'Select Columns',
                                                    choices = ROW_NAMES,
                                                    selected = c(
                                                      "name",
                                                      "price",
                                                      "mileage",
                                                      "production.date",
                                                      "seller.region",
                                                      "drivewheel",
                                                      "color",
                                                      "listing.url",
                                                      "VIN"
                                                    ),
                                                    multiple=TRUE),
                                        
                                        circle = TRUE,
                                        status = "danger",
                                        icon = icon("fa fa-gear"),
                                        width = "500px",
                                        tooltip = tooltipOptions(title = "Click to select columns")
                                      ) 
                                    ),
                                    dataTableOutput('listingsData'),
                                    downloadButton("downloadData", "Download")
                      )
                        
                      )
      )
    ),
  # Scheduler -----------------------------------------------------------
  material_side_nav_tab_content(
    
    side_nav_tab_id = "scheduler",
    
    material_row(
      # Inputs
      material_column(width = 6,
                      fluidPage(
                        tags$h2("Dropdown Button"),
                        br(),
                        dropdownButton(
                          tags$h3("List of Inputs"),
                          circle = TRUE, status = "danger",
                          icon = icon("gear"), width = "300px",
                          tooltip = tooltipOptions(title = "Click to see inputs !")
                          )
                        )
                      )
      )
    )
)