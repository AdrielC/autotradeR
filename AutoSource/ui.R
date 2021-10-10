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
                      
                      material_card("Pull Listings",
                        
                        material_card("Filters",
                          
                          sliderInput("milage", "Milage",
                                      min = 0L, 
                                      max = 200000L,
                                      value = c(0L, 60000L)),
                          
                          sliderInput("price", "Price",
                                      min = 1000, 
                                      max = 100000,
                                      value = c(1, 25000)),
                          
                          material_dropdown("sellerType", "Seller Type",
                                            choices = c(
                                              Private = 'p',
                                              Dealer = 'd'
                                            ),
                                            selected = 'p',
                                            multiple = TRUE),
                          
                          material_number_box("maxResults", "Max Results",
                            min_value = 1L,
                            max_value = 3000L,
                            initial_value = 1000L
                          )
                        ),
                        
                        material_button(
                          input_id = "run",
                          label = "Run"
                        ))
                      ),
      material_column(width = 8,
                      material_card("Data Viewer",
                                    dataTableOutput('listingsData'))
      ))
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
                          
                          selectInput(inputId = 'xcol',
                                      label = 'X Variable',
                                      choices = names(iris)),
                          
                          selectInput(inputId = 'ycol',
                                      label = 'Y Variable',
                                      choices = names(iris),
                                      selected = names(iris)[[2]]),
                          
                          sliderInput(inputId = 'clusters',
                                      label = 'Cluster count',
                                      value = 3,
                                      min = 1,
                                      max = 9),
                          
                          circle = TRUE, status = "danger",
                          icon = icon("gear"), width = "300px",
                          
                          tooltip = tooltipOptions(title = "Click to see inputs !")
                        )))))
                        
                      
                      # material_card(
                      #   title = "Pull Autotrader Listings",
                      #   
                      #   material_card(
                      #     title = "Filters",
                      #     sliderInput("milage", label = "Milage",
                      #                 min = 0, 
                      #                 max = 200000,
                      #                 value = c(20000, 60000)),
                      #     
                      #     sliderInput("price", label = "Price",
                      #                 min = 1000, 
                      #                 max = 100000,
                      #                 value = c(100, 25000)) 
                      #   ),
                      #   shinymaterial::material_modal(
                      #     modal_id = "runner",
                      #     button_text = "Run",
                      #     title = "Report Runner",
                      #     
                      #     material_number_box(
                      #       input_id = "max_results",
                      #       min_value = 1,
                      #       max_value = 2000,
                      #       initial_value = 1000, 
                      #       label = "Max Results"
                      #     ),
                      #     
                      #     material_button(
                      #       input_id = "run_report",
                      #       label = "Run Report"
                      #     )
                      #   )
                      # )
                      
                      
                      
                      # material_dropdown(
                      #   input_id = "report",
                      #   label = "Select a Group to Compare",
                      #   c(
                      #     "Jeep" = "jeep",
                      #     "Porsche" = "porsche"
                      #   ),
                      #   selected = c("jeep"),
                      #   multiple = TRUE,
                      #   color = "#ef5350"
                      # ),
                      
                      # material_column(width = 5,mainPanel())
    # ,
    # 
    # material_column(width = 10,
    #                 
    #                 tags$div(id = 'wholeApp', style = "opacity:0", title = "lmao",
    #                          
    #                          material_row(
    #                            material_column(width = 2)
    #                          ),
    #                          
    #                          material_row(
    #                            
    #                            material_column(width = 12,
    #                                            material_card(title = "lmao", depth = 3)
    #                            )
    #                          )
    #                 )
    # )
)