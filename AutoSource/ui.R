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
library(rintrojs)
library(shinyBS)

source("/Users/adriel/MAIN/Blog/autotradeR/R/global.R")
source("/Users/adriel/MAIN/Blog/autotradeR/R/Functions/get_listings.R")

##########################################
####   Shiny ui                       ####
##########################################
library(plotly)
library(shinythemes)
library(DT)
library(rsconnect)
# ------------------
# Main title section
# ------------------


ui <- navbarPage(
  div(
    img(src = "EZTrader.png", width = "25%", class="bg"),
    "EZTrader LLC"
  ),
  theme = shinytheme("flatly"),
  
  
  tabPanel("Listings",
            # App title ----
           titlePanel(div(
             "Listings Scraper"
           )),


           tags$br(),

           tabsetPanel(
             type = "tabs",
             tabPanel(
               "AutoTrader",
               sidebarLayout(
                 sidebarPanel(
                   h3("Filters"),
                   tags$br(),
                   
                   selectInput('make', 'Make',
                               choices = makes$label,
                               multiple=TRUE, selectize = TRUE),
                   
                   selectInput('make', 'Make',
                               choices = makes$label,
                               multiple=TRUE, selectize = TRUE),
                                 
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
                   
                   selectInput("seller_type", "Seller Type",
                               choices = c(
                                 'private',
                                 'dealer'
                               ),
                               selected = 'p',
                               multiple = TRUE),
                   
                   
                   numericInput("max_results", "Max Results",
                                min = 25L,
                                max = 3000L,
                                value = 1000L,
                                step = 100L,
                                width = "100px"),
                   
                   actionButton("refresh","Scrape",
                                icon = icon("fa fa-refresh"),
                                class = "btn btn-warning")
                   ),

                 mainPanel(
                   tabsetPanel(
                     type = "tabs"
                   ),
                   tags$br(),
                   tags$br()
                 )
               ),
               tags$hr(),


               sidebarLayout(
                 sidebarPanel(
                   # ------------------
                   # Data overview filters
                   # ------------------

                   # h3("Data Overview"),
                   # tags$br(),
                   # setSliderColor(c("#2c3e50 ", "#2c3e50"), c(1, 2)),
                   # sliderInput(
                   #   "incomeRange",
                   #   label = "Salary Range",
                   #   min = 1600,
                   #   max = 5000,
                   #   value = c(1600, 5000)
                   # ),
                   # # setSliderColor(c("e67e22 ", "#e67e22"), c(1, 2)),
                   # sliderInput(
                   #   "employRange",
                   #   label = "Employment Rate Range",
                   #   min = 0,
                   #   max = 100,
                   #   value = c(0, 100)
                   # ),
                   # selectInput(
                   #   "checkYearGroup",
                   #   "Select Year",
                   #   choices = data$year,
                   #   selected = "2018",
                   #   multiple = TRUE
                   # ),
                 ),
                 mainPanel(
                   h3("Browse All"),
                   tags$br(),
                   dataTableOutput("listingsData"),
                   tags$br(),
                   tags$br()
                 )
               ),
               tags$hr()
           )
           )),

           # material_column(width = 9,
           #                 material_card("",
           #                               sidebarLayout(
           #                                 sidebarPanel(
           #                                   material_card("Filters",
           #                                                 
                                                           # material_card("",
                                                           #               selectInput('make', 'Make',
                                                           #                           choices = makes$label,
                                                           #                           multiple=TRUE, selectize = TRUE),
                                                           # 
                                                           #               material_dropdown("milage", "Milage",
                                                           #                                 choices = c(
                                                           #                                   "Any" = "0",
                                                           #                                   "Under 15,000" = "15000",
                                                           #                                   "Under 30,000" = "30000",
                                                           #                                   "Under 45,000" = "45000",
                                                           #                                   "Under 60,000" = "60000",
                                                           #                                   "Under 75,000" = "75000",
                                                           #                                   "Under 100,000" = "100000",
                                                           #                                   "Under 150,000" = "150000",
                                                           #                                   "Under 200,000" = "200000",
                                                           #                                   "Over 200,000" = "200001"
                                                           #                                 ),
                                                           #                                 selected = "75000"),
                                                           # 
                                                           #               sliderInput("price", "Price",
                                                           #                           min = 0L,
                                                           #                           max = 100000L,
                                                           #                           value = c(0L, 25000L))
                                                           # ),
                                                           # 
                                                           # selectInput("seller_type", "Seller Type",
                                                           #             choices = c(
                                                           #               'private',
                                                           #               'dealer'
                                                           #             ),
                                                           #             selected = 'p',
                                                           #             multiple = TRUE),
                                                           # 
                                                           # 
                                                           # numericInput("max_results", "Max Results",
                                                           #              min = 25L,
                                                           #              max = 3000L,
                                                           #              value = 1000L,
                                                           #              step = 100L,
                                                           #              width = "100px"),
                                                           # 
                                                           # material_button("refresh","Scrape",
                                                           #                 icon = icon("fa fa-refresh"))
           #                                                 
           #                                                 
           #                                                 
           #                                                 
           #                                   ),
           #                                   tags$br(),
           #                                   tags$br(),
           #                                   material_card("Select and Download",
           #                                                 selectInput('select_cols', 'Select Columns',
           #                                                             choices = ROW_NAMES,
           #                                                             selected = c(
           #                                                               "listing.url",
           #                                                               "name",
           #                                                               "price",
           #                                                               "mileage",
           #                                                               "production.date",
           #                                                               "seller.region"
           #                                                             ),
           #                                                             multiple=TRUE),
           #                                                 
           #                                                 downloadButton("downloadData", "Download")
           #                                   )
           #                                 ),
           #                                 mainPanel(
           #                                   tags$br(),
           #                                   tags$br(),
           #                                   tabsetPanel(
           #                                     type = "tabs",
           #                                     tabPanel("Scraped Listings", dataTableOutput('listingsData')),
           #                                     tabPanel("Detail", dataTableOutput('detailTable')),
           #                                     tabPanel("No. of Graduates", plotOutput(outputId = "piePlot"))
           #                                   ),
           #                                   tags$br(),
           #                                   tags$br()
           #                                 )
           #                               )
           #                 )
           # )

  
  
  
  ##### Side Nav Content --------------------------------------------------------
  
  # Analyzer -----------------------------------------------------------
  
  # Scheduler -----------------------------------------------------------
  # material_side_nav_tab_content(
  #   
  #   side_nav_tab_id = "analyzer",
  #   
  #   material_row(
  #     # Inputs
  #     material_column(width = 6,
  #                     fluidPage(
  #                       tags$h2("Dropdown Button"),
  #                       br(),
  #                       dropdownButton(
  #                         tags$h3("List of Inputs"),
  #                         circle = TRUE, status = "danger",
  #                         icon = icon("gear"), width = "300px",
  #                         tooltip = tooltipOptions(title = "Click to see inputs !")
  #                         )
  #                       )
  #                     )
  #     )
  #   )
  
  ################################################
  #### Panel: Documentation                   ####
  ################################################
  
  tabPanel("Visualizer",
           fluidPage(htmlOutput("doc")))
  
)