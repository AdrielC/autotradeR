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
shinyServer(function(input, output) {
  
  rV <- reactiveValues()
 
  output$listingsData <- renderDataTable({
    # NUM_RECORDS <- 25L
    # PARALLEL_THREADS <- 32L
    
    df <- listResultPages(AutoTrader(),
                          AutoQuery(
                            minPrice = input$price[1],
                            maxPrice = input$price[2],
                            maxMileage = input$milage[2],
                            sellerType = input$seller_type),
                          pages=100,
                          numRecords=25) %>%
        read_listings(fork = PARALLEL_THREADS) %>%
        distinct(vehicleIdentificationNumber, .keep_all=TRUE) %>%
        group_by(manufacturer.name) %>%
        mutate(model_count = n()) %>%
        filter(model_count > 5) %>%
        ungroup() %>% 
        mutate(is_private=str_detect(offers.seller.name, "(?!=Private)( (Owner|Seller).*)"),
               mileageFromOdometer.value = readr::parse_number(mileageFromOdometer.value),
               is_used=factor(str_detect(offers.itemCondition, "Used"))) %>%
        rename(VIN=vehicleIdentificationNumber,
               price=offers.price,
               price.valid.until=offers.priceValidUntil,
               seller.region=offers.seller.address.addressRegion,
               seller.zip=offers.seller.address.postalCode,
               brand=brand.name,
               manufacturer=manufacturer.name,
               production.date=productionDate,
               drivewheel=driveWheelConfiguration,
               engine=vehicleEngine,
               transmission=vehicleTransmission,
               mileage=mileageFromOdometer.value,
               listing.url=url,
               color.interior=vehicleInteriorColor,
               fuel.efficiency=fuelEfficiency,
               fuel.type=fuelType,
               body.type=bodyType) %>%
        select(VIN,
               sku,
               listing.url,
               name,
               price,
               price.valid.until,
               brand,
               model,
               mileage,
               manufacturer,
               body.type,
               is_used,
               seller.region,
               production.date,
               drivewheel,
               engine,
               transmission,
               color,
               color.interior,
               fuel.efficiency,
               fuel.type)
      print(df)
      df
    })
})
