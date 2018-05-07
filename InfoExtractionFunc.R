
# Info Extraction Functions -----------------------------------------------

# All these functions take in the input of a single car listing URL 
  # and return dataframes of specific information

### Data to Scrape:
  # Price
    # CSS selectors: selected using rvest::html_nodes()
    # Selector: ".text-lg strong"

  # Price Reduction indicator
    # CSS selectors: selected using rvest::html_nodes()
    # Selector: ".text-success"

  # Full Car Info Table (Table format)
    # CSS selectors: selected using rvest::html_nodes() (if multiple selectors, use two piped html_nodes() functions)
    # First Selector:  ".margin-vertical-md.row .col-xs-6"
    #                ".table
    # Second selector: ".margin-vertical-md.row .col-xs-6+ .col-xs-6"
    #                ".table"

  # Car Features (Not table format. Return 1 text object)
    # Selector: ".column-count-3 ul"

  # Color exterior and interior
    # Selector: ".swatch"

  # Seller Comments
    # Selector: ".see-more-content p"

  # Warranty Info
    # Selector: ".margin-vertical-sm td"

  # Newly Listed
    # Selector: ".text-md.text-orange"


# Testing URLs for info extract functions ------------------------------------------------------------
#
# testURL <- "https://www.autotrader.com/cars-for-sale/vehicledetails.xhtml?listingId=476939933&zip=84604&referrer=%2Fcars-for-sale%2Fsearchresults.xhtml%3Fzip%3D84604%26startYear%3D1981%26sortBy%3Drelevance%26incremental%3Dall%26firstRecord%3D0%26endYear%3D2019%26modelCodeList%3DWRANGLER%26makeCodeList%3DJEEP%26searchRadius%3D50&startYear=1981&numRecords=25&firstRecord=0&endYear=2019&modelCodeList=WRANGLER&makeCodeList=JEEP&searchRadius=50&makeCode1=JEEP&modelCode1=WRANGLER"
# This has no price
# testURL <- "https://www.autotrader.com/cars-for-sale/vehicledetails.xhtml?listingId=484138993&zip=84604&referrer=%2Fcars-for-sale%2Fsearchresults.xhtml%3Fzip%3D84604%26startYear%3D1981%26sortBy%3Drelevance%26incremental%3Dall%26firstRecord%3D650%26endYear%3D2019%26modelCodeList%3DWRANGLER%26makeCodeList%3DJEEP%26searchRadius%3D50&startYear=1981&numRecords=25&firstRecord=650&endYear=2019&modelCodeList=WRANGLER&makeCodeList=JEEP&searchRadius=50&makeCode1=JEEP&modelCode1=WRANGLER"
# Price Reduced
# testURL <- "https://www.autotrader.com/cars-for-sale/vehicledetails.xhtml?listingId=466254090&zip=84604&referrer=%2Fcars-for-sale%2Fsearchresults.xhtml%3Fzip%3D84604%26startYear%3D1981%26sortBy%3Drelevance%26incremental%3Dall%26firstRecord%3D350%26endYear%3D2019%26modelCodeList%3DWRANGLER%26makeCodeList%3DJEEP%26searchRadius%3D50&startYear=1981&numRecords=25&firstRecord=350&endYear=2019&modelCodeList=WRANGLER&makeCodeList=JEEP&searchRadius=50&makeCode1=JEEP&modelCode1=WRANGLER"
# NewlyListed
# testURL <- "https://www.autotrader.com/cars-for-sale/vehicledetails.xhtml?listingId=484138993&zip=84604&referrer=%2Fcars-for-sale%2Fsearchresults.xhtml%3Fzip%3D84604%26startYear%3D1981%26sortBy%3Drelevance%26incremental%3Dall%26firstRecord%3D650%26endYear%3D2019%26modelCodeList%3DWRANGLER%26makeCodeList%3DJEEP%26searchRadius%3D50&startYear=1981&numRecords=25&firstRecord=650&endYear=2019&modelCodeList=WRANGLER&makeCodeList=JEEP&searchRadius=50&makeCode1=JEEP&modelCode1=WRANGLER"
# 
# sessionNode <- read_html(testURL)

priceExtract <- function(sessionNode)
{
  listingPrice <- sessionNode %>% 
    rvest::html_node(".text-lg strong") %>% 
    rvest::html_text()
  
  listingPrice <- tryCatch(readr::parse_number(listingPrice),
                           error = function(e) return(NA))
  
  listingPriceTable <- tibble(
    price = listingPrice
  )
  
  return(listingPriceTable)
}

priceReduExtract <- function(sessionNode)
{
  listingPriceRedu <- sessionNode %>% 
    rvest::html_node(".text-success") %>% 
    rvest::html_text()
  
  if(is.na(listingPriceRedu)){
    listingPriceRedu <- FALSE
  } else {
    listingPriceRedu <- TRUE
  }
  
  listingPriceReduTable <- tibble(
    listingPriceRedu = listingPriceRedu
  )
  
  return(listingPriceReduTable)
}

carInfoTableExtract <- function(sessionNode)
{
  carInfoTable1 <- tryCatch(
    
    sessionNode %>% 
      rvest::html_node(".margin-vertical-md.row .col-xs-6") %>%
      rvest::html_node(".table") %>% 
      rvest::html_table() %>% 
      dplyr::as_tibble() %>% 
      spread(X1, X2),
    
    error = function(e){
      naTable1 <- tibble(
        `Body Style` = NA,
        `Drive Type` = NA,
        Engine       = NA,
        Mileage      = NA,
        Transmission = NA
      )
      return(naTable1)
    })
  
  carInfoTable2 <- tryCatch(
    
    sessionNode %>% 
      rvest::html_node(".margin-vertical-md.row .col-xs-6+ .col-xs-6") %>% 
      rvest::html_node(".table") %>% 
      rvest::html_table() %>% 
      dplyr::as_tibble() %>% 
      spread(X1, X2),
    
    error = function(e){
      naTable1 <- tibble(
        `ATC Car ID`  = NA,
        Fuel          = NA,
        MPG           = NA,
        `Stock #`     = NA,
        VIN           = NA
      )
      return(naTable1)
    })
  
  
  carInfoTable <- dplyr::bind_cols(carInfoTable1, carInfoTable2)
  
  return(carInfoTable)
}

carFeaturesExtract <- function(sessionNode)
{
  carFeaturesTextRaw <- tryCatch(
    
    sessionNode %>% 
      rvest::html_node(".column-count-3 ul") %>%
      rvest::html_text(),
    
    error = function(e) return(NA)
  )
  
  carFeaturesText <- tryCatch(
    tibble(
      carFeaturesText = carFeaturesTextRaw
    ),
    error = function(e){
      carFeaturesText <- tibble(
        carFeaturesText = NA
      )
    }) 
  
  return(carFeaturesText)
}

carColorExtract <- function(sessionNode)
{
  carColorTable <- tryCatch(
    sessionNode %>%
      rvest::html_nodes(".swatch") %>%
      rvest::html_attr("style") %>% 
      sub(".*:", "", .) %>% 
      tibble(
        exterior = .[1],
        interior = .[2]
      ) %>% 
      .[1,2:3],
    
    error = function(e){
      na.tableColor <- tibble(
        exterior = NA,
        interior = NA
      )
      
      return(na.tableColor)
    })
  
  return(carColorTable)
}

sellerCommentExtract <- function(sessionNode)
{
  sellerCommentText <- tryCatch(
    
    sessionNode %>% 
      rvest::html_node(".see-more-content p") %>% 
      rvest::html_text() %>% 
      tibble(
        sellerComment = .
      ),
    
    error = function(e){
      na.tableComments <- tibble(
        sellerComment = NA
      )
    }
  )
  
  return(sellerCommentText)
}

warrantyInfoExtract <- function(sessionNode)
{
  warrantyTable <- tibble(
    `Basic Warranty` = NA,
    `Corrosion Warranty` = NA,
    `Drivetrain Warranty` = NA,
    `Roadside Assistance Warranty` = NA
  )
  
  warrantyText <- tryCatch(sessionNode %>% 
                              rvest::html_nodes(".margin-vertical-sm td") %>%
                              lapply(html_text),
                            
                            error = function(e) return(NULL))
  
  tryCatch(
    for(i in 1:(length(warrantyText)/2)){
      warrantyTable[1,i] <- warrantyText[i*2]
    },
    error = function(e){
      return(warrantyTable)
    })
  
  warrantyInfoTable <- warrantyTable
  
  return(warrantyInfoTable)
}

newListingIndicatorExtract <- function(sessionNode)
{
  newListingIndicator <- sessionNode %>% 
    rvest::html_node(".text-md.text-orange") %>% 
    rvest::html_text()
  
  newListingIndicatorTable <- tibble(
    newListingIndicator = newListingIndicator
  )
  
  if(is.na(newListingIndicatorTable$newListingIndicator)){
    newListingIndicatorTable$newListingIndicator <- FALSE
  } else {
    newListingIndicatorTable$newListingIndicator <- TRUE
  }
  
  return(newListingIndicatorTable)
}
