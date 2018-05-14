# Function List -----------------------------------------------------------
  # autoTrader_query()
    # Take in search arguments and make master search query
    # Input: 3 search arguments: 1) Make, 2) Model, 3) Zip
    # Output: Master search result xml_document

# autoTrader_query() ------------------------------------------------------

autoTrader_query <- function(make, model, zip, 
                             startYear=1981, numRecords=100, 
                             firstRecord = 0, endYear= 2019, # The following year
                             searchRadius = 0, sortBy = "relevance",
                             sellerTypes, minPrice, maxPrice, maxMileage,
                             pages, recursive = FALSE)
{
  possibleMilage <- c(0, seq(15000, 75000, by = 15000), 100000, 150000, 200000, 200001)
  masterSearchURLs <- list(NULL)
  
  if(numRecords < 25 && recursive == FALSE){
    warning("Minimum number of records per page is 25! Returning 25 listings.")
  }
  
  masterSearchURL <- paste0(
    "https://www.autotrader.com/cars-for-sale/",
    simpleCap(make), "/",
    simpleCap(model), "/",
    
    if(!missing(zip)){
      paste0(
        "?zip=", zip, "&"
      )
    } else {
      "?"      
    }
    ,
    "startYear=", startYear,
    "&numRecords=", numRecords,
    "&sortBy=", sortBy,
    "&firstRecord=", firstRecord,
    "&endYear=", endYear,
    "&searchRadius=", searchRadius
    ,
    if(!missing(minPrice)){
      paste0(
        "&minPrice=", minPrice
      )
    } else {
      ""
    }
    ,
    if(!missing(maxPrice)){
      paste0(
        "&maxPrice=", maxPrice
      )
    } else {
      ""
    }
    ,
    if(!missing(maxMileage)){
      maxMileage <- possibleMilage[findInterval(maxMileage, possibleMilage)]
      paste0(
        "&maxMileage=", maxMileage
      )
    } else {
      ""
    }
    ,
    if(!missing(sellerTypes)){
      paste0(
        "&sellerTypes=", sellerTypes
      )
    } else {
      ""
    }
    ,
    sep = ""
  )
  
  if(recursive == FALSE){ # This saves the function a TON of time if using recursion. This needs to be done only once
    possiblePages <- xml2::read_html(masterSearchURL) %>%
      rvest::html_node(".pull-right > span") %>% 
      rvest::html_text() %>% 
      str_split(pattern = " ") %>% 
      unlist() %>% 
      tail(n = 1) %>% 
      as.numeric()
  }
  
  if(!missing(pages) && !is.na(pages)){
    
    if(pages == "all" && !is.na(possiblePages)){
      range = 1:(possiblePages - 1)
    } else if(is.numeric(pages) && pages >= 2){
      range = 1:(pages - 1)
    } else {
      page <- 0
      range <- 0
      return(masterSearchURL)
      stop("One page search made")
    }
    
  } else {
    page <- 0
    range <- 0
    return(masterSearchURL)
    stop("One page search made")
  }
  
  masterSearchURLs[[1]] <- masterSearchURL
  
  for(page in range){
    masterSearchURLs[[page + 1]] <- autoTrader_query(make, model, zip, startYear, 
                                                     numRecords, firstRecord = (numRecords * page), 
                                                     endYear, searchRadius, sortBy, sellerTypes,
                                                     minPrice, maxPrice, maxMileage, pages = NA,
                                                     recursive = TRUE)
  }
  
  return(masterSearchURLs)
}
