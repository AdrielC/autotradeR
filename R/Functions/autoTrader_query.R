# Function List -----------------------------------------------------------
  # autoTrader_query()
    # Take in search arguments and make master search query
    # Input: 3 search arguments: 1) Make, 2) Model, 3) Zip
    # Output: Master search result xml_document



paginate_search <- function(masterSearchURL,
                            numRecords=25,
                            firstRecord = 0,
                            recursive = FALSE,
                            pages)
{
  
  if(numRecords < 25 && recursive == FALSE){
    warning("Minimum number of records per page is 25! Returning 25 listings.")
  }
  
  masterSearchURL <-
    param_set(masterSearchURL, "firstRecord", firstRecord) %>%
    param_set("numRecords", numRecords)
  
  possibleMilage <- c(0, seq(15000, 75000, by = 15000), 100000, 150000, 200000, 200001)
  masterSearchURLs <- list()
  
  if(numRecords < 25 && recursive == FALSE){
    warning("Minimum number of records per page is 25! Returning 25 listings.")
  }
  
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
    masterSearchURLs[[page + 1]] <- paginate_search(masterSearchURL,
                                                    numRecords,
                                                    firstRecord = (numRecords * page), 
                                                    pages = NA,
                                                    recursive = TRUE)
  }
  
  return(masterSearchURLs)
}

# autoTrader_query() ------------------------------------------------------

autoTrader_query <- function(make=NA_character_,
                             model=NA_character_,
                             zip=NA_integer_,
                             startYear=1981,
                             numRecords=25,
                             firstRecord = 0,
                             endYear= 2022, # The following year
                             searchRadius = 0, # 0 indicates Inf radius
                             sortBy = "relevance",
                             sellerTypes = c('p', 'd'),
                             minPrice,
                             maxPrice,
                             maxMileage,
                             pages,
                             recursive = FALSE)
{
  possibleMilage <- c(0, seq(15000, 75000, by = 15000), 100000, 150000, 200000, 200001)
  masterSearchURLs <- list()
  
  path <- if(is.na(make) && is.na(model)) {
    "/all-cars"
  } else {
    paste0(
      ifelse(is.na(make), "", paste0("/", simpleCap(make))),
      ifelse(is.na(model), "", paste0("/", simpleCap(model)))
    )
  }
  
  masterSearchURL <- paste0(
    
    "https://www.autotrader.com/cars-for-sale",
    
    path,
    
    if(!missing(zip) && !is.na(zip)){
      paste0(
        "?zip=", zip, "&"
      )
    } else {
      "?"      
    }
    ,
    "startYear=", startYear,
    "&sortBy=", sortBy,
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
        "&sellerTypes=", reduce(sellerTypes, function(a, b) paste(a, b, sep = ","))
      )
    } else {
      ""
    }
    ,
    sep = ""
  )
  paginate_search(masterSearchURL,
                  pages=pages,
                  numRecords=numRecords,
                  firstRecord=firstRecord,
                  recursive=FALSE)
}

