# App Description ---------------------------------------------------------
  ### This app will pull all* of the listings from autotrader.com given a URL resulting from a search
  ### on the site with any parameters. (e.g. https://www.autotrader.com/cars-for-sale/Jeep/Wrangler/Provo+UT-84604?zip=84604&startYear=1981&numRecords=25&sortBy=relevance&firstRecord=0&endYear=2019&modelCodeList=WRANGLER&makeCodeList=JEEP&searchRadius=50 )

# Function List -----------------------------------------------------------
  # query_autoTrader()
    # Take in search arguments and make master search query
    # Input: 3 search arguments: 1) Make, 2) Model, 3) Zip
    # Output: Master search result xml_document
  
  # listing_df()
    # Read in master search URL and return urls and some additional info on all the listings on a search result page.
    # Input: Master search result URL
    # Output: Listing URL dataframe containing links and other info for all the car listings in the page 
  
  # query_URL_reader()
    # Apply listing_df() to one or many search URLs. Has two methods for both character string input and list input
    # Input: master search URL
    # Output: listing_url dataframe from listing_df() (These URLs are incomplete, need to be built out)
  
  # build_listing_URL()
    # Appends autotrader domain to the URLs from query_url_reader() and uses read_html to request the page
      # and return the page xml document
    # Input: partial car listing_url output from query_URL_reader
    # Output: xml document of individual car listing
  
  # simpleCap()
    # This capitalizes titles of make and model in the masterSearchURL
    # Input: string argument from query_autoTrader
    # Output: capitalized strings
  
  # extract_listing_data()
    # This function calls all of the information extraction functions in InfoExtractionFunc.R
      # and returns a long dataframe of all the information for one listing (represented as a row)
    # Input: listing_url
    # Output: one row tibble of all listing information


# query_autoTrader() ------------------------------------------------------

query_autoTrader <- function(make, model, zip, 
                             startYear=1981, numRecords=100, 
                             firstRecord = 0, endYear=2019, 
                             searchRadius=50,
                             minPrice, maxPrice, maxMileage,
                             pages, recursive)
  {
  
  possibleMilage <- c(0, seq(15000, 75000, by = 15000), 100000, 150000, 200000, 200001)
  masterSearchURLs <- list(NULL)
  
  masterSearchURL <- paste0(
    "https://www.autotrader.com/cars-for-sale/",
    simpleCap(make), "/",
    simpleCap(model), "/",
    "?zip=", zip,
    "&startYear=", startYear,
    "&numRecords=", numRecords,
    "&firstRecord=", firstRecord,
    "&endYear=", endYear,
    "&searchRadius=", searchRadius,
    sep = ""
    )
  
  if(!missing(minPrice)){
    masterSearchURL <- paste0(
      masterSearchURL,
      "&minPrice=", minPrice
      )
    }
    
    if(!missing(maxPrice)){
      masterSearchURL <- paste0(
        masterSearchURL,
        "&maxPrice=", maxPrice
      )
    }
    
    if(!missing(maxMileage)){
      maxMileage <- possibleMilage[findInterval(maxMileage, possibleMilage)]
      masterSearchURL <- paste0(
        masterSearchURL,
        "&maxMileage=", maxMileage
      )
    }
  
  if(missing(recursive)){ # This saves the function a TON of time if using recursion. This needs to be done only once
    possiblePages <- read_html(masterSearchURL) %>%
    rvest::html_node(".pull-right > span") %>% 
    rvest::html_text() %>% 
    str_split(pattern = " ") %>% 
    unlist() %>% 
    tail(n = 1) %>% 
    as.numeric()
  }
  
  if(!missing(pages)){
    
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
    masterSearchURLs[[page + 1]] <- query_autoTrader(make, model, zip, startYear, 
                                                     numRecords, firstRecord = (numRecords * page), 
                                                     endYear, searchRadius, minPrice, maxPrice, 
                                                     maxMileage, recursive = TRUE)
  }
  
  return(masterSearchURLs)
}

# simpleCap() -------------------------------------------------------------

simpleCap <- function(x){
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# listings_df() -----------------------------------------------------------

listings_df <- function(masterSearchURL){
  
  listings_html <- masterSearchURL %>% 
    read_html()
  
  noResult <- html_text(html_node(listings_html, ".text-normal"))
  
  if(!is.na(noResult)){
    stop("No results found. You should try changing your search parameters boii")
  }
  
  listings_nodeset <- listings_html %>% 
    html_nodes(".col-sm-9 .text-md") %>%
    html_attrs() 
  
  listings_df <- listings_nodeset %>%
    tibble(
      listing_title = map(., "title") %>% 
        unlist()
      ,
      listing_distance_miles = listings_html %>%
        html_nodes(".text-sm strong") %>%
        html_text() %>%
        map(~ str_split(string = .x, pattern = " ")[[1]][1]) %>% 
        unlist()
      ,
      listing_url = map(., "href") %>% 
        unlist()
      ,
      listing_dealer = listings_html %>%
        html_nodes(".col-sm-3 .list-unstyled.text-sm li:nth-child(2)") %>%
        html_text()
    ) %>%
    .[,-1]
  
  return(listings_df)
}

# query_URL_reader() ---------------------------------------------------------

query_URL_reader <- function(masterSearchURLs) UseMethod("query_URL_reader")

query_URL_reader.list <- function(masterSearchURLs){
  masterSearch_df <- masterSearchURLs %>% 
    map(listings_df)
  return(masterSearch_df)
}

query_URL_reader.character <- function(masterSearchURLs){
  masterSearch_df <- listings_df(masterSearchURLs)
  return(masterSearch_df)
}

# build_listing_URL() -----------------------------------------------------

build_listing_URL <- function(listing_url){
  listing_html <- listing_url %>% 
    paste("https://www.autotrader.com", ., sep = "") %>%
    read_html()
  return(listing_html)
}

# extract_listing_data() -------------------------------------------------------

extract_listing_data <- function(listing_url){
  sessionNode <- build_listing_URL(listing_url)
  rm(listing_url)
  
  price               <- priceExtract(sessionNode)
  priceRedu           <- priceReduExtract(sessionNode)
  carInfoTable        <- carInfoTableExtract(sessionNode)
  carFeatures         <- carFeaturesExtract(sessionNode)
  carColor            <- carColorExtract(sessionNode)
  sellerComment       <- sellerCommentExtract(sessionNode)
  warrantyInfo        <- warrantyInfoExtract(sessionNode)
  newListingIndicator <- newListingIndicatorExtract(sessionNode)
  
  out <- c(mget(ls())) %>% 
    bind_cols()
  
  return(out)
}

# scraper_apply() ---------------------------------------------------------

scraper_apply <- function(list){
  full_df1 <- dplyr::bind_rows(list)
  full_df2 <- lapply(full_df1$listing_url, extract_listing_data)
  full_df_list <- dplyr::bind_cols(full_df1, dplyr::bind_rows(full_df2))
  return(full_df_list)
}

