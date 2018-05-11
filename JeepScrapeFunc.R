# App Description ---------------------------------------------------------
  ### This app will pull all* of the listings from autotrader.com given a URL resulting from a search
  ### on the site with any parameters. (e.g. https://www.autotrader.com/cars-for-sale/Jeep/Wrangler/Provo+UT-84604?zip=84604&startYear=1981&numRecords=25&sortBy=relevance&firstRecord=0&endYear=2019&modelCodeList=WRANGLER&makeCodeList=JEEP&searchRadius=50 )

# Function List -----------------------------------------------------------
  # autoTrader_query()
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
    # Input: string argument from autoTrader_query
    # Output: capitalized strings
  
  # extract_listing_data()
    # This function calls all of the information extraction functions in InfoExtractionFunc.R
      # and returns a long dataframe of all the information for one listing (represented as a row)
    # Input: listing_url
    # Output: one row tibble of all listing information

  # scraper_apply()
    # This function will apply the extract_listing_data() function to each listing_url in the
      # dataframe returned by query_URL_reader()
    # Input: dataframe with column containing listing_url
    # Output: full dataframe of all listing information


# autoTrader_query() ------------------------------------------------------

autoTrader_query <- function(make, model, zip, 
                             startYear=1981, numRecords=100, 
                             firstRecord = 0, endYear=2019, 
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
    if(!missing(minPrice)){
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
    possiblePages <- read_html(masterSearchURL) %>%
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

# simpleCap() -------------------------------------------------------------

simpleCap <- function(x)
{
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# listings_df() -----------------------------------------------------------

listings_df <- function(masterSearchURL)
{
  
  listings_html <- masterSearchURL %>% 
    read_html()
  
  noResult <- html_text(html_node(listings_html, ".text-normal"))
  
  if(!is.na(noResult)){ # noResult will be NA if there are results for the search
    if(noResult == "No results found."){
      stop("No results found. Please change your search parameters") 
    } else {
      message("Trying listings_df() again. Recursion boiiiiii")
      return(listings_df(masterSearchURL))
    }
  }
  
  if(!is.na(noResult)){
    message("Trying listings_df again. Recursion boiiiiii")
    return(listings_df(masterSearchURL))
  }
  
  listings_nodeset <- listings_html %>% 
    html_nodes(".col-sm-9 .text-md") %>%
    html_attrs() 
  
  listings_df <- listings_nodeset %>%
    tibble(
      listing_title = map(., "title") %>% 
        unlist()
      ,
      listing_distance_miles = if(
        length(listings_html %>%
          html_nodes(".text-sm strong")) == 0
      ){
        NA
      } else {
        listings_html %>%
          html_nodes(".text-sm strong") %>% 
          html_text() %>%
          map(~ str_split(string = .x, pattern = " ")[[1]][1]) %>% 
          unlist()
      }
      ,
      listing_url = map(., "href") %>% 
        unlist()
    ) %>%
    .[,-1]
  
  return(listings_df)
}

# query_URL_reader() ---------------------------------------------------------

query_URL_reader <- function(masterSearchURLs) UseMethod("query_URL_reader")

query_URL_reader.list <- function(masterSearchURLs)
{
  masterSearch_df <- masterSearchURLs %>% 
    map(listings_df)
  return(masterSearch_df)
}

query_URL_reader.character <- function(masterSearchURLs)
{
  masterSearch_df <- listings_df(masterSearchURLs)
  return(masterSearch_df)
}

# build_listing_URL() -----------------------------------------------------

build_listing_URL <- function(listing_url)
{
  listing_html <- listing_url %>% 
    paste("https://www.autotrader.com", ., sep = "") %>%
    read_html()
  return(listing_html)
}

# extract_listing_data() -------------------------------------------------------

extract_listing_data <- function(listing_url)
{
  sessionNode <- tryCatch(build_listing_URL(listing_url),
                          error = function(e){
                            warning(paste0("The following link is either broken or 403 error occurred: ", 
                                           listing_url, 
                                           "\nYou may be parsing too many pages at once."))
                            Sys.sleep(1)
                            
                            naTable <- tibble(
                              price                          = NA,
                              listingPriceRedu               = NA,
                              `Body Style`                   = NA,
                              `Drive Type`                   = NA,
                              Engine                         = NA,
                              Mileage                        = NA,
                              Transmission                   = NA,
                              `ATC Car ID`                   = NA,
                              Fuel                           = NA,
                              MPG                            = NA,
                              `Stock #`                      = NA,
                              VIN                            = NA,
                              carFeaturesText                = NA,
                              exterior                       = NA,
                              interior                       = NA,
                              sellerComment                  = NA,
                              `Basic Warranty`               = NA,
                              `Corrosion Warranty`           = NA,
                              `Drivetrain Warranty`          = NA,
                              `Roadside Assistance Warranty` = NA,
                              newListingIndicator            = NA,
                              seller                         = NA,
                              model                          = NA
                            )
                            
                            return(naTable)
                          })
  
  if(class(sessionNode)[1] == "xml_document"){
    
    price               <- priceExtract(sessionNode)
    priceRedu           <- priceReduExtract(sessionNode)
    carInfoTable        <- carInfoTableExtract(sessionNode)
    carFeatures         <- carFeaturesExtract(sessionNode)
    carColor            <- carColorExtract(sessionNode)
    sellerComment       <- sellerCommentExtract(sessionNode)
    warrantyInfo        <- warrantyInfoExtract(sessionNode)
    newListingIndicator <- newListingIndicatorExtract(sessionNode)
    seller              <- listingSellerExtract(sessionNode)
    model               <- modelExtract(sessionNode)
    
    rm(listing_url)
    rm(sessionNode)
    
    out <- c(price, priceRedu, carInfoTable, carFeatures, 
             carColor, sellerComment, warrantyInfo, 
             newListingIndicator, seller, model) %>% 
      bind_cols()
    
    return(out)
    
  } else {
    
    return(sessionNode)
    
    }
}

# scraper_apply() ---------------------------------------------------------

scraper_apply <- function(list, cl = NA)
{
  
  full_df1 <- dplyr::bind_rows(list)
  
  system.time(full_df2 <- pblapply(full_df1$listing_url, function(x){
    
    result <- extract_listing_data(x)
    
    if(is.na(result$`Body Style`)){ # This is run when the first scrape of a link doesnt work
      retryScrape <- extract_listing_data(x)
      warning("Retrying link!")
      return(retryScrape)
    } else {
      return(result)
    }
    
    if(is.na(retryScrape$`Body Style`)){ # This is run when the second scrape of a link doesnt work
      retryScrape <- extract_listing_data(x)
      warning("Retrying link for the second time!!!! Holy shiz")
      return(retryScrape)
    } else {
      return(result)
    }
    
    }, cl = cl))
  
  full_df_list <- dplyr::bind_cols(full_df1, dplyr::bind_rows(full_df2))
  return(full_df_list)
}
