library(rgl) # for 3d plotting
library(stringr) # Number of characters and other string manip
library(tidyverse)
library(rvest) # read_html functions and the like
library(httr)
library(leaps) # For model selection
library(MASS)
library(grDevices) # This is for HEX color conversion
library(pbapply) # For progress bars in queries
library(parallel) # For parallel processing
library(reticulate) # This is for running selenium and clicking buttons
library(V8) # for executing javascript
library(XML)
library(purrr)
library(glue)
source("R/Functions/autoTrader_scrape.R")
source("R/Functions/autoTrader_query.R")
source("R/Functions/data_clean.R")
source("R/Functions/info_extractors.R")
source("R/Functions/scrape_utils.R")

library(urltools)


# https://www.autotrader.com/cars-for-sale/searchresults.xhtml?zip=78741&listingTypes=used&makeCodeList=JEEP&modelCodeList=WRANGLER

# build_search_url <- function(x) UseMethod("build_search_url")
# build_search_url.autotrader <- function(make, model,) "autotrader"


setClass("AutoQuery",
         slots = c(
           make = "character",
           model = "character",
           zip = "integer",
           startYear="integer",
           endYear="integer",
           sellerType = "character",
           minPrice="numeric",
           maxPrice="numeric",
           maxMileage="integer"),
         prototype = prototype(
           make = NA_character_,
           model = NA_character_,
           zip = NA_integer_,
           startYear = NA_integer_,
           endYear = NA_integer_,
           sellerType = NA_character_,
           minPrice = NA_real_,
           maxPrice = NA_real_,
           maxMileage = NA_integer_))

# Helper to create instances of the auto.query class
AutoQuery <- function(make = NA_character_,
                      model = NA_character_,
                      zip = NA_integer_,
                      startYear = NA_integer_,
                      endYear = NA_integer_,
                      sellerType = NA_character_,
                      minPrice = NA_real_,
                      maxPrice = NA_real_,
                      maxMileage = NA_integer_) {
  minPrice <- as.double(minPrice)
  maxPrice <- as.double(maxPrice)
  make <- ifelse(!is.na(make) && make == "", NA_character_, make)
  model <- ifelse(!is.na(model) && model == "", NA_character_, model)
  new("AutoQuery",
      make = make,
      model = model,
      zip = zip,
      startYear = startYear,
      endYear = endYear,
      sellerType = sellerType,
      minPrice = as.double(minPrice),
      maxPrice = as.double(maxPrice),
      maxMileage = maxMileage
  )
}

setMethod("as.data.frame", "AutoQuery", function(x, row.names, optional) data.frame(
    make = x@make,
    model = x@model,
    zip = x@zip,
    startYear = x@startYear,
    endYear = x@endYear,
    sellerType = x@sellerType,
    minPrice = x@minPrice,
    maxPrice = x@maxPrice,
    maxMileage = x@maxMileage))

q <- AutoQuery()

as.data.frame(q)


# Validator to ensure that each slot is of length one
setValidity("AutoQuery", function(object) {
  invalids <- c()
  if (length(object@make) != 1 ||
      length(object@model) != 1 ||
      length(object@zip) != 1 ||
      length(object@startYear) != 1 ||
      length(object@endYear) != 1 ||
      length(object@sellerType) != 1 || 
      length(object@minPrice) != 1 || 
      length(object@maxPrice) != 1 || 
      length(object@maxMileage) != 1) {
    invalids <- "all slots must be of length 1"
  }
  
  if (length(invalids)) return(invalids)
  TRUE
})



setClass("AutoSource",
         slots = c(
           scheme = "character",
           domain = "character",
           path = "character"
         ))

setMethod("show", "AutoSource", function(object) {
  cat(is(object)[[1]], "\n",
      "  Scheme: ", object@scheme, "\n",
      "  Domain: ", object@domain, "\n",
      "  Path: ", object@path, "\n",
      sep = ""
  )
})

setMethod("as.data.frame", "AutoSource", function(x, row.names, optional) {
  url_parse(glue("{x@scheme}://{x@domain}/{x@path}"))
})

setMethod("url_compose", "AutoSource", function(parsed_urls) {
  print(parsed_urls)
  url_compose(as.data.frame(parsed_urls))
})

AutoTrader <- setClass("AutoTrader",
                       contains = "AutoSource",
                       prototype = prototype(
                         scheme = "https",
                         domain = "www.autotrader.com",
                         path = "cars-for-sale"))



a <- AutoTrader()
a

show(a)
validObject(a)

url_compose(as.data.frame(a))

url_compose(a)

query <- autoTrader_query(make = "jeep", pages = 30)

read_listings <- function(x) UseMethod("read_listings")

read_listings.list <- function(x) {
  purrr::map(x, read_listings) %>%
    reduce(bind_rows, .init = data.frame())
}

read_listings.default <- function(x) x %>%
  read_html() %>%
  html_nodes(".col-xs-12.col-md-9") %>%
  xml2::xml_find_all("//script[contains(@data-cmp, 'lstgSchema')]") %>%
  lapply(function(x) tryCatch(XML::xmlTreeParse(x), 
                              error=function(e) message("Error in parsing tree", e))) %>%
  (function(x) x[lapply(x, is_null) == FALSE]) %>%
  lapply(function(x) x$doc$children$script %>%
           XML::xmlValue() %>% 
           jsonlite::fromJSON() %>%
           as.data.frame) %>%
  reduce(bind_rows, .init = data.frame()) %>%
  as.tibble

listings <- read_listings(query)

listings$model

listings %>%
  dplyr::distinct(vehicleIdentificationNumber, .keep_all=TRUE) %>%
  dplyr::mutate(mileageFromOdometer.value = readr::parse_number(mileageFromOdometer.value)) %>%
  ggplot(aes(y=log1p(offers.price), x=log1p(mileageFromOdometer.value))) + 
  geom_point() + 
  geom_smooth(method = "lm", alpha=0.2) +
  facet_wrap(.~model)
  
