library(purrr)
library(glue)
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
           minPrice="integer",
           maxPrice="integer",
           maxMileage="character"))

setMethod("as.data.frame", "AutoQuery",
          function(x, row.names, optional) data.frame(
            row.names = row.names,
            make = x@make,
            model = x@model,
            zip = x@zip,
            startYear = x@startYear,
            endYear = x@endYear,
            sellerType = x@sellerType,
            minPrice = x@minPrice,
            maxPrice = x@maxPrice,
            maxMileage = x@maxMileage))

setMethod("as.list", "AutoQuery",
          function (x, ...) {
  df <- as.data.frame(x)
  l <- list(...)
  for(name in names(df)) {
    l[[name]] <- df[[name]]
  }
  l
})


# Validator to ensure that each slot is of length one
setValidity("AutoQuery", function(object) {
  invalids <- c()
  
  df <- tryCatch(as.data.frame(object), error=function(e){ 
    invalids <- ifelse(length(invalids) == 0, c(e), c(invalids, e))
  })
  
  if(length(df) == 0) {
    e <- glue("Fields must not be of length 0")
    invalids <- ifelse(length(invalids) == 0, c(e), c(invalids, e))
  }
  
  if (length(invalids)) return(invalids)
  
  TRUE
})

# Helper to create instances of the auto.query class
AutoQuery <- function(make = NA_character_,
                      model = NA_character_,
                      zip = NA_integer_,
                      startYear = NA_integer_,
                      endYear = NA_integer_,
                      sellerType = NA_character_,
                      minPrice = NA_integer_,
                      maxPrice = NA_integer_,
                      maxMileage = NA_character_) {
  fields <- list(
    make=make,
    model=model,
    zip=zip,
    startYear=startYear,
    endYear=endYear,
    sellerType=sellerType, 
    minPrice=minPrice,
    maxPrice=maxPrice, 
    maxMileage=maxMileage
  )
  
  lens <- map(fields, length)
  max_len <- max(unlist(lens))
  
  for (name in names(fields)) {
    value <- fields[[name]]
    val_len <- length(value)
    if(val_len == 1 && max_len != val_len) {
      fields[[name]] <- rep(value, max_len)
    }
  }
  
  new("AutoQuery",
      make = fields$make,
      model = fields$model,
      zip = fields$zip,
      startYear = fields$startYear,
      endYear = fields$endYear,
      sellerType = fields$sellerType,
      minPrice = fields$minPrice,
      maxPrice = fields$maxPrice,
      maxMileage = fields$maxMileage
  )
}



## Base class for sources of auto data e.g. Autotrader, Edmunds, etc.
setClass("AutoSource",
         slots = c(
           scheme = "character",
           server = "character",
           path = "character"))

setMethod("as.data.frame", "AutoSource",
          function(x, row.names, optional) {
            df <- urltools::url_parse(glue("{x@scheme}://{x@server}/{x@path}"))
            df.row.names <- row.names
            df
          })


setMethod("as.list", "AutoSource",
          function (x, ...) {
            df <- as.data.frame(x)
            l <- list(...)
            for(name in names(df)) {
              l[[name]] <- df[[name]]
            }
            l
          })

setMethod("show", "AutoSource",
          function(object) {
            cat(is(object)[[1]], "\n",
                "  Scheme: ", object@scheme, "\n",
                "  Server: ", object@server, "\n",
                "  Path: ", object@path, "\n",
                sep = "")})



AutoTrader <- setClass(
  "AutoTrader",
  contains = "AutoSource",
  prototype = prototype(
    scheme = "https",
    server = "www.autotrader.com",
    path = "cars-for-sale"))

