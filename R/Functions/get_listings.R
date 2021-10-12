library(tidyverse)
library(rvest) # read_html functions and the like
library(httr)
library(pbapply) # For progress bars in queries
library(parallel) # For parallel processing
library(purrr)
library(glue)
source("/Users/adriel/MAIN/Blog/autotradeR/R/Functions/scrape_utils.R")
source("/Users/adriel/MAIN/Blog/autotradeR/R/Functions/autoTrader_query.R")
source("/Users/adriel/MAIN/Blog/autotradeR/R/Functions/_base.R")


## Define function that will generate result urls from queries
setGeneric("listResultPages",
           function(source, query, ...) standardGeneric("listResultPages"))

setMethod("listResultPages",
          c("AutoTrader", "AutoQuery"),
          function(source, query, ...) {
            q_df <- as.data.frame(query)
            a <- rep(url_compose(as.data.frame(source)), nrow(q_df))
            q_df <- q_df %>% mutate(
              price = case_when(
                is.na(minPrice) && is.na(max_price) ~ "",
                is.na(minPrice) || (minPrice <= 0) ~ paste0("/cars-under-", maxPrice),
                is.na(maxPrice) ~ paste0("/cars-over-", minPrice),
                TRUE ~ paste0("/cars-between-", minPrice, "-and-", maxPrice)
              ),
              make = case_when(
                is.na(make) ~ "/all-cars",
                TRUE ~ paste0("/", make)
                ),
              model = case_when(
                is.na(model) ~ "",
                TRUE ~ paste0("/", model)
              ),
              path = paste0(path(a), make, model, price))
            
            for(param in names(q_df)[!names(q_df) %in% c("make", "model", "maxPrice", "minPrice", "price", "path")]) {
              a <- urltools::param_set(a, param, q_df[[param]])
            }
            path(a) <- q_df$path
            a <- urltools::param_set(a, "searchRadius", "0")
            a <- urltools::param_set(a, "isNewSearch", "false")
            print(a)
            as.list(unlist(map(a, paginate_search, ...)))
          })

read_listings <- function(x, ...) UseMethod("read_listings")

## Extract listing info from all results on a single page
read_listings.default <- function(x, ...) x %>%
  read_html() %>%
  xml2::xml_find_all("//script[contains(@data-cmp, 'lstgSchema')]") %>%
  lapply(function(x) tryCatch(XML::xmlTreeParse(x), error=function(e) NULL)) %>%
  (function(x) x[lapply(x, is_null) == FALSE]) %>%
  lapply(function(x) x$doc$children$script %>%
           XML::xmlValue() %>% 
           jsonlite::fromJSON() %>%
           as.data.frame) %>%
  reduce(bind_rows, .init = data.frame()) %>%
  as.tibble()

## Extract listing info from all results in all pages
read_listings.list <- function(x, fork=NULL) {
  if(is.null(fork)) {
    out <- lapply(x, read_listings)
  } else {
    message(paste0("Using ", fork, " core(s) to scrape links"))
    cl <- makeForkCluster(fork)
    out <- pblapply(x, read_listings, cl=cl) 
    stopCluster(cl)
    rm(cl)
  }
  
  ## Apply read_listings.default to each page in the list x
  out %>% reduce(bind_rows, .init = data.frame())
}

RESULTS_PER_PAGE <- 25L

get_pages <- function(max_results) as.integer(max_results / RESULTS_PER_PAGE) + ((max_results %% RESULTS_PER_PAGE) > 0)

get_filtered_listings <- function(max_results=1000L,
                                  fork=NULL,
                                  ...) listResultPages(AutoTrader(),
                                                       AutoQuery(...),
                                                       pages=get_pages(max_results),
                                                       numRecords=RESULTS_PER_PAGE) %>%
  read_listings(fork=fork) %>%
    distinct(vehicleIdentificationNumber, .keep_all=TRUE) %>%
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
           body.type=bodyType,
           SKU=sku)

test_scrape <- function(fork=NULL) {
  
  ## Generate urls for paginated search results
  query_urls <- listResultPages(AutoTrader(),
                                AutoQuery(
                                  make = "Jeep",
                                  model = "Wrangler",
                                  minPrice = 25000,
                                  maxMileage = 60000L,
                                  sellerType = c('p')),
                                pages=100,
                                numRecords=25)
  
  ## Extract info from each result on each page and collect into a single dataframe
  test_autotrader_listings <- read_listings(query_urls, fork=fork)
  
  show(count(test_autotrader_listings))
  
  ## Analyze results
  test_autotrader_listings_filtered <- test_autotrader_listings %>%
    distinct(vehicleIdentificationNumber, .keep_all=TRUE) %>%
    group_by(manufacturer.name) %>%
    mutate(model_count = n()) %>%
    filter(model_count > 5) %>%
    ungroup() %>% 
    mutate(is_private=str_detect(offers.seller.name, "(?!=Private)( (Owner|Seller).*)"),
           mileageFromOdometer.value = readr::parse_number(mileageFromOdometer.value))
  
  count(test_autotrader_listings_filtered)
  
  test_autotrader_listings_filtered %>%
    group_by(is_private) %>%
    count()
  
  ## Plot data
  test_autotrader_listings_filtered %>%
    mutate(mileageFromOdometer.value = readr::parse_number(mileageFromOdometer.value)) %>%
    ggplot(aes(y=log1p(offers.price),
               x=mileageFromOdometer.value, 
               color=is_private)) +
    geom_point() + 
    geom_smooth(method = "lm", alpha=0.2) +
    facet_wrap(.~manufacturer.name)
}

ROW_NAMES <<- c(
  "SKU",
  "listing.url",
  "name",
  "price",
  "price.valid.until",
  "brand",
  "model",
  "mileage",
  "manufacturer",
  "body.type",
  "is_used",
  "seller.region",
  "production.date",
  "drivewheel",
  "engine",
  "transmission",
  "color",
  "color.interior",
  "fuel.efficiency",
  "fuel.type"
)