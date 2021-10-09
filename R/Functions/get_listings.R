library(tidyverse)
library(rvest) # read_html functions and the like
library(httr)
library(pbapply) # For progress bars in queries
library(parallel) # For parallel processing
library(purrr)
library(glue)
source("R/Functions/scrape_utils.R")
source("R/Functions/autoTrader_query.R")
source("R/Functions/_base.R")


## Define function that will generate result urls from queries
setGeneric("listResultPages",
           function(source, query, ...) standardGeneric("listResultPages"))

setMethod("listResultPages",
          c("AutoTrader", "AutoQuery"),
          function(source, query, ...) {
  q_df <- as.data.frame(query)
  a <- rep(url_compose(as.data.frame(source)), nrow(q_df))
  
  for(param in names(q_df)[!names(q_df) %in% c("make", "model")]) {
    a <- urltools::param_set(a, param, q_df[[param]])
  }
  f <- function(x, empty="", suffix="", prefix="") ifelse(is.na(x), empty, paste0(prefix, x, suffix))
  makes <- map(q_df$make, f, empty="/all-cars", prefix="/")
  models <- map(q_df$model, f)
  paths <- q_df %>% mutate(path=paste0(path(a), makes, models)) %>% .$path
  path(a) <- paths
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
  as.tibble

## Extract listing info from all results in all pages
read_listings.list <- function(x, fork=NULL) {
  if(is.null(fork)) {
    fork <- parallel::detectCores() - 1
  }
  message(paste0("Using ", fork, " core(s) to scrape links"))
  cl <- makeForkCluster(fork)
  ## Apply read_listings.default to each page in the list x
  pblapply(x, read_listings, cl=cl) %>% 
    ## combine all dataframes into one
    reduce(bind_rows, .init = data.frame())
}

## Generate urls for paginated search results
query_urls <- listResultPages(AutoTrader(),
                              AutoQuery(minPrice = 100),
                              pages=100,
                              numRecords=25)

## Extract info from each result on each page and collect into a single dataframe
listings <- read_listings(query_urls, fork = 32)

show(count(listings))

## Analyze results
listings_filtered <- listings %>%
  distinct(vehicleIdentificationNumber, .keep_all=TRUE) %>%
  group_by(manufacturer.name) %>%
  mutate(model_count = n()) %>%
  filter(model_count > 5) %>%
  ungroup() %>% 
  mutate(is_private=str_detect(offers.seller.name, "(?!=Private)( (Owner|Seller).*)"),
         mileageFromOdometer.value = readr::parse_number(mileageFromOdometer.value))

count(listings_filtered)

listings_filtered %>%
  group_by(is_private) %>%
  count()

## Plot data
listings_filtered %>%
  mutate(mileageFromOdometer.value = readr::parse_number(mileageFromOdometer.value)) %>%
  ggplot(aes(y=log1p(offers.price),
             x=mileageFromOdometer.value, 
             color=is_private)) +
  geom_point() + 
  geom_smooth(method = "lm", alpha=0.2) +
  facet_wrap(.~manufacturer.name)

