library(tidyverse)
library(rvest) # read_html functions and the like
library(httr)
library(pbapply) # For progress bars in queries
library(parallel) # For parallel processing
library(pbapply)
library(reticulate)

listing_url <- "https://www.autotrader.com/cars-for-sale/vehicledetails.xhtml?listingId=484656454&zip=84604&referrer=%2Fcars-for-sale%2Fsearchresults.xhtml%3Fzip%3D84604%26startYear%3D1981%26sortBy%3Drelevance%26firstRecord%3D0%26endYear%3D2019%26modelCodeList%3DWRANGLER%26makeCodeList%3DJEEP%26searchRadius%3D25&startYear=1981&numRecords=25&firstRecord=0&endYear=2019&modelCodeList=WRANGLER&makeCodeList=JEEP&searchRadius=25"

listing_url <- "https://www.autotrader.com/cars-for-sale/vehicledetails.xhtml?listingId=481471704&zip=84604&referrer=%2Fcars-for-sale%2Fsearchresults.xhtml%3Fzip%3D84604%26startYear%3D1981%26sortBy%3Drelevance%26vehicleStyleCodes%3DHATCH%26incremental%3Dall%26firstRecord%3D0%26endYear%3D2019%26searchRadius%3D25%26driveGroup%3DAWD4WD&startYear=1981&numRecords=25&vehicleStyleCodes=HATCH&firstRecord=0&endYear=2019&searchRadius=25&makeCode1=SUB&modelCode1=IMPREZ&digitalRetail=true"

print(getwd())

# py_button_scrape --------------------------------------------------------

py_button_scrape <- function(listing_url, pythonpath, modulepath)
{
  
  print(modulepath)
  if(!missing(pythonpath)){
    use_python(python = pythonpath) 
  }
  
  if(!exists("selenium")){
    tryCatch(
      selenium <<- import("selenium"),
      error = function(e, modulepath = modulepath){
        if(!missing(modulepath)){
          print("oklmao")
          selenium <<- reticulate::import_from_path(module = "selenium", path = modulepath) 
        } else { 
          stop("no selenium module found in current module path")
        }
      })
  }
  
  if(!exists("AutoTraderSession")){
    source_python("Python/ScrapeModelInfo.py", envir = parent.frame())
  }
  
  html_doc <- scrape_button(listing_url) %>% 
    read_html()
  
  model_tables <- html_doc %>% 
    rvest::html_nodes(".padding-horizontal-lg") %>%
    .[[2]] %>%
    rvest::html_nodes("table") %>%
    .[1:2] %>% # This selects the number of table-halves in the "About This Model" tab. Most cases only 2 halves are needed/important.
    lapply(rvest::html_table) %>% 
    lapply(X = ., FUN = function(x) first_row_colnames(x, transpose = TRUE)) %>% 
    bind_cols()
  
  return(model_tables)
  
}

first_row_colnames <- function(df, transpose = FALSE)
{
  if(transpose == TRUE){
    df <- t(df)
  }
  
  colnames(df) <- df[1,]
  df <- tibble::as_tibble(df)
  df <- df[-1,]
  
  return(df)
}

py_button_scrape(listing_url, modulepath="/home/acasellas/anaconda3/envs/autotradeRPy35/lib/python3.5/site-packages")
