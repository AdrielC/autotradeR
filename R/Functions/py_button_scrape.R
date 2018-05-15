library(reticulate)
use_python(python = "/Users/adriel/anaconda3/bin/python")

tryCatch(
  selenium <- import("selenium"),
  error = function(e){
    selenium <- reticulate::import_from_path(module = "selenium", path = "/Users/adriel/anaconda3/lib/python3.6/site-packages/")
  })

testURL <- "https://www.autotrader.com/cars-for-sale/vehicledetails.xhtml?listingId=484656454&zip=84604&referrer=%2Fcars-for-sale%2Fsearchresults.xhtml%3Fzip%3D84604%26startYear%3D1981%26sortBy%3Drelevance%26firstRecord%3D0%26endYear%3D2019%26modelCodeList%3DWRANGLER%26makeCodeList%3DJEEP%26searchRadius%3D25&startYear=1981&numRecords=25&firstRecord=0&endYear=2019&modelCodeList=WRANGLER&makeCodeList=JEEP&searchRadius=25"

secondTestURL <- "https://www.autotrader.com/cars-for-sale/vehicledetails.xhtml?listingId=481471704&zip=84604&referrer=%2Fcars-for-sale%2Fsearchresults.xhtml%3Fzip%3D84604%26startYear%3D1981%26sortBy%3Drelevance%26vehicleStyleCodes%3DHATCH%26incremental%3Dall%26firstRecord%3D0%26endYear%3D2019%26searchRadius%3D25%26driveGroup%3DAWD4WD&startYear=1981&numRecords=25&vehicleStyleCodes=HATCH&firstRecord=0&endYear=2019&searchRadius=25&makeCode1=SUB&modelCode1=IMPREZ&digitalRetail=true"

source_python("Python/ScrapeModelInfo.py", envir = parent.frame())

html_doc <- scrape_button(testURL) %>% 
  read_html() 

html_doc %>% 
  rvest::html_nodes(".padding-horizontal-lg") %>%
  .[[2]] %>% 
  rvest::html_nodes("table") %>% 
  rvest::html_table()

scrape_button(secondTestURL) %>% 
  read_lines()
