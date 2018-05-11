library(tidyverse)
library(rvest) # read_html functions and the like
library(httr)
library(readr)
library(leaps)
library(MASS)
library(grDevices) # This is for HEX color conversion
library(pbapply) # For progress bars in queries
library(parallel) # For parallel processing
library(reticulate) # This is for running selenium and clicking buttons
source("JeepScrapeFunc.R")
source("InfoExtractionFunc.R")
source("cleanResultFunc.R")
source("autoTrader_scrape.R")

full_df <- bind_rows(autoTrader_scrape(make = "jeep", 
                                       model = "wrangler", 
                                       zip = 84604, 
                                       pages = "all", 
                                       sellerType = "p", 
                                       fork = 7L,
                                       locationName = "Provo"),
                     
                     autoTrader_scrape(make = "jeep", 
                                       model = "wrangler", 
                                       zip = 84604, 
                                       pages = "all", 
                                       sellerType = "d", 
                                       fork = 7L,
                                       locationName = "Provo"),
                     
                     autoTrader_scrape(make = "jeep", 
                                       model = "wrangler", 
                                       zip = 88901, 
                                       pages = "all", 
                                       sellerType = "p", 
                                       fork = 7L,
                                       locationName = "Vegas"),
                     
                     autoTrader_scrape(make = "jeep", 
                                       model = "wrangler", 
                                       zip = 88901, 
                                       pages = "all", 
                                       sellerType = "d", 
                                       fork = 7L,
                                       locationName = "Vegas"),
                     
                     autoTrader_scrape(make = "jeep", 
                                       model = "wrangler", 
                                       zip = 83712, 
                                       pages = "all", 
                                       sellerType = "p", 
                                       fork = 7L,
                                       locationName = "Boise"),
                     
                     autoTrader_scrape(make = "jeep", 
                                       model = "wrangler", 
                                       zip = 83712, 
                                       pages = "all", 
                                       sellerType = "d", 
                                       fork = 7L,
                                       locationName = "Boise"),
                     
                     autoTrader_scrape(make = "jeep", 
                                       model = "wrangler", 
                                       zip = 80014, 
                                       pages = "all", 
                                       sellerType = "p", 
                                       fork = 7L,
                                       locationName = "Denver"),
                     
                     autoTrader_scrape(make = "jeep", 
                                       model = "wrangler", 
                                       zip = 80014, 
                                       pages = "all", 
                                       sellerType = "d", 
                                       fork = 7L,
                                       locationName = "Denver"),
                     
                     autoTrader_scrape(make = "jeep", 
                                       model = "wrangler", 
                                       zip = 82930, 
                                       pages = "all", 
                                       sellerType = "p", 
                                       fork = 7L,
                                       locationName = "Evanston"),
                     
                     autoTrader_scrape(make = "jeep", 
                                       model = "wrangler", 
                                       zip = 82930, 
                                       pages = "all", 
                                       sellerType = "d", 
                                       fork = 7L,
                                       locationName = "Evanston"))

full_df_clean <- full_df %>% 
  distinct(VIN, .keep_all = TRUE)

full_df_clean %>% 
  filter(!is.na(SellerType),
         Mileage < 200000,
         price < 50000,
         year > 2010,
         grepl("Rubicon", as.character(model))) %>% 
  ggplot(aes(x = Mileage, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", aes(col = SellerType)) + 
  geom_text(aes(label = rowNum), hjust=0, vjust=0)

t.test(DealersProvo$price, PrivateSellersProvo$price)

Provo %>% 
  filter(!is.na(SellerType),
         Mileage < 200000,
         price < 50000,
         year > 2010,
         grepl("Rubicon", as.character(model))) %>% 
  lm(price ~ Mileage + year + SellerType, data = .) %>% 
  summary()

Provo[1027,]$listing_url
ProvoTest <- Provo %>% 
  mutate(interior = paste(as.vector(col2rgb(x)), collapse = " ")) %>% 
  spread()

str_split_fixed(col2rgb(Provo$exterior), " ", 3)


col2rgb(Provo$exterior[1])[,1]


tibble(color = rownames(temp),
       value = temp[,1])
       
       
       
as_tibble(col2rgb(Provo$exterior[1]))

Provo$listing_url[1]

a <- col2rgb(Provo$exterior)
a[3]














POST("http://volcano.si.edu/search_eruption_results.cfm",
     body = list(bp = "", `eruption_category[]` = "", `country[]` = "", polygon = "",  cp = "1"),
     encode = "form") -> res
