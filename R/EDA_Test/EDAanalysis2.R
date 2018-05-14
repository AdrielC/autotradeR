library(tidyverse)
library(rvest) # read_html functions and the like
library(httr)
library(leaps)
library(MASS)
library(grDevices) # This is for HEX color conversion
library(pbapply) # For progress bars in queries
library(parallel) # For parallel processing
library(reticulate) # This is for running selenium and clicking buttons
source("autotrader_scrape_main.R")
source("scrape_utils.R")
source("data_clean.R")
source("info_extractors.R")

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
                                       locationName = "Evanston"),

                     autoTrader_scrape(make = "jeep",
                                       model = "wrangler",
                                       zip = 90001,
                                       pages = "all",
                                       sellerType = "p",
                                       fork = 7L,
                                       locationName = "LA"),

                     autoTrader_scrape(make = "jeep",
                                       model = "wrangler",
                                       zip = 90001,
                                       pages = "all",
                                       sellerType = "d",
                                       fork = 7L,
                                       locationName = "LA"),

                     autoTrader_scrape(make = "jeep",
                                       model = "wrangler",
                                       zip = 94061,
                                       pages = "all",
                                       sellerType = "p",
                                       fork = 7L,
                                       locationName = "SF"),

                     autoTrader_scrape(make = "jeep",
                                       model = "wrangler",
                                       zip = 94061,
                                       pages = "all",
                                       sellerType = "d",
                                       fork = 7L,
                                       locationName = "SF")
                     ) %>%
  distinct(VIN, .keep_all = TRUE)

autoTrader_scrape(make = "Jeep", model = "Wrangler", minPrice = 7000, zip = 84604,
                  write_csv = TRUE, fork = 7)

query <- autoTrader_query()


full_df_clean <- full_df %>%
  dplyr::select(-rowNum) %>%
  rowid_to_column("rowNum")

full_df_clean %>%
  filter(!is.na(sellerType),
         Mileage < 200000,
         price < 50000,
         year > 2010,
         grepl("Rubicon", as.character(model))) %>%
  ggplot(aes(x = Mileage, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", aes(col = sellerType)) +
  geom_text(aes(label = rowNum), hjust=0, vjust=0)

t.test(full_df_clean$price, full_df_clean$price)

full_df_clean %>%
  filter(!is.na(sellerType),
         Mileage < 200000,
         price < 70000,
         year > 2005
         ) %>%
  lm(price ~ Mileage + year + sellerType + newListingIndicator + ownershipStatus + listingPriceRedu, data = .) %>%
  summary()

outMod <- full_df_clean %>%
  lm(price ~ Mileage + year + sellerType + newListingIndicator + ownershipStatus + listingPriceRedu, data = ., na.action = na.exclude)

model_full <- cbind(full_df_clean, resid = resid(outMod), fitted = fitted(outMod)) %>%
  arrange(resid)

model_full %>%
  filter(grepl("Rubicon", as.character(model))) %>%
  .[1:10,] %>%
  .$listing_url

model_full %>%
  filter(grepl("Rubicon", as.character(model))) %>%
  .[1:10,] %>%
  .$resid



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
