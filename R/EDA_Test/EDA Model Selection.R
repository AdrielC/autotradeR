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

source("R/Functions/autoTrader_scrape.R")
source("R/Functions/autoTrader_query.R")
source("R/Functions/data_clean.R")
source("R/Functions/info_extractors.R")
source("R/Functions/scrape_utils.R")


all_jeeps <- read_csv("Data/full_scrape_test1.csv") %>%
  bind_rows(read_csv("Data/full_scrape_test2.csv")) %>%
  distinct(`ATC Car ID`, .keep_all = TRUE) %>% 
  mutate(doors = ifelse(grepl("unlimited", listing_title, ignore.case = TRUE), yes = 4, no = 2)) %>% 
  mutate(lifted = ifelse(grepl("lift", carFeaturesText, ignore.case = TRUE), yes = TRUE, no = FALSE), 
         numWordsFeatures = lengths(strsplit(carFeaturesText, "\\W+")),
         modelGeneral = case_when(grepl("Rubicon", listing_title, ignore.case = TRUE)  ~ "Rubicon",
                                  grepl("Sport", listing_title, ignore.case = TRUE)    ~ "Sport",
                                  grepl("Sahara", listing_title, ignore.case = TRUE)   ~ "Sahara",
                                  grepl("Willy", listing_title, ignore.case = TRUE)    ~ "Willys",
                                  grepl("X", listing_title, ignore.case = TRUE)        ~ "X",
                                  TRUE                                                 ~ "other"))
  

all_jeeps <- clean_result_df(all_jeeps)


# kmeans on RGB data ------------------------------------------------------

#all_jeeps %>%
#  dplyr::select(price, Mileage, year) %>%
#  filter(price < 60000,
#         Mileage < 100000,
#         year > 2000) %>%
#  .[complete.cases(.), ] %>%
#  elbow_plot(K = 7)

# Looks like 2 clusters may work best here

#out_hc <- all_jeeps %>%
#  dplyr::select(price, Mileage, year) %>%
#  dist() %>%
#  hclust(method = "complete")

# Plot the dendrogram.
#plot(out_hc)

# Plotting colors
colors <- all_jeeps %>%
  distinct(exterior, .keep_all = TRUE) %>%
  dplyr::select(red:blue, exterior) %>%
  mutate(hex = as.character(exterior))

colors[10,5] <- "#FFFFFF"

colCount <- all_jeeps %>%
  count(exterior) %>%
  mutate(hex = as.character(exterior))

colCount[19,3] <- "#FFFFFF"

colorsFull <- full_join(colors, colCount)

plot3d(x = colors[,1:3],
       col = colors[,5][[1]],
       main="k-means clusters",
       size = log(colorsFull$n),
       type = "s")

out_lm <- lm(price ~ numWordsinComment, data = all_jeeps)

summary(out_lm)


# Plotting ----------------------------------------------------------------

ggplot(all_jeeps, aes(x = numWordsinComment, y = price)) +
  geom_point() +
  geom_smooth(method = "loess")

ggplot(all_jeeps, aes(x = listingPriceRedu, y = price)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm")

all_jeeps %>%
  filter(Mileage < 200000,
         price < 150000) %>%
  ggplot(aes(x = Mileage, y = price, col = factor(doors))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm")

ggplot(all_jeeps, aes(x = factor(doors), y = price)) +
  geom_boxplot()





# MODEL SELECTION ---------------------------------------------------------


outMOD <- all_jeeps %>% 
  lm(price ~ year + 
       numWordsinComment*sellerType +
       doors + 
       DriveTypeGeneral + 
       green*blue + 
       listingPriceRedu + 
       Mileage + 
       ownershipStatus + 
       newListingIndicator +
       Transmission + 
       Engine +
       numWordsFeatures*sellerType +
       lifted + 
       modelGeneral,
     data = .)


all_jeeps %>% 
  dplyr::select(-price, 
                  year , 
                  numWordsinComment ,
                  doors , 
                  DriveTypeGeneral , 
                  green ,
                  blue , 
                  listingPriceRedu , 
                  Mileage , 
                  ownershipStatus , 
                  newListingIndicator ,
                  Transmission , 
                  Engine ,
                  numWordsFeatures,
                  sellerType ,
                  lifted ,
                  modelGeneral) %>% 
  broom::augment(outMOD, .) %>% 
  .[ ,c(".fitted", "rowNum", "year", "Mileage", "listing_url")] -> predicted
  

good_deals <- predicted %>% 
  left_join(all_jeeps[,c("rowNum", "price")]) %>% 
  mutate(residual = .fitted - price) %>% 
  filter(Mileage < 50000,
         year > 2014) %>% 
  top_n(10, residual) %>% 
  dplyr::select(residual, listing_url) %>% 
  arrange(desc(residual))

# Python Setup ------------------------------------------------------------

if(!("autoTradeR" %in% conda_list(conda = "auto")$name)){
  conda_create("autoTradeR", packages = c("python",
                                          "pandas",
                                          "selenium",
                                          "unicodecsv"), conda = "auto")
}

tryCatch(use_condaenv("overstockShiny", conda = "auto", required = FALSE),
         error = function(e)stop("Conda environment not activated: Failure to start"))


# Opening all links for good deals ----------------------------------------

source_python("Python/ScrapeModelInfo.py")

lapply(X = as.list(good_deals$listing_url), FUN = function(X){build_listing_URL(X, read = FALSE) %>% visit_url()})

as.list(good_deals$listing_url)



# Scrape recent data ------------------------------------------------------
setwd("Data/")
autoTrader_scrape(make = "Jeep", 
                  model = "Wrangler", 
                  zip = 84604, 
                  pages = "all", 
                  locationName = "Provo", 
                  fork = 32,
                  write_csv = TRUE,
                  sellerTypes = "d",
                  path = "Data/autoTradeR-2018-06-17-Dealer")
setwd("..")

provo1 <- read_csv("Data/autoTradeR-2018-06-17-Private")
provo2 <- read_csv("Data/autoTradeR-2018-06-17-Dealer")

provo_full <- bind_rows(provo1, provo2)

provo_data <- provo_full %>% 
  distinct(`ATC Car ID`, .keep_all = TRUE) %>% 
  mutate(doors = ifelse(grepl("unlimited", listing_title, ignore.case = TRUE), yes = 4, no = 2)) %>% 
  mutate(lifted = ifelse(grepl("lift", carFeaturesText, ignore.case = TRUE), yes = TRUE, no = FALSE), 
         numWordsFeatures = lengths(strsplit(carFeaturesText, "\\W+")),
         modelGeneral = case_when(grepl("Rubicon", listing_title, ignore.case = TRUE)  ~ "Rubicon",
                                  grepl("Sport", listing_title, ignore.case = TRUE)    ~ "Sport",
                                  grepl("Sahara", listing_title, ignore.case = TRUE)   ~ "Sahara",
                                  grepl("Willy", listing_title, ignore.case = TRUE)    ~ "Willys",
                                  grepl("X", listing_title, ignore.case = TRUE)        ~ "X",
                                  TRUE                                                 ~ "other")) %>% 
  clean_result_df()



outMOD <- provo_data %>% 
  lm(price ~ year + 
       sellerType +
       doors + 
       DriveTypeGeneral + 
       listingPriceRedu + 
       Mileage + 
       ownershipStatus + 
       newListingIndicator +
       Engine, 
     data = .)

newData <- provo_data %>% 
  filter(Transmission != "Information Unavailable") 

newData$Transmission <- droplevels(newData$Transmission)

levels(newData$Transmission)

newData %>% 
  dplyr::select(-price, 
                year , 
                numWordsinComment ,
                doors , 
                DriveTypeGeneral , 
                green ,
                blue , 
                listingPriceRedu , 
                Mileage , 
                ownershipStatus , 
                newListingIndicator ,
                Transmission , 
                Engine ,
                numWordsFeatures,
                sellerType ,
                lifted ,
                modelGeneral) -> newmodel


predictions <- cbind(newmodel, pred = predict(outMOD, newdata = newmodel))

good_deals <- predictions %>% 
  left_join(provo_data[,c("rowNum", "price")]) %>% 
  mutate(residual = pred - price) %>% 
  filter(doors == 4,
         price < 30000,
         price > 10000,
         Mileage < 50000,
         year > 2013,
         sellerType == "Dealer") %>% 
  top_n(10, residual) %>%
  arrange(desc(residual))

lapply(X = as.list(good_deals$listing_url), FUN = function(X){build_listing_URL(X, read = FALSE) %>% visit_url()})






autoTrader_scrape(make = "Hyundai", 
                  model = "Elantra", 
                  zip = 84604, 
                  pages = "all", 
                  locationName = "Provo", 
                  fork = 32,
                  write_csv = TRUE,
                  path = "Data/Elantra1")

elantra <- read_csv("Data/Elantra1") %>% 
  clean_result_df()


colCount <- elantra %>%
  count(exterior) %>%
  mutate(hex = as.character(exterior))



colors <- elantra %>%
  distinct(exterior, .keep_all = TRUE) %>%
  dplyr::select(red:blue, exterior) %>%
  mutate(hex = as.character(exterior))

colors


colors[9,5] <- "#FFFFFF"


colCount[12,3] <- "#FFFFFF"

colorsFull <- full_join(colors, colCount)

plot3d(x = colors[,1:3],
       col = colors[,5][[1]],
       main="k-means clusters",
       size = log(colorsFull$n),
       type = "s")







outMOD <- elantra %>% 
  lm(price ~ year + 
       DriveTypeGeneral + 
       listingPriceRedu + 
       Mileage + 
       ownershipStatus + 
       newListingIndicator +
       Transmission +
       Engine, 
     data = .)

elantra %>% 
  dplyr::select(-price, 
                year, 
                DriveTypeGeneral, 
                listingPriceRedu, 
                Mileage, 
                ownershipStatus, 
                newListingIndicator,
                Transmission, 
                Engine) %>% 
  broom::augment(outMOD, .) %>% 
  .[ ,c(".fitted", "rowNum", "year", "Mileage", "listing_url", "listing_title", "listing_distance_miles")] -> predicted


good_deals <- predicted %>% 
  left_join(elantra[,c("rowNum", "price")]) %>% 
  mutate(residual = .fitted - price) %>%
  filter(price < 10000,
         listing_distance_miles < 50) %>% 
  top_n(10, residual) %>%
  arrange(desc(residual))


lapply(X = as.list(good_deals$listing_url), FUN = function(X){build_listing_URL(X, read = FALSE) %>% visit_url()})
