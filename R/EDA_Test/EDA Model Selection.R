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
                                  TRUE                                                 ~ "other"),
         numUpperWordsinComment = unlist(lapply(sellerComment, count_upper_words)),
         branded = case_when(grepl("branded", sellerComment, ignore.case = TRUE)  ~ TRUE,
                             TRUE                                                 ~ FALSE),
         logNumUpperWordsinComment = log1p(numUpperWordsinComment)) %>% 
  clean_result_df()


all_jeeps %>% 
  ggplot(aes(x = modelGeneral, fill = ownershipStatus)) +
  geom_histogram(position = "fill", stat = "count")

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
       modelGeneral*Mileage + 
       ownershipStatus + 
       newListingIndicator +
       Transmission + 
       Engine +
       numWordsFeatures*sellerType +
       lifted +
       numUpperWordsinComment*sellerType,
     data = .)

summary(outMOD)


# all_jeeps %>% 
#   dplyr::select(-price, 
#                   year , 
#                   numWordsinComment ,
#                   doors , 
#                   DriveTypeGeneral , 
#                   green ,
#                   blue , 
#                   listingPriceRedu , 
#                   Mileage , 
#                   ownershipStatus , 
#                   newListingIndicator ,
#                   Transmission , 
#                   Engine ,
#                   numWordsFeatures,
#                   sellerType ,
#                   lifted ,
#                   modelGeneral) %>% 
#   broom::augment(outMOD, .) %>% 
#   .[ ,c(".fitted", "rowNum", "year", "Mileage", "listing_url")] -> predicted
  

# good_deals <- predicted %>% 
#   left_join(all_jeeps[,c("rowNum", "price")]) %>% 
#   mutate(residual = .fitted - price) %>% 
#   filter(Mileage < 50000,
#          year > 2014) %>% 
#   top_n(10, residual) %>% 
#   dplyr::select(residual, listing_url) %>% 
#   arrange(desc(residual))

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
                                  TRUE                                                 ~ "other"),
         numUpperWordsinComment = unlist(lapply(sellerComment, count_upper_words)),
         logNumUpperWordsinComment = log1p(numUpperWordsinComment)) %>% 
  clean_result_df()


newData <- provo_data %>% 
  filter(Transmission != "Information Unavailable") 

newData$Transmission <- droplevels(newData$Transmission)

levels(newData$Transmission)

predictions <- cbind(newData, pred = predict(outMOD, newdata = newData))

good_deals <- predictions %>% 
  left_join(provo_data[,c("rowNum", "price")]) %>% 
  mutate(residual = pred - price) %>% 
  filter(doors == 4,
         price < 24000,
         price > 10000,
         Mileage < 50000,
         year > 2013) %>% 
  top_n(5, residual) %>%
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



provo_data %>% 
  filter(Mileage < 200000,
         price < 100000) %>%
  dplyr::select(price, Mileage, year, sellerType) %>% 
  plot3d(x = .$Mileage,
         y = .$price,
         z = .$year)






# Car Prediction ------------------------------------------------------------

JeepSahara <- "https://www.autotrader.com/cars-for-sale/vehicledetails.xhtml?listingId=485403585&zip=84604&referrer=%2Fcars-for-sale%2Fsearchresults.xhtml%3Fzip%3D84604%26startYear%3D1981%26numRecords%3D100%26sortBy%3Drelevance%26maxPrice%3D17000%26incremental%3Dall%26firstRecord%3D0%26endYear%3D2019%26modelCodeList%3DWRANGLER%26makeCodeList%3DJEEP%26searchRadius%3D50&startYear=1981&numRecords=100&maxPrice=17000&firstRecord=0&endYear=2019&modelCodeList=WRANGLER&makeCodeList=JEEP&searchRadius=50&makeCode1=JEEP&modelCode1=WRANGLER"

jeepInfoTable <- extract_listing_data(JeepSahara)

jeepInfoTable$price <- 16995
jeepInfoTable$listingPriceRedu <- FALSE
jeepInfoTable$`Body Style` <- "Sport Utility"

SaharaInfo <- all_jeeps[which(all_jeeps$modelGeneral == "Sahara"),][1,]
SaharaInfo$listing_title <- "Used 2007 Jeep Wrangler 4WD Unlimited Sahara"
SaharaInfo$price <- 16995
SaharaInfo$listingPriceRedu <- FALSE
SaharaInfo$`Body Style` <- "Sport Utility"
SaharaInfo$Mileage <- 95829
SaharaInfo$sellerType <- "Dealer"
SaharaInfo$year <- 2007
SaharaInfo$Transmission <- "5-Speed Automatic"
SaharaInfo$Transmission <- as.factor(SaharaInfo$Transmission)

prediction <- cbind(SaharaInfo, pred = predict(outMOD, newdata = SaharaInfo))




all_Sahara <- all_jeeps[which(all_jeeps$modelGeneral == "Sahara"),]

outMOD <- lm(price ~ 
               Mileage +
               Transmission +
               year +
               model
             , data = all_Sahara)

summary(outMOD)

newData <- SaharaInfo %>% 
  dplyr::select(Mileage, Transmission, year, model)

prediction <- cbind(newData, pred = predict(outMOD, newdata = newData))







# X -----------------------------------------------------------------------



all_x <- all_jeeps[which(all_jeeps$modelGeneral == "X"),]

outMOD <- lm(price ~ 
               Mileage +
               year +
               doors +
               lifted
             , data = all_x)

summary(outMOD)

newData <- tibble(Mileage = 125732,
                  year = 2009,
                  doors = 4,
                  lifted = FALSE)

prediction <- cbind(newData, pred = predict(outMOD, newdata = newData))

prediction







# Rubicon -----------------------------------------------------------------------


all_rubicon <- all_jeeps[which(all_jeeps$modelGeneral == "Rubicon"),]

outMOD <- lm(price ~ 
               Mileage * year +
               doors * lifted +
               ownershipStatus +
               newListingIndicator +
               listingPriceRedu +
               numUpperWordsinComment
             , data = all_rubicon)

summary(outMOD)

newData <- tibble(Mileage = 126481,
                  year = 2009,
                  doors = 4,
                  lifted = FALSE,
                  ownershipStatus = "Used",
                  newListingIndicator = TRUE,
                  listingPriceRedu = FALSE,
                  numUpperWordsinComment = 14
                  )

newData$newListingIndicator <- factor(newData$newListingIndicator)

prediction <- cbind(newData, pred = predict(outMOD, newdata = newData))

prediction


outMOD <- all_jeeps %>% 
  lm(price ~ year + 
       numWordsinComment*sellerType +
       doors + 
       DriveTypeGeneral + 
       green*blue + 
       listingPriceRedu + 
       modelGeneral*Mileage + 
       ownershipStatus + 
       newListingIndicator +
       Transmission + 
       Engine +
       numWordsFeatures*sellerType +
       lifted +
       numUpperWordsinComment*sellerType,
     data = .)

summary(outMOD)


newData <- tibble(Mileage = 126481,
                  year = 2009,
                  doors = 4,
                  lifted = FALSE,
                  ownershipStatus = "Used",
                  newListingIndicator = TRUE,
                  listingPriceRedu = FALSE,
                  numUpperWordsinComment = 14,
                  numWordsinComment = 339,
                  DriveTypeGeneral = "Automatic",
                  green = 200,
                  blue = 200,
                  modelGeneral = "Rubicon",
                  Transmission = "Automatic",
                  Engine = "6-Cylinder",
                  sellerType = "Dealer",
                  numWordsFeatures = 25
                  )

newData$newListingIndicator <- factor(newData$newListingIndicator)

prediction <- cbind(newData, pred = predict(outMOD, newdata = newData))

prediction




all_rubicon %>% 
  filter(price < 70000) %>% 
  ggplot(aes(x = Mileage, y = price, fill = sellerType)) +
  geom_point() +
  geom_smooth(method = "lm")

