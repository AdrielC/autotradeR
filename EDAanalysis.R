source("JeepScrapeFunc.R")
source("InfoExtractionFunc.R")
source("cleanResultFunc.R")
library(ggplot2)
library(stringr)
library(purrr)
library(rvest)
library(httr)
library(readr)
library(leaps)
library(MASS)

# full_clean <- read_csv("JeepData_05-04-18.csv", guess_max = 9000)
# full_clean <- read_csv("JeepData_05-05-18.csv", guess_max = 9000)

# DataExtractAndClean -----------------------------------------------------------------

### 100 Miles from Provo
Provo <- query_autoTrader(make = "jeep", model = "wrangler", zip = 84604, pages = "all", searchRadius = 100) %>% 
  query_URL_reader() %>% 
  scraper_apply()

### 100 Miles from Logan
Logan <- query_autoTrader(make = "jeep", model = "wrangler", zip = 84321, pages = "all", searchRadius = 100) %>% 
  query_URL_reader() %>% 
  scraper_apply()

### 100 Miles from Las Vegas
Vegas <- query_autoTrader(make = "jeep", model = "wrangler", zip = 88901, pages = "all", searchRadius = 100) %>% 
  query_URL_reader() %>% 
  scraper_apply()

### 100 Miles from Bethesda/DC
DC <- query_autoTrader(make = "jeep", model = "wrangler", zip = 20895, pages = "all", searchRadius = 100) %>% 
  query_URL_reader() %>% 
  scraper_apply()

### 100 Miles from Corolla, NC
Corolla <- query_autoTrader(make = "jeep", model = "wrangler", zip = 27927, pages = "all", searchRadius = 100) %>% 
  query_URL_reader() %>% 
  scraper_apply()

### 100 Miles from Denver, CO
Denver <- query_autoTrader(make = "jeep", model = "wrangler", zip = 80014, pages = "all", searchRadius = 100) %>% 
  query_URL_reader() %>% 
  scraper_apply()

Dallas <- query_autoTrader(make = "jeep", model = "wrangler", zip = 75315, pages = "all", searchRadius = 100) %>% 
  query_URL_reader() %>% 
  scraper_apply()

Austin <- query_autoTrader(make = "jeep", model = "wrangler", zip = 73301, pages = "all", searchRadius = 100) %>% 
  query_URL_reader() %>% 
  scraper_apply()

Austin <- query_autoTrader(make = "jeep", model = "wrangler", zip = 73301, pages = "all", searchRadius = 100) %>% 
  query_URL_reader() %>% 
  scraper_apply()

ElPaso <- query_autoTrader(make = "jeep", model = "wrangler", zip = 79916, pages = "all", searchRadius = 100) %>% 
  query_URL_reader() %>% 
  scraper_apply()

Pheonix <- query_autoTrader(make = "jeep", model = "wrangler", zip = 85001, pages = "all", searchRadius = 100) %>% 
  query_URL_reader() %>% 
  scraper_apply()


Reno <- query_autoTrader(make = "jeep", model = "wrangler", zip = 89505, pages = "all", searchRadius = 100) %>% 
  query_URL_reader() %>% 
  scraper_apply()


BoulderCity <- query_autoTrader(make = "jeep", model = "wrangler", zip = 89006, pages = "all", searchRadius = 100) %>% 
  query_URL_reader() %>% 
  scraper_apply()


Pocatello <- query_autoTrader(make = "jeep", model = "wrangler", zip = 83201, pages = "all", searchRadius = 100) %>% 
  query_URL_reader() %>% 
  scraper_apply()

Boise <- query_autoTrader(make = "jeep", model = "wrangler", zip = 83799, pages = "all", searchRadius = 100) %>% 
  query_URL_reader() %>% 
  scraper_apply()

ProvoClean <- clean_result_df(Provo, "Provo")
VegasClean <- clean_result_df(Vegas, "Vegas")
DCClean <- clean_result_df(DC, "DC")
LoganClean <- clean_result_df(Logan, "Logan")
AustinClean <- clean_result_df(Austin, "Austin")
BoiseClean <- clean_result_df(Boise, "Boise")
BoulderCityClean <- clean_result_df(BoulderCity, "BoulderCity")
DallasClean <- clean_result_df(Dallas, "Dallas")
DenverClean <- clean_result_df(Denver, "Denver")
PheonixClean <- clean_result_df(Pheonix, "Pheonix")
PocatelloClean <- clean_result_df(Pocatello, "Pocatello")
RenoClean <- clean_result_df(Reno, "Reno")

full_clean <- bind_rows(mget(ls()))

# Data plotting -----------------------------------------------------------

full_clean %>% 
  filter(
    year >= 2014,
    Mileage < 50000
  ) %>% 
  ggplot(aes(x = Mileage, y = price)) +
    geom_point() +
    geom_smooth(method = "lm") + 
    geom_text(aes(label = rowNum), hjust=0, vjust=0)

good_deal <- full_clean_all[6125,]$listing_url

lm_out21 <- lm(price ~ Mileage + year, data = full_clean)

summary(lm_out21)

min(residuals(lm_out21))

residualsCars <- tibble(residuals = residuals(lm_out21),
                           number = row_number(residuals(lm_out21)))


residualsCars %>% 
  top_n(-10)



ggplot(full_clean, aes(x = log(Mileage + 1), y = price)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(full_clean, aes(x = log(Mileage + 1), y = price)) +
  geom_point() +
  geom_smooth(method = "loess")

full_clean %>% 
  group_by(exterior) %>% 
  summarise(meanPrice = mean(price, na.rm = T), N = n()) %>% 
  filter(N > 10) %>% 
  arrange(desc(meanPrice))

full_clean %>% 
  group_by(listing_dealer) %>% 
  summarise(meanPrice = mean(price, na.rm = T), N = n()) %>% 
  filter(N > 10) %>% 
  arrange(desc(meanPrice))

ggplot(full_clean, aes(x = listing_distance_miles, y = price)) +
  geom_jitter() +
  geom_smooth(method = "lm")

full_clean %>% 
  group_by(ownershipStatus) %>% 
  summarise(meanPrice = mean(price, na.rm = T), N = n()) %>% 
  filter(N > 10) %>% 
  arrange(desc(meanPrice))

full_clean %>% 
  group_by(listing_dealer) %>% 
  summarise(meanPrice = mean(price, na.rm = T), N = n()) %>% 
  filter(N > 10) %>% 
  arrange(meanPrice)

full_clean_all %>% 
  group_by(year) %>% 
  summarise(meanPrice = mean(price, na.rm = T), N = n()) %>% 
  filter(N > 10) %>% 
  arrange(meanPrice) %>% 
  ggplot(aes(x = as.factor(year), y = meanPrice, fill = year)) +
  geom_bar(stat = "identity")

full_clean %>% 
  group_by(Transmission) %>% 
  summarise(meanPrice = mean(price, na.rm = T), N = n()) %>% 
  filter(N > 10) %>% 
  arrange(meanPrice)

ggplot(full_clean, aes(x = Mileage, y = price)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Transmission)

top_4_dealers <- full_clean %>% 
  count(listing_dealer) %>% 
  top_n(4) %>% 
  .[,1]

full_clean %>% 
  filter(listing_dealer %in% top_4_dealers$listing_dealer) %>% 
  mutate(listing_dealer = droplevels(listing_dealer)) %>% 
  ggplot(aes(x = log(Mileage + 1), y = price)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~listing_dealer)
  

ggplot(full_clean, aes(x = log(Mileage + 1), y = price)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~as.factor(CityMpg))

lm_out <- full_clean %>% 
  lm(price ~ Mileage + listing_distance_miles + price_reduction + year + CityMpg + HyMpg + ownershipStatus, data = .)

lm_out1 <- full_clean %>% 
  lm(price ~ Mileage + listing_distance_miles + year + CityMpg + ownershipStatus, data = .)

summary(lm_out1)

lm_out2 <- full_clean %>% 
  lm(price ~ Mileage + listing_distance_miles + year + CityMpg, data = .)

summary(lm_out2)

lm_out3 <- full_clean %>% 
  lm(price ~ Mileage + year + CityMpg, data = .)

lm_out4 <- full_clean %>% 
  lm(price ~ Mileage + year + CityMpg, data = .)

lm_out5 <- full_clean %>% 
  lm(price ~ Mileage + year + Mileage*year, data = .)

summary(lm_out5)

str(full_clean)

full_clean1 <- full_clean %>% 
  mutate(Mileage = as.numeric(Mileage),
         ownershipStatus = factor(ownershipStatus),
         CityMpg = as.numeric(CityMpg),
         HyMpg = as.numeric(HyMpg))

explanVars <- c("listing_distance_miles","year","CityMpg","HyMpg","ownershipStatus","Transmission","Engine","price_reduction","listing_dealer")
explanVarsCol <- as.matrix(full_clean[,explanVars])

leapmodels <- leaps(x = explanVarsCol, y = full_clean$price)

leapModels <- regsubsets(price ~ listing_distance_miles + year + ownershipStatus + Engine + price_reduction + Mileage, data=full_clean1, nbest=10)

subsets(leapModels, statistic="rsq")

plot(leapModels)

lm_eqn()

full_clean_all %>% 
  count(model) %>% 
  top_n(5)

filtered <- filter(full_clean_all, model %in% c("4WD Unlimited Rubicon",
                                    "4WD Unlimited Sahara",
                                    "4WD Unlimited Sport"),
                   year > 2010)

ggplot(filtered, aes(x = Mileage, y = price)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", aes(col = filtered$listingPriceRedu)) +
  facet_wrap(~model)

ggplot(filtered, aes(x = newListingIndicator, y = price)) +
  geom_boxplot(aes(col = filtered$listingPriceRedu)) +
  facet_wrap(~year)


# Testing -----------------------------------------------------------------
