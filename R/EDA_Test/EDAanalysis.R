source("JeepScrapeFunc.R")
source("InfoExtractionFunc.R")
source("cleanResultFunc.R")
library(tidyverse)
library(rvest)
library(httr)
library(readr)
library(leaps)
library(MASS)
library(broom)

full_clean <- read_csv("JeepData_05-05-18.csv", guess_max = 9000) %>% 
  rowid_to_column("rowNum")

# Data plotting -----------------------------------------------------------

full_clean %>% 
  distinct(VIN, .keep_all = TRUE) %>% 
  filter(
    as.character(Location) %in% c("Provo", "Logan", "Vegas", "Pocatello", "Boise"),
    model %in% c("4WD Unlimited Rubicon", "JK 4WD Rubicon", "JK 4WD Unlimited Rubicon"),
    year >= 2012,
    Mileage < 100000,
    Mileage > 100
  ) %>% 
  ggplot(aes(x = Mileage, y = price)) +
    geom_point() +
    geom_smooth(method = "lm") + 
    geom_text(aes(label = rowNum), hjust=0, vjust=0)

lm_out <- full_clean %>% 
  filter(
    model %in% c("4WD Unlimited Rubicon", "JK 4WD Rubicon", "JK 4WD Unlimited Rubicon"),
    year == 2012,
    Mileage < 100000,
    Mileage > 0
  ) %>% 
  lm(price ~ Mileage, data = .)

Daryl <- tibble(
  Mileage = 94000
)

augment(lm_out, newdata = Daryl)


good_deal <- full_clean[365,]$listing_url

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
  group_by(exterior) %>% 
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

full_clean %>% 
  filter(
    Location %in% c("Provo", "Logan", "Vegas", "Pocatello", "Boise"),
    model %in% c("4WD Unlimited Rubicon", "JK 4WD Rubicon", "JK 4WD Unlimited Rubicon"),
    grep("Rubicon", model),
    year >= 2012,
    Mileage < 100000,
    Mileage > 0
  ) %>%
  group_by(year, Location) %>% 
  summarise(meanPrice = mean(price, na.rm = T)) %>% 
  ggplot(data = ., aes(x = factor(year), y = meanPrice)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Location, scales = "free")


# Testing -----------------------------------------------------------------
