library(rgl) # for 3d plotting
library(stringr) # Number of characters and other string manip
library(tidyverse)
library(rvest) # read_html functions and the like
library(httr)
library(leaps)
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
  distinct(`ATC Car ID`, .keep_all = TRUE)

all_jeeps <- clean_result_df(all_jeeps)


# kmeans on RGB data ------------------------------------------------------

all_jeeps %>% 
  dplyr::select(price, Mileage, year) %>% 
  filter(price < 60000,
         Mileage < 100000,
         year > 2000) %>% 
  .[complete.cases(.), ] %>% 
  elbow_plot(K = 7)

# Looks like 2 clusters may work best here

out_hc <- all_jeeps %>% 
  dplyr::select(price, Mileage, year) %>% 
  dist() %>% 
  hclust(method = "complete")

# Plot the dendrogram.
plot(out_hc)

# Plotting colors
colors <- all_jeeps %>% 
  distinct(exterior, .keep_all = TRUE) %>% 
  dplyr::select(red:blue, exterior) %>% 
  mutate(hex = as.character(exterior))

colors[10,5] <- "#FFFFFF"

plot3d(x = colors[,1:3], col = colors[,5],main="k-means clusters")


plot(train$Education, train$Fertility)
plot(train$Catholic, train$Fertility)
plot(train$Infant.Mortality, train$Fertility)
hist(train$Fertility)

first_row_colnames(col2rgb(all_jeeps$exterior[1]), transpose = TRUE)
