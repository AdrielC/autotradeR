library(tidyverse)

empty_auto_df <- function()
{
  tibble(
    VIN = NA_character_,
    name = NA_character_, 
    image = NA_character_, 
    offers.type = NA_character_, 
    price = NA_character_, 
    price.valid.until = NA_character_, 
    condition = NA_character_, 
    availability = NA_character_, 
    seller.type = NA_character_, 
    seller.name = NA_character_, 
    seller.telephone = NA_character_,
    seller.region = NA_character_,
    seller.zip = NA_character_,
    seller.address = NA_character_,
    brand = NA_character_,
    model = NA_character_,
    manufacturer = factor(NA_character_),
    year = NA_integer_,
    drivewheel = factor(NA_character_),
    engine = factor(NA_character_),
    transmission = factor(NA_character_),
    color = factor(NA_character_),
    mileage = NA_character_,
    body.type = factor(NA_character_),
    listing.url = NA_character_,
    color.interior = NA_character_,
    fuel.efficiency = NA_character_,
    fuel.type = factor(NA_character_),
    SKU = NA_character_,
    description = NA_character_,
    is.private = NA,
    is.used = NA
  )
}

# HELP & INTRO DATA ---------------------------------------------------------------

steps <- read_csv2("../R/help.csv")
intro <- read_csv2("../R/intro.csv")
makes <- read_csv("../R/makes.csv")
makes_lookup <- as.list(makes)

ROW_NAMES <- names(empty_auto_df())

comma_sep <- function(list) reduce(list, function(a, b) paste(a, b, sep = ","), .init = "")
