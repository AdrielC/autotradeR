### function to clean all the outputs

clean_result_df <- function(df, locationName = NA){
  df <- df %>% 
    mutate(
      model = sub('.*Wrangler ', '', listing_title),
      year = suppressWarnings(parse_number(listing_title)),
      ownershipStatus = suppressWarnings(gsub("([A-Za-z]+).*", "\\1", listing_title)),
      listing_distance_miles = suppressWarnings(as.numeric(listing_distance_miles)),
      listing_title = factor(listing_title),
      `Dealer` = factor(`Dealer`),
      `Body Style` = factor(`Body Style`),
      `Drive Type` = factor(`Drive Type`),
      Engine = factor(Engine),
      Transmission = factor(Transmission),
      exterior = factor(exterior),
      Mileage = suppressWarnings(parse_number(Mileage)),
      newListingIndicator = factor(newListingIndicator),
      Location = locationName,
      sellerType = ifelse(Dealer == "Private Seller", "Private", "Dealer")
    )
  
  MPG <- as_data_frame(suppressWarnings(str_split_fixed(df$MPG, " / ", 2)))
  colnames(MPG) <- c("CityMpg", "HyMpg")
  
  full_clean <- as_tibble(cbind(df, MPG) %>% 
                            mutate(CityMpg = suppressWarnings(parse_number(CityMpg)),
                                   HyMpg = suppressWarnings(parse_number(HyMpg)))) %>% 
    rowid_to_column("rowNum") %>% 
    dplyr::select(-MPG)
  
  return(full_clean)
} 


