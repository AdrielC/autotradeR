### function to clean all the outputs

clean_result_df <- function(df, LocationName){
  df <- df %>% 
    mutate(
      model = sub('.*Wrangler ', '', listing_title),
      year = parse_number(listing_title),
      ownershipStatus = gsub("([A-Za-z]+).*", "\\1", listing_title),
      listing_distance_miles = as.numeric(listing_distance_miles),
      listing_title = factor(listing_title),
      listing_dealer = factor(listing_dealer),
      `Body Style` = factor(`Body Style`),
      `Drive Type` = factor(`Drive Type`),
      Engine = factor(Engine),
      Transmission = factor(Transmission),
      exterior = factor(exterior),
      Mileage = parse_number(Mileage),
      newListingIndicator = factor(newListingIndicator),
      Location = LocationName
    )
  
  MPG <- as_data_frame(str_split_fixed(df$MPG, " / ", 2))
  colnames(MPG) <- c("CityMpg", "HyMpg")
  
  full_clean <- as_tibble(cbind(df, MPG) %>% 
                            mutate(CityMpg = parse_number(CityMpg),
                                   HyMpg = parse_number(HyMpg))) %>% 
    rowid_to_column("rowNum")
  
  return(full_clean)
} 