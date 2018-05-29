### function to clean all the outputs

clean_result_df <- function(df, locationName = NA)
{
  if("rowNum" %in% colnames(df)){
    df <- dplyr::select(df, -rowNum)
  }
  
  df <- df %>% 
    mutate(
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
      sellerType = ifelse(Dealer == "Private Seller", "Private", "Dealer"),
      DriveTypeGeneral = ifelse(grepl("Automatic", Transmission), "Automatic", 
                                ifelse(grepl("Manual", Transmission), "Manual", NA)),
      numWordsinComment = stringr::str_count(sellerComment)
    ) %>% 
    rowid_to_column("rowNum")
  
  if( !("CityMpg" %in% colnames(df)) ){
    MPG <- as_data_frame(suppressWarnings(str_split_fixed(df$MPG, " / ", 2)))
    colnames(MPG) <- c("CityMpg", "HyMpg")
    
    df <- as_tibble(cbind(df, MPG) %>% 
                              mutate(CityMpg = suppressWarnings(parse_number(CityMpg)),
                                     HyMpg = suppressWarnings(parse_number(HyMpg)))) %>% 
      dplyr::select(-MPG)
  }
  
  df <- df %>% 
    dplyr::select(exterior) %>% 
    map_dfc(function(x){
      df <- grDevices::col2rgb(x) # This changes hex colors (one column) to rgb values(3 columns).
      df <- as_tibble(t(df))
      return(df)
    }) %>% 
    bind_cols(. ,df)
  
  return(df)
}
