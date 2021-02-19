################################################################################
#  
#   Name: 02_preprocess.R
#   
#   Purpose:
#	  Conduct basic cleaning and conversion of the imported data objects for analysis
#
################################################################################

# Get everything into format compatible with sf package with a function
split_convert <- function(dataset) {
  
  # Get a separate dataset for just the coordinates
  coords <- dataset %>%
    filter(!is.na(x) & !is.na(y)) %>%
    dplyr::select(x, y)
  
  # Get a separate dataset for all the accompanying metadata
  attributes <- dataset %>%
    filter(!is.na(x) & !is.na(y)) %>%
    dplyr::select(-x, -y)
    
  # Convert the coordinates to sf
  coords <- coords %>% st_as_sf(coords=c("x", "y"), crs="WGS84")
    
  # Put the converted coordinates back with the metadata
  output <- st_sf(cbind(attributes, coords))
  
  # Spit it out
  output
}

# Convert schools and population data to sf objects
country_schools_new <- country_schools_new %>% split_convert() %>%
  mutate(schoolid=as.numeric(rownames(.)))
country_pop_new <- country_pop_new %>% split_convert() %>%
  mutate(popid=as.numeric(rownames(.)))

# Do the same for additional year of data if applicable
if(multiyear==TRUE) {
  country_schools_old <- country_schools_old %>% split_convert() %>%
    mutate(schoolid=as.numeric(rownames(.)))
  country_pop_old <- country_pop_old %>% split_convert() %>%
    mutate(popid=as.numeric(rownames(.)))
}

