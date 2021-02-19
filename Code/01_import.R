################################################################################
#  
#   Name: 01_import.R
#   
#   Purpose: 
#	  Import the various datasets required to run the analysis
#
################################################################################

# Load in Schools Data
country_schools_new <- read_csv(country_schools_new_path) %>% 
  # Minor cleaning of variable names
  rename(x=longitude, y=latitude)

# Load in population Data, converted to points and then dataframes
country_pop_new_raster <- raster(country_pop_new_path) 
country_pop_new <- country_pop_new_raster %>%
  rasterToPoints() %>% 
  as.data.frame() 
colnames(country_pop_new) <- c("x", "y", "population")

# Load in schools and pop data for the older year if running multiyear analysis
if(multiyear==TRUE) {
  country_schools_old <- read_csv(country_schools_old_path) %>% 
    # Minor cleaning of variable names
    rename(x=longitude, y=latitude)
  
  country_pop_old <- raster(country_pop_old_path) %>%
    rasterToPoints() %>% 
    as.data.frame()
  colnames(country_pop_old) <- c("x", "y", "population")
}

# Load in shapefile for the country and create a border shape just for aesthetic appeal in the country plots
country_outline <- st_read(country_shape_path)

  # Create a special version of the border data for compatibility with rayshader
outline_3d <- st_cast(country_outline$geometry, "MULTILINESTRING") %>%
  st_buffer(dist=0.01)

# Load in enrollment data by region if running the analysis to compare desert stats to enrollment stats by region
if(enrollmentanalysis==TRUE) {
  country_regions <- st_read(country_regions_path) %>%
    st_transform(crs="WGS84")
  country_enrollment <- read_csv(country_enrollment_path)
  
  country_regions <- left_join(country_regions, country_enrollment, by=enrollment_merge_var)
}
  
# Load in elevation data if running the elevation-adjusted pathing analysis
if(elevationanalysis==TRUE) {
  country_elevation <- raster(country_elevation_path) %>%
    aggregate(fun=mean, fact=elevationscaler)
  
    # Adjust the elevation raster to fit with the population raster
    country_elevation <- extend(country_elevation, country_pop_new_raster)
}


