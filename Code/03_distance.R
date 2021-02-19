################################################################################
#  
#   Name: 03_distance.R
#   
#   Purpose: 
#	  Calculate the distance between each population point and its closest school
#
################################################################################

# Create a function to calculate the distance between a population point and its
# closest school
closest_school <- function(populationdata, schooldata) {
  
  # Basic timer function just to see how long things take
  start_time <- proc.time()
  
  # Find the nearest school for each population point
  nearest_school_index <- st_nearest_feature(populationdata, schooldata)
  
  # Get a dataset of the corresponding nearest school point for each population point
  nearest_schools <- schooldata[nearest_school_index, ]
  
  # Calculate the distance between each population point and its nearest school
  pop_dist_to_school <- st_distance(populationdata, nearest_schools, by_element=TRUE)
  
  # Print out how long that whole process took
  print(paste("Time taken:", round(((proc.time() - start_time)[3])/60, 3), "minutes", sep=" "))
  
  # Create a dataset with all the relevant information
  output <- data.frame(populationdata, distance=as.vector(pop_dist_to_school)/1000, schoolid=nearest_school_index, nearest_schools$geometry) %>%
    rename(school_point = geometry.1)
  
  # Spit it out
  output
}

# Now apply to each country context for each level of schooling
country_dist_new <- closest_school(populationdata=country_pop_new, schooldata=country_schools_new)

# Do the same for additional year of data if applicable
if(multiyear==TRUE) {
  country_dist_old <- closest_school(populationdata=country_pop_old, schooldata=country_schools_old)
}



