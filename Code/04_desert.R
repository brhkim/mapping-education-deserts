################################################################################
#  
#   Name: 04_desert.R
#   
#   Purpose: 
#	  Assess the proportion of country population within various distances to
#	  school
#
################################################################################

# Create a function to calculate the share of each country in an education desert
# along a basically-continuous set of threshold distances for the definition of
# a desert in kilometers
desert_checker <- function(analyticdata) {
  
  # Calculate the total population for the country
  totalpop <- sum(analyticdata$population)
  
  # Create an empty dataframe for the output
  output <- data.frame(desert_count=as.numeric(), desert_prop=as.numeric(), totalpop=as.numeric(), distance=as.numeric())
  
  # Run a loop using varying thresholds for the definition of an education desert
  for (threshold in seq(from=0.1, to=10, by=0.1)) {
    
    # Generate a binary indicator for whether a population point is in a desert
    deserts <- analyticdata %>%
      mutate(desert_test = case_when(distance<threshold ~ 0,
                                     TRUE ~ 1))
    
    # Calculate what proportion of the country is in a desert
    loopoutput <- deserts %>%
      filter(desert_test==1) %>%
      summarize(desert_count=sum(population),
                desert_prop=sum(population)/totalpop,
                totalpop=totalpop,
                distance=threshold)
    
    # Add the data to a row in the output dataset
    output <- bind_rows(output, loopoutput)
  }
  
  # Spit it out
  output
  
}

# Run desert calculation using the constructed distance dataset
country_desert_new <- country_dist_new %>% desert_checker()

# Do the same for additional year of data if applicable
if(multiyear==TRUE) {
  country_desert_old <- country_dist_old %>% desert_checker()
  
  # Also construct comparison datasets for visualizations using both old and new data years if applicable
  compare_deserts <- bind_rows(year_old=country_desert_old, year_new=country_desert_new, .id="year") %>%
    mutate(year=case_when(year=="year_old" ~ paste0(oldyear),
                          year=="year_new" ~ paste0(newyear)))
  
  compare_dist <- bind_rows(year_old=country_dist_old, year_new=country_dist_new, .id="year") %>%
    mutate(year=case_when(year=="year_old" ~ paste0(oldyear),
                          year=="year_new" ~ paste0(newyear)))
}


# Create a chart showing the proportion of population living in a desert at varying thresholds
ggplot(country_desert_new) +
  geom_line(aes(x=distance, y=desert_prop)) +
  theme_minimal() + 
  theme(axis.text = element_text(size=12), legend.text = element_text(size=12), legend.title = element_text(size=12)) + 
  labs(x="\nDistance Threshold in Kilometers", y="Proportion of Population Living in a Desert", 
       title=paste0("Proportion of ", countryname, " Population Living in Education Desert at Varying Distance Thresholds"), subtitle=paste0("For ", schooltype, " in ", newyear))
ggsave(file.path(project_output, paste0(countryname, '_threshold_', newyear, '.png')), width=10, height=6)

# Create a chart showing the distribution of distances to school across country population
ggplot(country_dist_new) +
  geom_density(aes(x=distance, weight=population)) + 
  theme_minimal() + 
  scale_x_continuous(limits=c(0,10), oob=scales::squish) + 
  theme(axis.text = element_text(size=12), legend.text = element_text(size=12), legend.title = element_text(size=12)) + 
  labs(x="\nDistance to Nearest School (km)", y="Density", 
       title=paste0("Distribution of Distance to Nearest School Across ", countryname, " Population"), subtitle=paste0("For ", schooltype, " in ", newyear))
ggsave(file.path(project_output, paste0(countryname, '_hist_', newyear, '.png')), width=10, height=6)

# Create a map showing the distance to school across country
ggplot() + 
  theme_minimal() +
  geom_sf(data=country_dist_new, aes(geometry = geometry, color=distance, stroke=0), size=0.7, shape=15) + 
  geom_sf(data=country_outline, size=1, color="#1c1c1c", fill=NA) + 
  scale_color_viridis_c(option="plasma", limits=c(0,15), oob=scales::squish, breaks=c(0,5,10,15), labels=c("0", "5", "10", "15+")) + 
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  guides(shape=FALSE) + 
  labs(color="Distance in km \n", 
       title=paste0("Distance to School Across ", countryname), subtitle=paste0("For ", schooltype, " in ", newyear))
ggsave(file.path(project_output, paste0(countryname, '_map_', newyear, '.png')), width=10, height=6)

if(multiyear==TRUE) {

  # Create a map showing the distance to school across country for older year if multiyear analysis is enabled
  ggplot() + 
    theme_minimal() +
    geom_sf(data=country_dist_old, aes(geometry = geometry, color=distance, stroke=0), size=0.7, shape=15) +
    geom_sf(data=country_outline, size=1, color="#1c1c1c", fill=NA) + 
    scale_color_viridis_c(option="plasma", limits=c(0,15), oob=scales::squish, breaks=c(0,5,10,15), labels=c("0", "5", "10", "15+")) + 
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) + 
    guides(shape=FALSE) + 
    labs(color="Distance in km \n", 
         title=paste0("Distance to School Across ", countryname), subtitle=paste0("For ", schooltype, " in ", oldyear))
  ggsave(file.path(project_output, paste0(countryname, '_map_', oldyear, '.png')), width=10, height=6)
  
  # Create a chart showing the proportion of population living in a desert at varying thresholds, but comparing across years for the country
  ggplot(compare_deserts) +
    geom_line(aes(x=distance, y=desert_prop, color=year)) +
    theme_minimal() + 
    theme(axis.text = element_text(size=12), legend.text = element_text(size=12), legend.title = element_text(size=12)) + 
    labs(x="\nDistance Threshold in Kilometers", y="Proportion of Population Living in a Desert", color="Year", 
         title=paste0("Proportion of ", countryname, " Population Living in Education Desert at Varying Distance Thresholds"), subtitle=paste0("For ", schooltype, " Across Years"))
  ggsave(file.path(project_output, paste0(countryname, '_threshold_multiyear.png')), width=10, height=6)
  
  # Create a chart showing the distribution of distances to school across country population, but comparing across years for the country
  ggplot(compare_dist) +
    geom_density(aes(x=distance, weight=population, color=year)) + 
    theme_minimal() + 
    scale_x_continuous(limits=c(0,10), oob=scales::squish) + 
    theme(axis.text = element_text(size=12), legend.text = element_text(size=12), legend.title = element_text(size=12)) + 
    labs(x="\nDistance to Nearest School (km)", y="Density",  color="Year",
         title=paste0("Distribution of Distance to Nearest School Across ", countryname, " Population"), subtitle=paste0("For ", schooltype, " Across Years"))
  ggsave(file.path(project_output, paste0(countryname, '_hist_multiyear.png')), width=10, height=6)
  

}