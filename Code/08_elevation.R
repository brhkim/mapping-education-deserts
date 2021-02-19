################################################################################
#  
#   Name: 08_elevation.R
#   
#   Purpose: 
#	  Recalculate each population point's distance to the nearest school using
#	  contextual information on geographic elevation
#
################################################################################

# Basics to set up a transition layer that tracks slope across elevations
heightDiff <- function(x){x[2] - x[1]}
hd <- transition(country_elevation,heightDiff,8,symm=FALSE)
slope <- geoCorrection(hd)

adj <- adjacent(country_elevation, cells=1:ncell(country_elevation), pairs=TRUE, directions=8)
speed <- slope
speed[adj] <- exp(-3.5 * abs(slope[adj] - 0.05))
    # Note, this is a reversed version of Tobler's hiking function.
    # We mirror it here so that when we calculate school > home instead of
    # home > school for computational efficiency's sake, we get the correct
    # pathing and cost function

costmatrix <- geoCorrection(speed)

# Create a function to get pairings between each population point and
# only the 5 closest schools for each for use in the elevation function
close_schools <- function(populationdata, schooldata) {
  start_time <- proc.time()
  
  temp1 <- st_coordinates(populationdata) %>% as.data.frame()
  temp1 <- data.frame(x=temp1$X, y=temp1$Y, population=populationdata$population, popid=populationdata$popid, merge=1)
  
  temp2 <- st_coordinates(schooldata) %>% as.data.frame()
  temp2 <- data.frame(x=temp2$X, y=temp2$Y, schoolid=schooldata$schoolid, merge=1)
  
  output <- data.frame(popx=as.numeric(), popy=as.numeric(), population=as.numeric(), popid=as.numeric(), schoolx=as.numeric(), schooly=as.numeric(), schoolid=as.numeric())
  
  for(i in 1:nrow(temp1)){
    temp1a <- temp1[i,]
    
    crossed <- full_join(temp1a, temp2, by="merge") %>%
      rename(popx=x.x, popy=y.x, schoolx=x.y, schooly=y.y) %>%
      mutate(distance_rough=111.32*sqrt((popx-schoolx)^2+(popy-schooly)^2)) %>%
      slice_min(distance_rough, n=5) %>%
      dplyr::select(-merge)

    output <- bind_rows(output, crossed)
  }
  
  print(paste("Time taken:", round(((proc.time() - start_time)[3])/60, 3), "minutes", sep=" "))
  
  output
  
}

# Create a function that calculates the shortest path for each population point to each school
cost_distance <- function(pairsdata) {
  
  start_time <- proc.time()
  
  sorted_schools <- pairsdata %>%
    arrange(schoolid, popid)
  output <- data.frame(popid=as.numeric(), schoolid=as.numeric(), distance=as.numeric())
  
  for (i in unique(pairsdata$schoolid)) {
    
    filtered_schools <- sorted_schools %>%
      filter(schoolid==i)
    
    temp_output <- shortestPath(costmatrix, as.matrix(filtered_schools[1,c("schoolx", "schooly")]), as.matrix(filtered_schools[,c("popx","popy")]), output="SpatialLines") %>%
      SpatialLinesLengths(longlat=TRUE)
    
    temp_output <- data.frame(popid=filtered_schools$popid, schoolid=filtered_schools$schoolid, distance=temp_output)
    
    output <- bind_rows(output, temp_output)
    
  }

  output <- output %>%
    group_by(popid) %>%
    slice_min(distance) %>%
    distinct(popid, .keep_all=TRUE)
  
  pairsdata_merge <- pairsdata %>%
    dplyr::select(-distance_rough, -schoolid) %>%
    distinct(popid, .keep_all=TRUE)
  
  output <- left_join(pairsdata_merge, output, by="popid") 
  
  print(paste("Time taken:", round(((proc.time() - start_time)[3])/60, 3), "minutes", sep=" "))
  
  output
  
}

# Note now that the following 2 steps are *highly* computationally costly. For reference,
# we used a Intel i5-6600k CPU @ 3.50 Ghz with 16GB of RAM, and it took approximately
# 11 hours of runtime to complete for our Guatemala analysis. Your mileage may vary
# if you have a larger country that may require more RAM to properly store all
# the results as it goes.

# Use the close_schools function to get schools across the country in most recent year
close_schools_new <- close_schools(populationdata=country_pop_new, schooldata=country_schools_new)

# Then calculate the distance to each of the closest schools, accounting for elevation
elevation_dist_new <- cost_distance(close_schools_new)

# Get it into appropriate format for mapping
elevation_dist_new <- elevation_dist_new %>%
  rename(x=popx, y=popy) %>%
  split_convert()

# Clean results to remove rounding errors
elevation_dist_new <- left_join(elevation_dist_new, country_dist_new, by="popid", suffix=c("",".y")) %>%
  mutate(distance=case_when(distance.y>distance ~ distance.y,
                            distance.y<=distance ~ distance)) %>%
  dplyr::select(-ends_with(".y"))

# Run it through the desert checker for graphing later
elevation_desert_new <- elevation_dist_new %>% 
  st_drop_geometry() %>% 
  desert_checker()

compare_elevation_deserts <- bind_rows(no_elevation=country_desert_new, elevation=elevation_desert_new, .id="elevation") %>%
  mutate(elevation=case_when(elevation=="no_elevation" ~ "Without Elevation",
                        elevation=="elevation" ~ "With Elevation"))

compare_elevation_dist <- bind_rows(no_elevation=country_dist_new, elevation=elevation_dist_new, .id="elevation") %>%
  mutate(elevation=case_when(elevation=="no_elevation" ~ "Without Elevation",
                        elevation=="elevation" ~ "With Elevation"))

# Run some basic diagnostics
elevation_diagnostics <- left_join(country_dist_new, elevation_dist_new, by="popid") %>%
  mutate(diff_distance=distance.y-distance.x,
         percent_diff=diff_distance/distance.x,
         diff_school=case_when(schoolid.x!=schoolid.y ~ 1,
                               schoolid.x==schoolid.y ~ 0)
  )

# Plot the raw difference in crow flies distance and elevation distance across population
ggplot(elevation_diagnostics) +
  theme_minimal() +
  geom_histogram(aes(x=diff_distance, weight=population.x), bins=50, fill="blue", color="blue", alpha=0.7) + 
  scale_y_continuous(labels=scales::comma) + 
  scale_x_continuous(limits=c(0,2), oob=scales::squish) + 
  labs(x="\nDifference in Crow Flies and Elevation Distances (km)", y="Total Population",
       title=paste0("Distance to Nearest School in ", countryname, ": Raw Difference in 'Crow Flies' and Elevation Pathing Distance"), subtitle=paste0("For ", schooltype, " in ", newyear))
ggsave(file.path(project_output, paste0(countryname, '_elev_comp_hist_raw.png')), width=10, height=6)

# Plot the percentage difference in crow flies distance and elevation distance across population
ggplot(elevation_diagnostics) +
  theme_minimal() +
  scale_y_continuous(labels=scales::comma) + 
  scale_x_continuous(labels=scales::percent, limits=c(0,2), oob=scales::squish) + 
  geom_histogram(aes(x=percent_diff, weight=population.x), bins=50, fill="blue", color="blue", alpha=0.7) + 
  labs(x="\nDifference in Crow Flies and Elevation Distances (%)", y="Total Population",
       title=paste0("Distance to Nearest School in ", countryname, ": Percent Difference in 'Crow Flies' and Elevation Pathing Distance"), subtitle=paste0("For ", schooltype, " in ", newyear))
ggsave(file.path(project_output, paste0(countryname, '_elev_comp_hist_perc.png')), width=10, height=6)

# Show a scatterplot that shows the differences between crow flies distance and elevation distance
ggplot(elevation_diagnostics) +
  theme_minimal() +
  stat_summary_2d(fun=function(x) sum(x), binwidth=c(.5,.5), aes(x=distance.x, y=distance.y, z=population.x)) + 
  geom_abline(intercept = 0, slope = 1, color="red") + 
  scale_fill_viridis_c(option="plasma", limits=c(1,100000), oob=scales::squish, breaks=c(25000, 50000, 75000, 100000), labels=c("25000", "50000", "75000", "100000+")) + 
  labs(x="\nDistance as the Crow Flies (km)", y="Distance Accounting for Elevation (km)", fill="Population",
       title=paste0("Distance to Nearest School in ", countryname, ": Comparing 'Crow Flies' Distance to Elevation Pathing Distance"), subtitle=paste0("For ", schooltype, " in ", newyear))
ggsave(file.path(project_output, paste0(countryname, '_elev_comp_scatter.png')), width=10, height=6)


# Create charts showing the proportion of population living in a desert at varying thresholds, with and without elevation
ggplot(compare_elevation_deserts) +
  geom_line(aes(x=distance, y=desert_prop, color=elevation)) +
  theme_minimal() + 
  theme(axis.text = element_text(size=12), legend.text = element_text(size=12), legend.title = element_text(size=12)) + 
  labs(x="\nDistance Threshold in Kilometers", y="Proportion of Population Living in a Desert", color="Pathing Method", 
       title=paste0("Proportion of ", countryname, " Population Living in Education Desert at Varying Distance Thresholds"), subtitle=paste0("For ", schooltype, " in ", newyear, " Accounting for Elevation"))
ggsave(file.path(project_output, paste0(countryname, '_elev_threshold.png')), width=10, height=6)

# Create a chart showing the distribution of distances to school across country population
ggplot(elevation_dist_new) +
  geom_density(aes(x=distance, weight=population), adjust=1/4) + 
  theme_minimal() + 
  scale_x_continuous(limits=c(0,10), oob=scales::squish) + 
  theme(axis.text = element_text(size=12), legend.text = element_text(size=12), legend.title = element_text(size=12)) + 
  labs(x="\nDistance to Nearest School (km)", y="Density", 
       title=paste0("Distribution of Distance to Nearest School Across ", countryname, " Population"), subtitle=paste0("For ", schooltype, " in ", newyear, " Accounting for Elevation"))
ggsave(file.path(project_output, paste0(countryname, '_elev_hist.png')), width=10, height=6)


ggplot(compare_elevation_dist) +
  geom_density(aes(x=distance, weight=population, color=elevation), adjust=1/4) + 
  theme_minimal() + 
  scale_x_continuous(limits=c(0,10), oob=scales::squish) + 
  theme(axis.text = element_text(size=12), legend.text = element_text(size=12), legend.title = element_text(size=12)) + 
  labs(x="\nDistance to Nearest School (km)", y="Density",  color="Pathing Methods",
       title=paste0("Distribution of Distance to Nearest School Across ", countryname, " Population"), subtitle=paste0("For ", schooltype, " in ", newyear, " With and Without Elevation"))
ggsave(file.path(project_output, paste0(countryname, '_elev_hist_comp.png')), width=10, height=6)

# Create a map showing the distance to school across country
ggplot() + 
  theme_minimal() +
  geom_sf(data=elevation_dist_new, aes(geometry = geometry, color=distance, stroke=0), size=0.7, shape=15) + 
  geom_sf(data=country_outline, size=1, color="#1c1c1c", fill=NA) + 
  scale_color_viridis_c(option="plasma", limits=c(0,15), oob=scales::squish, breaks=c(0,5,10,15), labels=c("0", "5", "10", "15+")) + 
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  guides(shape=FALSE) + 
  labs(color="Distance in km \n", 
       title=paste0("Distance to School Across ", countryname), subtitle=paste0("For ", schooltype, " in ", newyear, " Accounting for Elevation"))
ggsave(file.path(project_output, paste0(countryname, '_elev_map_new.png')), width=10, height=6)


