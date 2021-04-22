################################################################################
#  
#   Name: 07_simulation.R
#   
#   Purpose: 
#	  Run a simulation exercise to optimize school placements between older year and
#	  newer year, then compare resulting desert metrics to actual newer year data
#
################################################################################

check_adjacency <- function(desertdata) {

  # Initialize timer function
  start_time <- proc.time()

  # Create an empty output dataframe
  adjacency_output <- data.frame(popid=as.numeric())

  # Loop over each population point and calculate which other population cells are within the threshold distance of the given point
  for (i in 1:nrow(desertdata)) {

    closepoints <- st_is_within_distance(desertdata[i,"geometry"], desertdata[,"geometry"], dist=distancethreshold*1000, sparse=TRUE)[[1]]

    loop_output <- data.frame(popid=as.data.frame(desertdata[i,"popid"])["popid"], adjacent=I(list(closepoints)))

    # Append it to the output dataframe
    adjacency_output <- bind_rows(adjacency_output, loop_output)

    if (i %% 1000 == 0) {
      print(paste("Finished with row", i, "of", nrow(desertdata), ". Cumulative Time taken:", round(((proc.time() - start_time)[3])/60, 3), "minutes", sep=" "))
    }
    
  }
  
  # Provide the output
  adjacency_output
  
}

school_mapper <- function(desertdata, adjacency_output, iterations=100000, minreached=1) {
  
  start_time <- proc.time()
  
  # Merge the output from the loop (adjacency values) to main dataframe
  fulldata <- left_join(desertdata, adjacency_output, by="popid") 
  
  cut_obs <- c()

  # Set up an output dataframe
  best_schools <- fulldata[0,]
  
  # Loop through the number of schools to be constructed
  for (j in 1:(iterations)) {
    
    subdata <- fulldata
    subdata$population[cut_obs] <- 0
    
    # Calculate the population that would be reached if a school were built on each cell,
    # accounting for those cells already reached by prior iterations
    subdata <- subdata %>%
      rowwise() %>%
      mutate(reduction=sum( subdata$population[ adjacent[[1]] ] )) %>%
      ungroup()

    # Get the best school from this new set
    best_schools_plus <- subdata %>%
      slice_max(reduction, with_ties=FALSE)
    
    # Stop the loop if we go below the minimum number of population reached
    if((best_schools_plus$reduction) < minreached) {
      break
    }
    
    # Add it to the output
    best_schools <- bind_rows(best_schools, best_schools_plus)
    
    # Add its adjacent cells to those excluded from each loop's calculations
    cut_obs <- unique(c(cut_obs, best_schools_plus$adjacent[[1]]))
    
    if (j %% 100 == 0) {
      print(paste("Finished with iteration", j, "of", iterations, ". Cumulative Time taken:", round(((proc.time() - start_time)[3])/60, 3), "minutes", sep=" "))
    }

  }
    
  # Print output of timer function
  print(paste("Time taken:", round(((proc.time() - start_time)[3])/60, 3), "minutes", sep=" "))

  # Output each component into a list
  output <- list(school_list = best_schools, reached_pop = cut_obs, data = fulldata)
  
  output
  
}


# If all you wanted to do is map out the location of X optimally located schools given the schools
# and population for the most recent year of data, you would only need to run the following:
  # adjacency_data <- check_adjacency(desertdata = pop_desert_new2)
  # school_mapper_output <- school_mapper(desertdata = pop_desert_new2, adjacency_output = adjacency_data, iterations = X)
  # View(school_mapper_output[[1]])
# where pop_desert_new2 is constructed from 06_enrollment.R.
# and school_mapper_output[[1]] displays the location of the schools (coordinates stored as geometry)
# and the estimated number of people reached (stored as reduction) within the distancethreshold you set in 00_main.R
# Note that these scripts take quite a while to run; for Guatemala, it took about 7 hours.
# You can alternately opt to set a "minreached" argument instead of an iterations argument that stops mapping new 
# schools once they cross below a set threshold of people served. 
# For example, if all schools must have a catchment of at least 200 population, you'd run the following
# with an arbitrarily large iterations value:
  # adjacency_data <- check_adjacency(desertdata = pop_desert_new2)
  # school_mapper_output <- school_mapper(desertdata = pop_desert_new2, adjacency_output = adjacency_data, minreached=200)
  # View(school_mapper_output[[1]])
# But note that by default, it will not try to make more than 100,000 schools (arbitrarily large number) unless iterations
# is set.

# The remainder of this script runs our simulation exercise on Guatemala as per the paper.

# Get the number of new schools to create for the simulation based on which schools
# remained unclosed over the time period
unclosed_schools <- left_join(st_drop_geometry(country_schools_old), st_drop_geometry(country_schools_new), by=unique_school_identifier) %>%
  filter(!is.na(schoolid.y)) %>% 
  dplyr::select(starts_with(unique_school_identifier)) %>%
  left_join(country_schools_old, by=unique_school_identifier) %>%
  st_set_geometry(.$geometry)
  
new_school_count <- nrow(country_schools_new) - nrow(unclosed_schools)

# Make pop_desert dataset for country in older year
pop_desert_old2 <- closest_school(populationdata=country_pop_old, schooldata=unclosed_schools) %>%
  filter(distance>distancethreshold & population>0)

# Get algorithm output based on old population distribution (given only those 
# schools that did not close over time) and how many new schools were built since then
adjacency_data <- check_adjacency(desertdata = pop_desert_old2)
school_mapper_output <- school_mapper(desertdata = pop_desert_old2, adjacency_output = adjacency_data, iterations = new_school_count)

# Generate a plot to show how many people, cumulatively, are reached per newly constructed school
construction_table <- school_mapper_output[[1]] %>%
  mutate(order_num=1:n())

ggplot(construction_table) + 
  geom_step(aes(x=order_num, y=cumsum(reduction))) + 
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() + 
  labs(x="\nNumber of Optimally Located Schools Built", y="Cumulative New Population Reached", 
       title="New Population Reached Per Optimally Located School", subtitle=paste0("For Simulated ", schooltype, " in ", newyear))
ggsave(file.path(project_output, paste0(countryname, '_hypo_totpop_reached.png')), width=10, height=6)

# Analysis: Append new school points to old year's schools list
new_school_append <- school_mapper_output[[1]][1:350,] %>%
  dplyr::select(popid, geometry) %>%
  rename(schoolid=popid) %>%
  mutate(schoolid=as.numeric(paste(schoolid, "99999", sep="")))
  
hypo_schools_new <- bind_rows(unclosed_schools, new_school_append)

# Run desert analysis using newer year reality against our hypothetical
hypo_dist_new <- closest_school(populationdata=country_pop_new, schooldata=hypo_schools_new)
hypo_desert_new <- hypo_dist_new %>% desert_checker()

# Run desert analysis using only unclosed schools to compare against real and hypothetical
hypo_dist_new2 <- closest_school(populationdata=country_pop_new, schooldata=unclosed_schools)
hypo_desert_new2 <- hypo_dist_new2 %>% desert_checker()

# Create graphing datasets to compare the hypothetical data to real data in 2017
compare_hypo <- bind_rows(real=country_desert_new, hypo=hypo_desert_new, unclosed=hypo_desert_new2, .id="version") %>%
  mutate(version=case_when(version=="real" ~ "Real 2017 Schools \n(n=16,110)",
                           version=="unclosed" ~ "Unclosed Schools from \n2008 (n=9,040)",
                        version=="hypo" ~ "Unclosed + 350 Simulated \nSchools (n=9,390)")) %>%
  mutate(version=factor(version, levels=c("Unclosed Schools from \n2008 (n=9,040)",
                                          "Unclosed + 350 Simulated \nSchools (n=9,390)",
                                          "Real 2017 Schools \n(n=16,110)")))

# Create a chart to show the proportion of population in a desert at the various thresholds, across real and hypothetical data
ggplot(compare_hypo) +
  geom_line(aes(x=distance, y=desert_prop, color=version)) +
  theme_minimal() + 
  theme(axis.text = element_text(size=12), legend.text = element_text(size=12), legend.title = element_text(size=12)) + 
  labs(x="\nDistance Threshold in Kilometers", y="Proportion of Population Living in a Desert", color="School Set", 
       title=paste0("Proportion of ", countryname, " Population Living in Education Desert at Varying Distance Thresholds"), subtitle=paste0("Comparing Real and Simulated ", schooltype, " in ", newyear))
ggsave(file.path(project_output, paste0(countryname, '_hypo_threshold.png')), width=10, height=6)


# Filter out any empty or non-desert population cells
pop_hypo_desert_new <- hypo_dist_new %>%
  filter(distance>distancethreshold & population>0)

# Set up clean plot labels for later
plottitle_2d <- paste0("Geographic Distribution of ", countryname, " Population At Least ", distancethreshold, "km Away from a School")
plottitle_3d <- paste0(plottitle_2d, " \nFor Population in ", newyear, " Using Simulated School Construction")

# Plot out map, showing population as color
pop_desert_plot2 <- ggplot() + 
  theme_minimal() +
  geom_sf(data=outline_3d, color=NA, fill="#1c1c1c") + 
  geom_sf(data=pop_hypo_desert_new, aes(geometry = geometry, color=population, stroke=0), size=0.7, shape=15) +
  scale_color_viridis_c(option="plasma", limits=c(1,500), oob=scales::squish, breaks=c(1, 100, 200, 300, 400, 500), labels=c("1", "100", "200", "300", "400", "500+")) + 
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  guides(shape=FALSE) + 
  labs(color="Population Per \nSquare Kilometer \n\n\n")

# Make a 3D version of this chart
plot_gg(pop_desert_plot2, height=6, width=10, height_aes="color", windowsize=c(1400,800))

Sys.sleep(5)

# Set up and save the 3D version
render_camera(theta=27.63, phi=31.36, zoom=0.4, fov=30.0)

Sys.sleep(5)

render_snapshot(filename=file.path(project_output, paste0(countryname, '_hypo_popmap', newyear, '_3d.png')),
                title_text = plottitle_3d,
                title_color = "black", title_bar_color = "white", title_size=20,
                vignette = FALSE,
                title_font = "Helvetica", gravity = "North")

Sys.sleep(5)

rgl::rgl.close()

# Now print the 2D version of it
pop_desert_plot2 + 
  labs(title=plottitle_2d, subtitle=paste0("For Population in ", newyear, " Using Simulated School Construction"), color="Population Per \nSquare Kilometer \n")
ggsave(file.path(project_output, paste0(countryname, '_hypo_popmap', newyear, '.png')), width=10, height=6)

