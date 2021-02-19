################################################################################
#  
#   Name: 06_enrollment.R
#   
#   Purpose:
#	  Compare the enrollment rates by region to the population in a desert, using
#	  analyses from 05_threshold as a point of comparison
#
################################################################################

# Calculate the total population in each region, based on the geolocated population data
popcalc1 <- st_join(country_regions, country_pop_new) %>%
  group_by(merger = .[[enrollment_merge_var]]) %>%
  summarize(totalpop=sum(population))

# Calculate the regional population lacking access to school, based on the geolocated population data
pop_desert_new2 <- pop_desert_new %>% dplyr::select(-school_point) %>% st_set_geometry(.$geometry)
popcalc2 <- st_join(country_regions, pop_desert_new2) %>%
  group_by(merger = .[[enrollment_merge_var]]) %>%
  summarize(desertpop=sum(population))

# Join the two datasets together and calculate the proportion of regional population in a desert
enrollment_scatter_data <- left_join(st_drop_geometry(popcalc1), st_drop_geometry(popcalc2), by="merger") %>%
  mutate(desertprop = desertpop/totalpop)

# Rename the variables for ease of use
enrollment_scatter_data[[enrollment_merge_var]] <- enrollment_scatter_data$merger

# Join the enrollment data to desert proportion data
enrollment_scatter_data <- left_join(country_regions, enrollment_scatter_data, by=enrollment_merge_var)

# Rename the variables for ease of use
enrollment_scatter_data$enrollment_value <- enrollment_scatter_data[[enrollment_value_var]]

# Plot a scatterplot that shows regional enrollment rates against regional percent of population in a desert
ggplot(enrollment_scatter_data, aes(x=enrollment_value, y=desertprop*100, size=totalpop)) + 
  theme_minimal() +
  geom_point() + 
  scale_size_continuous(labels = scales::comma, range=c(1,10)) +
  theme(axis.text = element_text(size=12), legend.text = element_text(size=12), legend.title = element_text(size=12)) + 
  labs(x="\nRegional Enrollment Rate", y=paste0("Percent of Population Living at least ", distancethreshold, "km from School\n"), size="Regional Population", 
       title=paste0("Regional Enrollment Rates and Percent of Regional Population Living in Education Deserts"), subtitle=paste0("For ", schooltype))
ggsave(file.path(project_output, paste0(countryname, '_enrollment_scatter.png')), width=10, height=6)

# Plot just the enrollment rates across regions on a map
ggplot() + 
  theme_minimal() +
  geom_sf(data=outline_3d, color=NA, fill="#1c1c1c") + 
  geom_sf(data=enrollment_scatter_data, aes(fill=enrollment_value), size=1, color="#1c1c1c") + 
  scale_fill_continuous(limits=c(50,100), breaks=c(50, 60, 70, 80, 90, 100)) + 
  scale_size_continuous() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  guides(shape=FALSE) + 
  labs(fill="Regional Enrollment Rate\n", title=paste0(countryname, " Enrollment Rates by Region"), subtitle=paste0("For ", schooltype, " in ", enrollmentyear))
ggsave(file.path(project_output, paste0(countryname, '_enrollment_', enrollmentyear, '.png')), width=10, height=6)

# Now overlay the population in deserts across the country
ggplot() + 
  theme_minimal() +
  geom_sf(data=outline_3d, color=NA, fill="#1c1c1c") + 
  geom_sf(data=enrollment_scatter_data, aes(fill=enrollment_value), size=1, color="#1c1c1c") + 
  geom_sf(data=pop_desert_new2, aes(color=population, stroke=0), size=0.7, shape=15) + 
  scale_fill_continuous(limits=c(50,100), breaks=c(50, 60, 70, 80, 90, 100)) + 
  scale_color_viridis_c(option="plasma", limits=c(1,500), oob=scales::squish, breaks=c(1, 100, 200, 300, 400, 500), labels=c("1", "100", "200", "300", "400", "500+")) + 
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  guides(color=guide_colourbar(order=1), fill=guide_colourbar(order=2), shape=FALSE) + 
  labs(fill="Regional Enrollment Rate\n", color="Population per \nSquare Kilometer", title=paste0(countryname, " Enrollment Rates by Region Against Population at Least ", distancethreshold, "km Away from a School"), subtitle=paste0("For ", schooltype, " in ", enrollmentyear))
ggsave(file.path(project_output, paste0(countryname, '_enrollment_desert_', enrollmentyear, '.png')), width=10, height=6)
