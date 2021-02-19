################################################################################
#  
#   Name: 05_threshold.R
#   
#   Purpose: 
#	  Set a specific threshold distance for an education desert and conduct basic
#	  analyses of the population given that
#
################################################################################

# Filter out any empty or non-desert population cells
pop_desert_new <- country_dist_new %>%
  filter(distance>distancethreshold & population>0)

# Set up clean plot labels for later
plottitle_2d <- paste0("Geographic Distribution of ", countryname, " Population At Least ", distancethreshold, "km Away from a School")
plottitle_3d <- paste0(plottitle_2d, " \nFor ", schooltype, " in ", newyear)

# Plot out map, showing population as color
pop_desert_plot <- ggplot() + 
  theme_minimal() +
  geom_sf(data=outline_3d, color=NA, fill="#1c1c1c") + 
  geom_sf(data=pop_desert_new, aes(geometry = geometry, color=population, stroke=0), size=0.7, shape=15) +
  scale_color_viridis_c(option="plasma", limits=c(1,500), oob=scales::squish, breaks=c(1, 100, 200, 300, 400, 500), labels=c("1", "100", "200", "300", "400", "500+")) + 
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  guides(shape=FALSE) + 
  labs(color="Population Per \nSquare Kilometer \n\n\n")

# Make a 3D version of this chart
plot_gg(pop_desert_plot, height=6, width=10, height_aes="color", windowsize=c(1400,800))

Sys.sleep(5)

# Set up and save the 3D version
render_camera(theta=27.63, phi=31.36, zoom=0.4, fov=30.0)

Sys.sleep(5)

render_snapshot(filename=file.path(project_output, paste0(countryname, '_pop_', newyear, '_3d.png')),
  title_text = plottitle_3d,
                title_color = "black", title_bar_color = "white", title_size=20,
                vignette = FALSE,
                title_font = "Helvetica", gravity = "North")

Sys.sleep(5)

rgl::rgl.close()

# Now print the 2D version of it
pop_desert_plot + 
  labs(title=plottitle_2d, subtitle=paste0("For ", schooltype, " in ", newyear), color="Population Per \nSquare Kilometer \n")
ggsave(file.path(project_output, paste0(countryname, '_pop_', newyear, '.png')), width=10, height=6)

