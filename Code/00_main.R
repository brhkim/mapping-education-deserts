################################################################################
#  
#   Name: 00_main.R
#   
#   Purpose: 
#   Conduct the Education Deserts analysis by Daniel Rodriguez-Segura
#   and Brian Heseung Kim using R.
#
#   Instructions:
#   To properly use this codebase, you will need to take the following steps:
#   1.) Install and load the packages indicated under #setup based on whether you
#       want to run only the main analysis, or the extended analyses as well
#   2.) Adjust all the filepaths under #setup to your chosen work directory
#   3.) Adjust all the options and individual file paths under #specifications to your context
#   4.) Run each script in order under #mainscripts, in order
#   5.) If desired, run supplemental analyses under #extscripts. These require all 
#       #mainscripts to be run first
#
#   If you set all of #setup and #specifications, you should be able to run the
#   entirety of this script in one go!
# 
#   Please don't hesitate to reach out to us if you require any support! We'd love
#   to be helpful to any interested. For technical support, contact  @brhkim on 
#   Twitter/Github, or bhk5fs@virginia.edu for email.
#
################################################################################

################################################################################
#
#   #setup - Setup file and check dependencies
#
################################################################################

# Clean things up first
rm(list = ls())

# Load required packages for the main analysis; you must install them each beforehand 
# using install.packages("tidyverse") (as an example) prior to the sapply "require" 
# call below
libs <- c('tidyverse', 'sf', 'haven', 'raster', 'units', 'rgdal')
sapply(libs, require, character.only = TRUE)
# 'rgdal', 'plotly', 'RSQLite', 'lwgeom'

# The following packages are *only required* if you wish to run the #extscripts
# that extend our main analysis.
# Note that installing rayshader requires XQuartz to be installed for OSX users
# *prior to instalation of rayshader*, and it must be run prior to any runs of
# these scripts to generate the 3D plots properly
libs2 <- c('gdistance', 'rayshader', 'rgl', 'magick')
sapply(libs2, require, character.only = TRUE)

# Set up file pathing
setwd(file.path('..'))

project_data <- file.path('Data')
  # Set this director to the project data folder, relative to the project root folder.
  # Note that all filepaths for this codebase are referenced below in #specifications
  # and are relative to the project root folder

project_code <- file.path('Code')
  # Set this director to the project code folder, relative to the project root folder.
  # All code needs to be in this folder, including this present script.

project_output <- file.path('Output')
  # Set this director to the project output folder, relative to the project root folder.
  # This is where all plots will be saved to by default.


################################################################################
#
#   #specifications - Set up specific file paths and analytic options for the rest of the analysis
#
################################################################################

countryname <- "Guatemala"
  # Write in the properly capitalized name; this is how it will appear in all plots
  # This will also be used as a prefix to all saved outputs (plots, datasets, etc.)

schooltype <- "Public Primary Schools"
  # Write in a descriptive explanation of the school sample; this will appear in all plots as a subtitle

newyear <- 2017
  # Write in the year of data for your main analysis, should be written as a number (not in quotes)
  # This will also appear in all appropriate plots

country_schools_new_path <- file.path(project_data, 'Guatemala data', 'Guatemala schools', 'Clean reconciled data', 'Clean_GT_2017.csv')
  # Write in the filepath to the schools location dataset; we assume this is in csv format
  # Coordinates must be stored in separate columns as "longitude" and "latitude"
  # Note that this file can really represent *any* public good/service of choice (e.g. supermarkets, clinics, etc.) 
  # as long as each instance has a coordinate attached. Just note that all graph titles in the ensuing code will need
  # to be adjusted if you are analyzing something besides education deserts/schools

country_pop_new_path <- file.path(project_data, 'Guatemala data', 'Guatemala population', 'Resolution 1km', 'gtm_ppp_2017_1km_Aggregated.tif')
  # Write in the filepath to the population data; we assume this is structured and formatted as a raster at 1km resolution in the "top-down, unconstrained" .tif format provided by worldpop.org
  # Relies on data structure as 3 columns, in order: "x" for longitude, "y" for latitude, and a population value column.

country_shape_path <- file.path(project_data, 'Guatemala data', 'Additional data', 'border_gtm', 'gtm_admbnda_adm0_ocha_conred_20190207.shp')
  # Write in the filepath to the shapefile of the country itself; will be translated into a border for plots

multiyear <- TRUE
  # Set as TRUE or FALSE (not in quotes) depending on whether you wish to compare and contrast two separate years of schools and population data
  # None of settings in the if statement immediately following this comment need to be set if you set multiyear==FALSE

  if(multiyear==TRUE) {
    oldyear <- 2008
      # If conducting a multiyear analysis, write in the year of data for your comparison year, should be written as a number (not in quotes)
      # This will appear in all plots as appropriate
    
    country_schools_old_path <- file.path(project_data, 'Guatemala data', 'Guatemala schools', 'Clean reconciled data', 'Clean_GT_2008.csv')
      # Write in the filepath to the older schools location dataset; we assume this is in csv format
      # Coordinates must be stored in separate columns as "longitude" and "latitude"
    
    country_pop_old_path <- file.path(project_data, 'Guatemala data', 'Guatemala population', 'Resolution 1km', 'gtm_ppp_2008_1km_Aggregated.tif')
      # Write in the filepath to the older year's population data; we assume this is structured and formatted as a raster at 1km resolution in the "top-down, unconstrained" format provided by worldpop.org
      # Relies on data structure as 3 columns, in order: "x" for longitude, "y" for latitude, and a population value column.
    
    unique_school_identifier <- "school_code"
      # The simulation analysis requires a unique identifier that matches the old schools data to new schools data
      # to identify which schools closed over the time period. Input the name of that unique identifier column here.
  }

enrollmentanalysis <- TRUE
  # Set as TRUE or FALSE (not in quotes) depending on whether you wish to compare and contrast the distance-to-school measures against regional enrollment rates
  # None of settings in the if statement immediately following this comment need to be set if you set enrollmentanalysis==FALSE

  if(enrollmentanalysis==TRUE) {
    country_regions_path <- file.path(project_data, 'Guatemala data', 'Additional data', 'departamentos_gtm', 'departamentos_gtm.shp')
      # If conducting enrollment analysis, write in the filepath to shapefiles for the regional units you're examining
    
    country_enrollment_path <- file.path(project_data, 'Guatemala data', 'Additional data', 'Enrollment_Depts_Guatemala_2016.csv')
      # Write in the file path to the enrollment statistics dataset; we assume this is in .csv format. These should be saved at the same unit of analysis as your regions shapefile

    enrollment_merge_var <- "nombre"
    # Provide the name of the variable to merge the regions and enrollment data on; must be included in both data files
    
    enrollment_value_var <- "enrollment_net"
    # Provide the name of the variable that indicates percent enrollment in each region
    
    enrollmentyear <- 2016
    # Indicate the year of data for enrollment data, if different than newyear
  }

elevationanalysis <- TRUE
  # Set as TRUE or FALSE (not in quotes) depending on whether you wish to run the supplemental analysis that takes into account geographic elevation in its pathing algorithm
  # None of settings in the if statement immediately following this comment need to be set if you set elevationanalysis==FALSE

  if(elevationanalysis==TRUE) {
    country_elevation_path <- file.path(project_data, 'Guatemala data', 'Additional data', 'gtm_srtm_topo_100m.tif')
      # Write in the filepath for the elevation raster data file (.tif format) to be used in the elevation analysis

    elevationscaler <- 5
      # We assume the elevation data is in 100m resolution and scale it up to a 500m resolution for computational speed using a default elevationscaler value of 5
      # This setting can be adjusted, but note that the final elevation data resolution must be finer-grain resolution than the population data,
      # and it must be evenly divisible into the population data resolution. Otherwise, this analysis will produce gibberish output because of the way the pathing algorithm works.
  }

distancethreshold <- 3
  # Set the specific distance threshold for desert definitions, in km, used in the 05_threshold.R script


################################################################################
#
#   #mainscripts - Call individual scripts to complete and visualize main analysis
#
################################################################################

# Main analysis; scripts are dependent on all prior scripts in order

# 01_import.R processes and loads each dataset as appropriate; any data structure and incompatibility
# issues will likely be revealed in this file.
source(file.path(project_code, '01_import.R'))
  datafiles <- c("country_pop_new_raster", "country_outline", "outline_3d")
    # This just prepares a running list of objects to be saved at the end of this script sequence
  
  if(enrollmentanalysis==TRUE) {
    datafiles <- c(datafiles, "country_regions")
  }
  
  if(elevationanalysis==TRUE) {
    datafiles <- c(datafiles, "country_elevation")
  }
  
# 02_preprocess.R processes the schools and population data into an appropriate format for distance analysis
source(file.path(project_code, '02_preprocess.R'))
  datafiles <- c(datafiles, "split_convert", "country_schools_new", "country_pop_new")
  if(multiyear==TRUE){
    datafiles <- c(datafiles, "country_schools_old", "country_pop_old")
  }
  
# 03_distance.R analyzes the school and population data for distance between pop point and nearest school
source(file.path(project_code, '03_distance.R'))
  datafiles <- c(datafiles, "closest_school", "country_dist_new")
  if(multiyear==TRUE){
    datafiles <- c(datafiles, "country_dist_old")
  }
  
# 04_desert.R conducts visualization exercises on the output of the distance analysis
source(file.path(project_code, '04_desert.R'))
  datafiles <- c(datafiles, "desert_checker", "country_desert_new")
  if(multiyear==TRUE){
    datafiles <- c(datafiles, "country_desert_old", "compare_deserts", "compare_dist")
  }
  
# Save checkpoint for all main analytic files
base::save(list=datafiles, file=file.path(project_data, paste0(countryname, '_tempdata1.RData')))


################################################################################
#
#   #extscripts - Call individual scripts to complete and visualize extension analyses
#
################################################################################

base::load(file=file.path(project_data, paste0(countryname, '_tempdata1.RData')))

# Supplementary/extension analyses; these require main analyses to be run first
source(file.path(project_code, '05_threshold.R'))
  datafiles2 <- c("pop_desert_new")

if(enrollmentanalysis==TRUE) {
  source(file.path(project_code, '06_enrollment.R'))
    datafiles2 <- c(datafiles2, "enrollment_scatter_data")
}

# Simulation analysis can only be run if the analysis is multiyear and after 05_threshold.R has been run
if(multiyear==TRUE) {
  source(file.path(project_code, '07_simulation.R'))
    datafiles2 <- c(datafiles2, "school_mapper_output", "unclosed_schools", "new_school_count",
                    "pop_desert_old2", "hypo_schools_new", "hypo_dist_new", "hypo_desert_new",
                    "hypo_dist_new2", "hypo_desert_new2", "construction_table", "compare_hypo", "pop_hypo_desert_new")
}
  
# Elevation analysis can only be run after 05_threshold.R
if(elevationanalysis==TRUE) {
  source(file.path(project_code, '08_elevation.R'))
    datafiles2 <- c(datafiles2, "elevation_dist_new", "elevation_desert_new",
                    "compare_elevation_deserts", "compare_elevation_dist",
                    "elevation_diagnostics")
}

# Save checkpoint
base::save(list=datafiles2, file=file.path(project_data, paste0(countryname, '_tempdata2.RData')))
base::load(file=file.path(project_data, paste0(countryname, '_tempdata2.RData')))

