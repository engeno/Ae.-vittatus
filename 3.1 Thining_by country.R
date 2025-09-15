#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# thinning data by density of points in a country
# points per country determined by estimates obtained from calculating number of occurrences per square miles in a country
# refer to excel sheet ".../Data/Aggregated_and_clean/Working data/Occurance per country area in square miles" for the estimates
# ten random subsamples generated 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get subsamples of occurance points from countries as defined based on densities of points 

# packages
library(rgdal)
library(spocc)
library(rgbif)
library(maps)
library(ellipsenm)
library(raster)
library(sp)
library(dplyr)
library(purrr)
library(terra)

# directory
setwd(".../Data")


# Thinning by country density of occurrence points

##Set seed
set.seed(111)

# Load shapefile - ESRI UIA_World Countries Boundaries 2023. 
wrld <- readOGR("World_Countries_Generalized/World_Countries_Generalized.shp")
plot(wrld$geometry)

# Load and convert CSV to SpatialPointsDataFrame
sf_sp <- read.csv("Aggregated_and_clean/Working data/22112023_Spatialthinned_Ae.vittatus_without_centroids.csv")
coordinates(sf_sp) <- ~Longitude + Latitude
crs(sf_sp) <- crs(wrld$geometry)
plot(sf_sp, add = TRUE, pch=21)

# Extract country attributes
sf_df <- data.frame(sf_sp, over(sf_sp, wrld$geometry))
sf_df$COUNTRY <- as.character(sf_df$COUNTRY)

# Split data by country
cto_sampleL <- split(sf_df, sf_df$COUNTRY, drop = TRUE)

# Define sample sizes per country
sample_sizes <- c(
  "Congo, Democratic Republic of" = 2, "India" = 3,"Pakistan" = 2, "Mali" = 2,
  "Niger" = 3,"Sudan" = 6,"South Africa" = 4,"Mozambique" = 3,"Cameroon" = 2,
  "Ivory Coast" = 2,"Burkina Faso" = 2,"Ghana" = 2,"Senegal" = 2,"Spain" = 4,
  "Madagascar" = 5,"Central African Republic" = 5,"Uganda" = 2,"Kenya" = 5,
  "Nigeria" = 8,"Benin" = 1,"Liberia" = 1,"Sri Lanka" = 1,"Thailand" = 4,"Comoros" = 1)


# Sampling loop and saving each iteration
for (i in 1:10) {
  samp_countries <- map_df(names(cto_sampleL), function(country) {
    df <- cto_sampleL[[country]]
    n <- sample_sizes[country]
    if (!is.na(n)) {
      slice_sample(df, n = n, replace = FALSE)
    } else {
      df
    }
  })
  
  # Save each sampled dataset
  write.csv(samp_countries, paste0("A_vittatus_country_2.5_sample_", i, ".csv"), row.names = FALSE)
}