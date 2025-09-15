#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Moving occurrence points to closest environmental pixel.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#packages
library(terra)

## set working directory 
setwd("")

#get function for moving the points 
source("Code/move2_closest_function.R")

#Reading occurrence data
vittatus <- read.csv("Data/Aggregated_and_clean/Working data/22112023_Spatialthinned_Ae.vittatus_without_centroids.csv")[-1]

#Read environ layer to be used as reference
layer1 <-rast("Data/Bioclim/wc2.1_2.5m_bio_1.tif")

#Applying the move_points function
data_vittatus <- move_2closest_cell(vittatus, longitude_column = "Longitude", 
                                    latitude_column = "Latitude", 
                                    raster_layer = layer1, move_limit_distance = 5)

write.csv(data_vittatus, ".../Data/vittatus_moved.csv")
