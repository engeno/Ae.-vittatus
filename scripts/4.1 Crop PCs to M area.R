#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Crop PCs to M area and convert them to .ascii file
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#define working directory
setwd(".../Data")

# required package
library(terra)

# path to the PCs
dir_rasters <- ".../PCA/pcs_mosquito_part_world"

# Bring the shapefile- includes newly invated areas
shape <- vect(".../Shapefile/M_shapefile/Mosquito_m.shp")
plot(shape)

# load the PC files
rasters <- list.files(dir_rasters, pattern=".tif$", full.names=TRUE)
rasters

# set raster and shapefile to same projection
## PCs layers
raster1 <-  rast(rasters)
crs(raster1, describe=TRUE, proj=TRUE)

## M shapefile
crs(shape, describe=TRUE, proj=TRUE)

## project the m-shapefile to WGS84
crs(shape) <- "+proj=longlat +datum=WGS84"
crs(shape, proj=TRUE)

# Crop in a loop
for (i in rasters) {
  # convert all PCs to raster
  raster <- rast(i)
  
  # New Raster name
  cropped <- paste0(tools::file_path_sans_ext(basename(i)), "_cropped.tif")
  
  # Cut the rasters using the M shapefile
  cropped_rasters <- crop(raster, shape, mask=TRUE)
  
  # Save the cut raster
  writeRaster(cropped_rasters, paste0(dir_rasters, cropped), overwrite=TRUE)
  
  # Print confirmation message
  cat("Cropped layer saved:", cropped, "\n")
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Convert generated .tif files to ASCII format
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(raster)

input_folder <- "PATH TO YOUR CROPPED .TIF FILE"

#output folder
output_folder <- "PATH FOR YOUR .ASCII FILE"

# creating the output folder
if (!file.exists(output_folder)){
  dir.create(output_folder)
}

# get list of the tiff files
tiff_files <-  list.files(input_folder, pattern = '.tif$', full.names = TRUE)

for (i in tiff_files){
  output_asci <- file.path(output_folder,paste0(tools::file_path_sans_ext(basename(i)), ".ascii"))
  
  r <- raster(i)
  
  writeRaster(r, filename = output_asci, format="ascii", overwrite=TRUE)
}
