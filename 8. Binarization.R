# Binarization
library(terra)

# setting working directory 
## maps to where all data subset outputs of KUENM are saved (i.e.sample_1, sample_2, ... sample_10)

setwd("")

all_dirs <- dir(pattern="sample_", full.names = TRUE)

# Initialize an empty list to store the grouped directories
grouped_folders_list <- list()

# Loop through each directory and group by sample
for (dir in all_dirs) {
    sample_name <- basename(dir)  # Extract the sample name from the directory path
    path <- file.path(dir, "Final_models")
    folders <- list.dirs(path, full.names = TRUE, recursive = FALSE)
    sub_folders <- folders[grepl("^M_", basename(folders))]
  
  # Add the sub_folders to the corresponding sample in the list
    grouped_folders_list[[sample_name]] <- sub_folders
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create directory where files will be saved
dir.create('sample_Pred_files_allOcc_g')

# Create subdirectories for different extrapolotation types (E, EC, and NE) 
categories <- c("E", "EC", "NE")
for (cat in categories) {
  dir.create(file.path("sample_Pred_files_allOcc_g", cat))
}

# Process, sought and save files into corresponding projection categories and data subset
# load the function
source("files_processing_function.R")
output_base_path <- ".../sample_Pred_files_allOcc_g"
process_files(grouped_folders_list, categories, base_path,  file.path(output_base_path, "E"),  target_cat = 1)
process_files(grouped_folders_list, categories, base_path,  file.path(output_base_path, "EC"), target_cat = 2)
process_files(grouped_folders_list, categories, base_path,  file.path(output_base_path, "NE"), target_cat = 3)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Generate median for each extrapolation and data subset
# set your projection folder
cat = "EC"

for (j in 1:10) {
  # Generate the path for each sample directory
  path <- paste0("sample_Pred_files_allOcc_g/",cat,"/sample_", j, "_new")
  
  # List all CSV files in the directory
  subset <- list.files(path = path, pattern = ".csv$", full.names = TRUE)
  
  # Initialize a list to store the Cloglog.prediction values
  s <- list()
  
  # Loop through each file,pull prediction values and sort in a descending order
  for (i in 1:length(subset)) {
    occ <- read.csv(subset[i])
    s[[i]] <- sort(occ$Cloglog.prediction, decreasing = TRUE)
  }
  
  # Create a dataframe from the list and calculate the median
  all <- as.data.frame(do.call(cbind, s))
  all$median <- apply(all, 1, median)
  
  # Write the result to a CSV file
  write.csv(all, paste0(path, "/median.csv"), row.names = FALSE)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# get the threshold and predict to M
## Initialize an empty vector to store thresholds
thrs_EC <- numeric(10)

for (k in 1:10) {
  # Generate the path for each sample directory
  path   <- paste0("sample_Pred_files_allOcc_g/",cat,"/sample_", k, "_new")
  layer  <- rast(paste0("sample_",k,"_new","/Final_model_stats/Statistics_E/current_med.tif"))
  all    <- read.csv(paste0(path, "/median.csv"))
  o_suit <- all$median
  o_suit_sort <- sort(o_suit)
  threshold <- 5
  thres <- o_suit_sort[ceiling(length(all[, 1]) * threshold / 100) + 1]
  
  # Store the threshold in the thrs vector
  thrs_EC[k] <- thres[]

# Binarization
  blayer <- (layer >= thres) * 1

  writeRaster(blayer, paste0(path,"/subset2_",k,".tif"), filetype = "GTiff", overwrite = TRUE)
}


# Use the threshold and predict to Americas
for (k in 1:10) {
  # Generate the path for each sample directory
  path_p   <- paste0("sample_Pred_files_allOcc_g/",cat,"/sample_", k, "_new")
  layer_p  <- rast(paste0("sample_",k,"_new","/Final_model_stats/Statistics_E/current_med.tif"))
  
  # Binarization
  player <- (layer_p >= thres) * 1
  
  writeRaster(player, paste0(path_p,"/p_subset_",k,".tif"), filetype = "GTiff", overwrite = TRUE)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#E
bin1 <- rast("sample_Pred_files_allOcc_g/E/sample_1/subset2_1.tif")
bin2 <- rast("sample_Pred_files_allOcc_g/E/sample_2/subset2_2.tif")
bin3 <- rast("sample_Pred_files_allOcc_g/E/sample_3/subset2_3.tif")
bin4 <- rast("sample_Pred_files_allOcc_g/E/sample_4/subset2_4.tif")
bin5 <- rast("sample_Pred_files_allOcc_g/E/sample_5/subset2_5.tif")
bin6 <- rast("sample_Pred_files_allOcc_g/E/sample_6/subset2_6.tif")
bin7 <- rast("sample_Pred_files_allOcc_g/E/sample_7/subset2_7.tif")
bin8 <- rast("sample_Pred_files_allOcc_g/E/sample_8/subset2_8.tif")
bin9 <- rast("sample_Pred_files_allOcc_g/E/sample_9/subset2_9.tif")
bin10 <- rast("sample_Pred_files_allOcc_g/E/sample_10/subset2_10.tif")


#E_p
bin1_p <- rast("sample_Pred_files_allOcc_g/E/sample_1/p_subset_1.tif")
bin2_p <- rast("sample_Pred_files_allOcc_g/E/sample_2/p_subset_2.tif")
bin3_p <- rast("sample_Pred_files_allOcc_g/E/sample_3/p_subset_3.tif")
bin4_p <- rast("sample_Pred_files_allOcc_g/E/sample_4/p_subset_4.tif")
bin5_p <- rast("sample_Pred_files_allOcc_g/E/sample_5/p_subset_5.tif")
bin6_p <- rast("sample_Pred_files_allOcc_g/E/sample_6/p_subset_6.tif")
bin7_p <- rast("sample_Pred_files_allOcc_g/E/sample_7/p_subset_7.tif")
bin8_p <- rast("sample_Pred_files_allOcc_g/E/sample_8/p_subset_8.tif")
bin9_p <- rast("sample_Pred_files_allOcc_g/E/sample_9/p_subset_9.tif")
bin10_p <- rast("sample_Pred_files_allOcc_g/E/sample_10/p_subset_10.tif")

## all layers
all_bins <- bin1 + bin2 +bin3 + bin4 + bin5 + bin6 + bin7 + bin8 + bin9 + bin10
plot(all_bins)

writeRaster(all_bins, filename = "sample_Pred_files_allOcc_g/Binary/E/all_bins2.tif", 
            filetype = "GTiff", overwrite = TRUE)

## Excluding layer 3
nine_bins2 <- bin1 + bin2 + bin4 + bin5 + bin6 + bin7 + bin8 + bin9 + bin10
plot(nine_bins2)

writeRaster(nine_bins2, filename = "sample_Pred_files_allOcc_g/Binary/E/nine_bins2.tif", 
            filetype = "GTiff", overwrite = TRUE)

## projection
all_bins_p <- bin1_p + bin2_p + bin4_p + bin5_p + bin6_p + bin7_p + bin8_p + bin9_p + bin10_p
plot(all_bins_p)

writeRaster(all_bins_p, filename = "sample_Pred_files_allOcc_g/Binary/E/nine_bins2_p.tif", 
            filetype = "GTiff", overwrite = TRUE)
