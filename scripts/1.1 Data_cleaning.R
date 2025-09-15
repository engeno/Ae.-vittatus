#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# data cleaning 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# define working directory
setwd("") 

## read the .csv files  
D <- list.files(path = "PATH TO YOUR CSV FILES", pattern = ".csv$", full.names = TRUE)

## get names for saving the clean records
nam <- list.files(path = "PATH TO YOUR CSV FILES", pattern = ".csv$", full.names = FALSE)

## Create list with paths for writing each .csv
finalnam <- file.path("PATH TO YOUR CLEAN FOLDER", nam)



# Function to count decimal places
decimal_places <- function(x) {
  if (abs(x - round(x)) > .Machine$double.eps^0.5) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][2])
  } else {
    return(0)
  }
}

# Cleaning loop
for (i in seq_along(D)) {
  occurrences <- read.csv(D[i])[, c("species", "decimalLongitude", "decimalLatitude")]
  colnames(occurrences) <- c("Species", "Longitude", "Latitude")
  
  # Remove duplicates and missing values
  occurrences$code <- paste(occurrences$Species, occurrences$Longitude, occurrences$Latitude, sep = "_")
  occurrences <- occurrences[!duplicated(occurrences$code), c("Species", "Longitude", "Latitude")]
  occurrences <- na.omit(occurrences)
  
  # Remove invalid coordinates
  occurrences <- occurrences[occurrences$Longitude != 0 & occurrences$Latitude != 0, ]
  
  # Filter by precision
  occurrences <- occurrences[
    sapply(occurrences$Longitude, decimal_places)  >= 2 &
      sapply(occurrences$Latitude, decimal_places) >= 2, ]
  
  # Save cleaned data
  write.csv(occurrences, finalnam[i], row.names = FALSE)
}