#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Download occurrence data from GBIF
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Required packages
library(rgbif)

# define working directory
setwd("")

# Get GBIF keys for taxon of interest 
spKey <- name_backbone("Aedes vittatus")$usageKey  

# downloading taxon and retrieve a download key.
gbif_download_key = occ_download(
                    pred("taxonKey", spKey),
                    pred("hasCoordinate", TRUE),
                    pred("hasGeospatialIssue", FALSE),
                    format = "SIMPLE_CSV",
  
  # Specify your GBIF data download user details.
  user  = "",  #add your GBIF user name
  pwd   = "",  #add your GBIF password
  email = ""   #add your GBIF email
  
)

# Check download status
occ_download_wait(gbif_download_key)

data_download <- occ_download_get(gbif_download_key, overwrite = T) %>% occ_download_import()

# write out as a .csv 
vit <- unique(data_download$species)
vit <- for (i in 1:length(vit)){
    fila <- data_download[data_download$species == vit[i], ]
  write.csv(fila, paste0(vit[i], ".csv"), row.names = F)
}