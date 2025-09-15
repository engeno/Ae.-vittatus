#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Spatial thinning
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# working directory
setwd("")

# Loading packages
library(spThin)

# Reading occurrence data - this is the output from the previous step (moving of occurrence points)  
vits <- read.csv(".../Data/vittatus_moved.csv.csv")

# Spatial thinning
occt <- thin(loc.data = vits, lat.col = "Latitude", long.col = "Longitude",   
             spec.col = "Species", thin.par = 5, reps = 5,  # thin.par = 5; Assuming 5km thinning distance
             locs.thinned.list.return = FALSE, write.files = TRUE,
             max.files = 1, out.dir = "", # your output directory
             out.base = "vittatus_occ_thinned")
