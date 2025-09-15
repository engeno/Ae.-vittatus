#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#M simulation of Aedes vittatus in half africa
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# M represents the areas explored by the species of interest throughout its 
# recent history. To delineate M, we employed a simulation approach that accounts 
# for processes of dispersal, colonization, and extinction in the current climate scenario.

#Load packages
library(grinnell)
library(dplyr)
library(terra)
library(rworldmap)
library(rnaturalearth)
library(pbapply)
library(parallel)

# define working directory
setwd("")

#Create directory to save M simulations
dir.create("5.M_simulations_new1", recursive = T)


#Import occurrences 
occ <- read.csv("..../A_vittatus_country_2.5_05.csv")
occ <- occ[, 1:3]
head(occ)

#Import the climatic variables 
var <- rast(list.files(path = "...PCA/variables/",
                  pattern = ".tif$", full.names = TRUE))
#Plot 
plot(var[[1:4]])
names(var)

#Get polygon of world (only to plot)]
world <- rnaturalearth::ne_countries() %>% vect()

#load the half world shapefile
half_world <- vect("Data/Shapefile/half_world2.shp")
plot(half_world)

#mask variables to half world
var_as <- crop(var, half_world, mask = TRUE)
plot(var_as[[1:4]])

#extract environmental values
occ_as <- terra::extract(var_as[[1]],
                         occ[, c("Longitude", "Latitude")], ID = FALSE)
#Remove NAs
occ_as <- occ[which(!is.na(occ_as[, 1])), ]

#Convert dataframe to spatial points
pts_thin <- vect(occ_as, geom = c("Longitude", "Latitude"), crs = crs(var_as))


#create kernel and dispersal matrix
ks <- seq(1, 8, by = 1) #Kernel spread
dispersal.events = c(30, 60, 120, 180, 240) #Dispersal events in the last 60 years 

df_comb <- expand.grid(ks = ks,
                       dispersal.events = dispersal.events)


#Create results directories
sp.path <- file.path("5.M_simulations_new1/Aedes_vittatu_2.5m/Subset5")
dir.create(sp.path, recursive = T)
dir.create(file.path(sp.path, "M"))
dir.create(file.path(sp.path, "Plot_maps"))
dir.create(file.path(sp.path, "Simulations_details"))

#Wrap spatial variables for parallel processing
world_wrap <- wrap(world)
half_world_wrap <- wrap(half_world)
pts_thin_wrap <- wrap(pts_thin)
sr_var <- wrap(var_as)

#get the cores
parallel::detectCores()
cl <- makeCluster(2)

#Get necessary objects and send to nodes
clusterExport(cl, varlist= c("occ_as", "df_comb", "var_as", "sp.path",
                             "world_wrap", "half_world_wrap", "pts_thin_wrap", "sr_var"),
              envir=environment())

#when using cluster computing, Send necessary package to nodes
clusterEvalQ(cl, {
  library(terra)
  library(grinnell)
  library(dplyr)
})


#Looping through dispersal capabilities
pblapply(1:nrow(df_comb), function(z){
  #Get the combination of parameters z
  df_comb_i <- df_comb[z,]
  ks <- df_comb_i$ks
  de <- df_comb_i$dispersal.events

  #Create temporary directory to save
  temp_sp <- file.path(sp.path, z)
  dir.create(temp_sp)

  #Unwrap spatial variables
  world <- unwrap(world_wrap)
  half_world <- unwrap(half_world_wrap)
  pts_thin <- unwrap(pts_thin_wrap)
  sr_var_init <- unwrap(sr_var)

  #M_simulationR
  m <- try(M_simulationR(data = occ_as, #OCCURRENCES points
                         current_variables = sr_var_init,
                         starting_proportion = 0.9,
                         sampling_rule = "random",
                         barriers = NULL,
                         scale = TRUE,
                         center = TRUE,
                         project = FALSE,
                         projection_variables = NULL,
                         dispersal_kernel = "normal",
                         kernel_spread = ks, #kernel spread
                         max_dispersers = 5,
                         suitability_threshold = 5, #Ellipsoid threshold
                         replicates = 10,
                         dispersal_events = de, #Dispersal event
                         access_threshold = 5,
                         out_format = "GTiff",
                         set_seed = 42,
                         write_all_scenarios = F,
                         output_directory = temp_sp)
  )

  #Delete temporary folder
  unlink(temp_sp, recursive = TRUE, force = TRUE)

  #Check if all points fall inside M
  if(class(m) == "list") {
    #Disaggregate M
    new_m <- disagg(m$A_polygon)
    new_m$binary <- 1:length(new_m$binary)

    #See where the occurrences falling
    occ_m <- terra::extract(new_m, occ_as[,2:3])
    occ_m <- cbind(occ_as, "feature" = occ_m$binary)

    #Remove features without occurrences
    m_final <- subset(new_m, new_m$binary %in% unique(occ_m$feature))
    crs(m_final) <- "+init=epsg:4326"

    #Calculate number of independent Ms
    n_pol <- length(m_final)
    #Are there points outside the M?
    n_occ_outside <- sum(is.na(occ_m$feature))

    ####Save results to check####
    writeVector(m_final, filename = paste0(sp.path,
                                             "/M/M_", z, ".gpkg"),
                  overwrite = TRUE)

    #Save report in a dataframe
    sp.info <- data.frame("Combination" = z,
                          "species" = "Aedes_vittatus",
                          "N_records" = nrow(occ_as),
                          "Outside_M" =  n_occ_outside,
                          "N-polygons" = n_pol,
                          "kernel_spread" = ks,
                          "suitability_threshold" = 5,
                          "Dispersal_events" = de,
                          "replicates" = 10,
                          "access_threshold"= 5,
                          "Projection" = "Only_current")
    #Write dataframe
    write.csv(sp.info, paste0(sp.path,
                              "/Simulations_details/Model_info_",
                              z, ".csv"),
              row.names = F)

    #Plot map
    #Get legend
    leg <- paste0("Kernel spread = ", ks, "; Dispersal events = ", de,
                  "; Ms = ", n_pol, "; occ outside = ", n_occ_outside)

    #Plot
    png(filename = paste0(sp.path, "/Plot_maps/", z, ".png"),
        width = 8, height = 5, units = "in", res = 600)
    plot(world, 
         #xlim=c(-10, 70), ylim = c(35, 81.5), 
         col = "gray", main = leg, cex = 0.75)
    plot(half_world, 
         #xlim=c(-10, 70), ylim = c(35, 81.5), 
         col = "gray50", add = TRUE)
    plot(m_final, col = "red", add = TRUE, alpha = 0.4)
    plot(pts_thin, col = "black", add =TRUE, pch = 21, bg = "green", cex = 0.55)
    dev.off()
  }
  }, cl = cl)
parallel::stopCluster(cl)

