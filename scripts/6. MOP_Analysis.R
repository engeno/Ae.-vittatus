#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# mop: Mobility Oriented-Parity Metric ####
#See more in: https://github.com/marlonecobos/mop
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#define working directory
setwd(".../Data/Modelling")

#Load packages
library(terra)
library(mop)
library(dplyr)
library(rnaturalearth)
library(data.table)


#Get all subsets
s <- c(paste0("subset_", 1:10),
       "All_points")
s

#Loop in subsets
lapply(s, function(x){
  message("Running mop for ", x)

  #Get path for G_variables of set x
  g_x <- paste0("G_variables/G_variables_", x)

  #Get all variables for set x
  var_x <- list.files(g_x, pattern = ".asc", recursive = TRUE, full.names = TRUE)
  var_names <- basename(var_x) %>% fs::path_ext_remove() %>% unique()

  #Create dataframe with paths
  df <- data.frame(path = var_x,
                   variable = gsub(".*current/(PC\\d+).asc$", "\\1", var_x))
  #Select only one of the sets
  df <- df %>% filter(variable %in% var_names) %>%
    distinct(variable, .keep_all = TRUE)

  #Import variables
  spat_x <- rast(lapply(1:nrow(df), function(i){
    rast(df$path[i])
  }))

  #Get path for calibration data of set x
  c_p <- paste0("G_variables/G_variables_", x)
  c_x <- list.files(c_p, pattern = ".csv", full.names = TRUE)
  c_x <- lapply(c_x, read.csv) %>% rbindlist(fill = TRUE) %>% as.data.frame()
  #Select columns
  cols <- c("PC01", "PC02", "PC03", "PC04", "PC05", "PC06")
  c_x2 <- lapply(cols, function(i) {
    pc_i <- c_x[,c("Longitude", "Latitude", i)] %>% na.omit() %>%
      distinct(Longitude, Latitude, .keep_all = TRUE)
    return(pc_i)
  })
  #Merge datafram,es
  c_x3 <- c_x2 %>% purrr::reduce(left_join, by = c("Longitude", "Latitude"))

  #remove columns
  m_i <- c_x3 %>% select(-Longitude, - Latitude) %>% as.matrix()

  #Make sure rasters have the same variables
  m_i <- m_i[,names(spat_x)]

  #Mop with extrapolation
  mop_ext <- mop(m = m_i, g = spat_x,
                 type = "detailed",
                 calculate_distance = TRUE,
                 where_distance = "all",
                 distance = "euclidean",
                 scale = TRUE, center = TRUE,
                 parallel = TRUE, n_cores = 8)

  #Set levels
  for (z in names(mop_ext$mop_detailed[-1])) {
  #lapply(names(mop_ext$mop_detailed)[-1], function(x){
    if(z %in% c("towards_low_combined", "towards_high_combined")){
      levels(mop_ext$mop_detailed[[z]]) <- list(data.frame(
        id = mop_ext$mop_detailed$interpretation_combined$values,
        category = mop_ext$mop_detailed$interpretation_combined$extrapolation_variables))}
  }
#)

  #Save results
  dir_x<- paste0("6.MOP/", x) #Create directory to save
  dir.create(dir_x)
  saveRDS(mop_ext$summary,
          paste0(dir_x, "/summary_E.rds"))
  writeRaster(mop_ext$mop_distances, paste0(dir_x, "/mop_distances_E.tif"),
              overwrite = TRUE)
  writeRaster(mop_ext$mop_basic, paste0(dir_x, "/mop_basic_E.tif"),
              overwrite = TRUE)
  writeRaster(mop_ext$mop_simple, paste0(dir_x, "/mop_simple_E.tif"),
              overwrite = TRUE)
  writeRaster(mop_ext$mop_detailed$towards_low_end,
              paste0(dir_x, "/towards_low_end_E.tif"),
              overwrite = TRUE)
  writeRaster(mop_ext$mop_detailed$towards_high_end,
              paste0(dir_x, "/towards_high_end_E.tif"),
              overwrite = TRUE)
  writeRaster(mop_ext$mop_detailed$towards_low_combined,
              paste0(dir_x, "/towards_low_combined_E.tif"),
              overwrite = TRUE)
  writeRaster(mop_ext$mop_detailed$towards_high_combined,
              paste0(dir_x, "/towards_high_combined_E.tif"),
              overwrite = TRUE)
  write.csv(mop_ext$mop_detailed$interpretation_combined,
            paste0(dir_x, "/interpretation_combined_E.csv"),
            row.names = FALSE)

  ####Mop with extrapolation and clamping####
  #Get min and max values in calibration area
  min_i <- apply(m_i, 2, min)
  max_i <- apply(m_i, 2, max)

  g_clamped <- rast(lapply(names(spat_x), function(x){
    terra::clamp(spat_x[[x]], min_i[x], max_i[x])
  }))

  mop_clamp <- mop(m = m_i, g = g_clamped,
                   type = "basic",
                   calculate_distance = TRUE,
                   where_distance = "all",
                   distance = "euclidean",
                   scale = TRUE, center = TRUE,
                   parallel = TRUE, n_cores = 10)

  #Save results
  saveRDS(mop_clamp$summary,
          paste0(dir_x, "/summary_EC.rds"))
  terra::writeRaster(mop_clamp$mop_distances, paste0(dir_x, "/mop_distances_EC.tif"),
                     overwrite = TRUE)
  #End function
  return(invisible(NULL))

})

#### Plot of maps with ggplot ####
library(ggplot2)
library(tidyterra)

#Create dir to save results
dir.create("6.MOP/Figures")

#Get all subsets
s <- c(paste0("subset_", 1:10),
       "All_points")
s

#Get spatvector
r_base <- rast("G_variables/G_variables_subset_1/Set_52/current/PC01.asc")
plot(r_base)
r_base <- (!is.na(r_base)) * 1
terra::NAflag(r_base) <- 0
v <- as.polygons(terra::trim(r_base))
plot(v)

####Mop combined####
#Towards low
tl <- lapply(s, function(x){
  print(x)
  r_x <- rast(paste0("7. MOP/", x, "/towards_low_combined_E.tif"))
  crop(r_x, v, mask = TRUE)
}) %>% rast()
names(tl) <- s
plot(tl)
#Plot
g_tl <- ggplot() + geom_spatvector(data = v, fill = "grey88") +
    geom_spatraster(data = tl) +
    facet_wrap(~lyr, ncol = 3) +
    scale_fill_manual(values = as.character(pals::okabe()[-1]),
                      na.value = NA, na.translate=FALSE,
                      name = "Variables") +
    geom_spatvector(data = v, fill = NA) +
    #scale_fill_terrain_c() +
    # scale_fill_whitebox_d(palette = "soft", name = "Number of variables", drop = TRUE,
    #                       na.translate = F) +
    ggtitle("MOP - Towards Low values") +
    ggpubr::theme_pubclean() +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.text=element_text(size=8),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
g_tl
ggsave("6.MOP/Figures/Towards_low.png",
       g_tl, dpi = 600, width = 7.5, height = 7,
       scale = 1.4)

#Towards high
th <- lapply(s, function(x){
  r_x <- rast(paste0("6.MOP/", x, "/towards_high_combined_E.tif"))
  crop(r_x, v, mask = TRUE)
}) %>% rast()
names(th) <- s
plot(th)

#Create pallete
p <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
       "#D55E00", "#CC79A7","#1ABC9C", "forestgreen", "firebrick",
       "blue1", "#FA0087", "#565656" )

#Plot
g_th <- ggplot() + geom_spatvector(data = v, fill = "grey88") +
  geom_spatraster(data = th) +
  facet_wrap(~lyr, ncol = 3) +
  scale_fill_manual(values = p,
                    na.value = NA, na.translate=FALSE,
                    name = "Variables") +
  geom_spatvector(data = v, fill = NA) +
  #scale_fill_terrain_c() +
  # scale_fill_whitebox_d(palette = "soft", name = "Number of variables", drop = TRUE,
  #                       na.translate = F) +
  ggtitle("MOP - Towards High values") +
  ggpubr::theme_pubclean() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text=element_text(size=8),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
g_th
ggsave("6.MOP/Figures/Towards_high.png",
       g_th, dpi = 600, width = 7.5, height = 7,
       scale = 1.4)

####Mop distance####
#With extrapolation
mop_e <- lapply(s, function(x){
  r_x <- rast(paste0("6.MOP/", x, "/mop_distances_E.tif"))
  crop(r_x, v, mask = TRUE)
}) %>% rast()
names(mop_e) <- s
plot(mop_e)
#Plot
g_dist_e <- ggplot() + geom_spatvector(data = v, fill = "grey88") +
  geom_spatraster(data = mop_e) +
  facet_wrap(~lyr, ncol = 3) +
  geom_spatvector(data = v, fill = NA) +
  scale_fill_terrain_c(name = "Distance",
                       breaks = c(0,5,10,15,20,25),
                       limits = c(0,25)) +
  ggtitle("MOP - Distances (Extrapolation)") +
  ggpubr::theme_pubclean() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text=element_text(size=8),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.key.width = unit(2, "cm"))
g_dist_e
ggsave("6.MOP/Figures/Distance_Extrapolation.png",
       g_dist_e, dpi = 600, width = 7.5, height = 7,
       scale = 1.4)
