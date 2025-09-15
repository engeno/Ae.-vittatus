#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This code performs an exploration of occurrences samples in environment using PC1 and 
# PC2
# Data necessary for this script:
# 1. occurrences for each sample
# 2. Accessible area (M) shape file
# 3. Environmental variables
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Open library
library(ggplot2)
library(ecospat)
library(terra)

# Set working directory
setwd(".../Data/environ_exploration/environ_exploration")

#Read occ of each sample
sample1 <- read.csv("sample_1_new/sp_joint.csv")
sample2 <- read.csv("sample_2_new/sp_joint.csv")
sample3 <- read.csv("sample_3_new/sp_joint.csv")
sample4 <- read.csv("sample_4_new/sp_joint.csv")
sample5 <- read.csv("sample_5_new/A_vittatus_country_2.5_05.csv")
sample6 <- read.csv("sample_6_new/A_vittatus_country_2.5_06.csv")
sample7 <- read.csv("sample_7_new/A_vittatus_country_2.5_07.csv")
sample8 <- read.csv("sample_8_new/A_vittatus_country_2.5_08.csv")
sample9 <- read.csv("sample_9_new/A_vittatus_country_2.5_09.csv")
sample10 <- read.csv("sample_10_new/A_vittatus_country_2.5_10.csv")


# Read environment variables
cvar <- rast(list.files("variables/variables_PCs_2.5m",
                        full.names = T)[c(1, 2, 3, 4, 5)])

# Change name of the environmental variables
names(cvar) <- c("PC01","PC02","PC03", "PC04", "PC05")

# Plot environmental variables
plot(cvar)

# Open the M shapefile
mnat <- vect("..Data/Shapefile/M_shapefile/Mosquito_m.shp")

# Plot M shapefile
plot(mnat)

# Change CRS 
crs(mnat) <- crs(cvar)

# Crop environmental variables using the M shape and convert it to a dataframe
M <- as.data.frame(crop(cvar,mnat, mask = TRUE))

# Extract environmental data from records and M
var_1 <- na.omit(extract(cvar, sample1[, 2:3])[, -1])
var_2 <- na.omit(extract(cvar, sample2[, 2:3])[, -1])
var_3 <- na.omit(extract(cvar, sample3[, 2:3])[, -1])
var_4 <- na.omit(extract(cvar, sample4[, 2:3])[, -1])
var_5 <- na.omit(extract(cvar, sample5[, 2:3])[, -1])
var_6 <- na.omit(extract(cvar, sample6[, 2:3])[, -1])
var_7 <- na.omit(extract(cvar, sample7[, 2:3])[, -1])
var_8 <- na.omit(extract(cvar, sample8[, 2:3])[, -1])
var_9 <- na.omit(extract(cvar, sample9[, 2:3])[, -1])
var_10 <- na.omit(extract(cvar, sample10[, 2:3])[, -1])

# Plot pc1 vs pc2
if (!dir.exists("Figures")) dir.create("Figures")
png(file = paste0("Figures/pc01_pc02.png"), width = 215, height = 279, units = "mm", res = 600)
mlay <- matrix(1:10, nrow = 5, ncol = 2, byrow = TRUE)  
layout(mlay, heights = rep(1, 5))  
par(cex = 0.5, mar = c(6, 6, 6, 1))

# Graph 1
plot(M$PC01, M$PC02, pch = 10, col = "gray", xlab = "", ylab = "PC02",ylim = c(-10, 15),
     xlim= c(-20,5), axes= FALSE, main = "Sample 1", cex.lab=2, cex.main=2)
axis(2, cex.axis=2)
points(var_1$PC01, var_1$PC02, col = "black", pch = 20)

# Graph 2
plot(M$PC01, M$PC02, pch = 10, col = "gray", xlab = "", ylab = "",ylim = c(-10, 15),
     xlim= c(-20,5), axes= FALSE, main = "Sample 2", cex.lab=2, cex.axis=2, cex.main=2)
points(var_2$PC01, var_2$PC02, col = "black", pch = 20)

# Graph 3
plot(M$PC01, M$PC02, pch = 10, col = "gray", xlab = "", ylab = "PC02",
     ylim = c(-10, 15), xlim= c(-20,5), axes= FALSE,main = "Sample 3", cex.lab=2, cex.axis=2, cex.main=2)
axis(2, cex.axis=2)
points(var_3$PC01, var_3$PC02, col = "black", pch = 20)

# Graph 4
plot(M$PC01, M$PC02, pch = 10, col = "gray", xlab = "", ylab = "",ylim = c(-10, 15),
     xlim= c(-20,5), axes= FALSE, main = "Sample 4", cex.lab=2, cex.axis=2, cex.main=2)
points(var_4$PC01, var_4$PC02, col = "black", pch = 20)

# Graph 5
plot(M$PC01, M$PC02, pch = 10, col = "gray", xlab = "", ylab = "PC02", ylim = c(-10, 15),
    xlim= c(-20,5), axes= FALSE,main = "Sample 5", cex.lab=2, cex.axis=2, cex.main=2)
axis(2, cex.axis=2)
points(var_5$PC01, var_5$PC02, col = "black", pch = 20)

# Graph 6
plot(M$PC01, M$PC02, pch = 10, col = "gray", xlab = "", ylab = "", ylim = c(-10, 15),
     xlim= c(-20,5), axes= FALSE,main = "Sample 6", cex.lab=2, cex.axis=2, cex.main=2)
points(var_6$PC01, var_6$PC02, col = "black", pch = 20)

# Graph 7
plot(M$PC01, M$PC02, pch = 10, col = "gray", xlab = "", ylab = "PC02",ylim = c(-10, 15),
     xlim= c(-20,5), axes= FALSE, main = "Sample 7", cex.lab=2, cex.axis=2, cex.main=2)
axis(2, cex.axis=2)
points(var_7$PC01, var_7$PC02, col = "black", pch = 20)

# Graph 8
plot(M$PC01, M$PC02, pch = 10, col = "gray", xlab = "", ylab = "", ylim = c(-10, 15),
     xlim= c(-20,5), axes= FALSE,main = "Sample 8", cex.lab=2, cex.axis=2, cex.main=2)
points(var_8$PC01, var_8$PC02, col = "black", pch = 20)

# Graph 9
plot(M$PC01, M$PC02, pch = 10, col = "gray", xlab = "PC01", ylab = "PC02",ylim = c(-10, 15),
     xlim= c(-20,5), axes= FALSE, main = "Sample 9", cex.lab=2, cex.axis=2, cex.main=2)
axis(1, cex.axis=2)
axis(2, cex.axis=2)
points(var_9$PC01, var_9$PC02, col = "black", pch = 20)

# Graph 10
plot(M$PC01, M$PC02, pch = 10, col = "gray", xlab = "PC01", ylab = "", ylim = c(-10, 15),
     xlim= c(-20,5), axes= FALSE, main = "Sample 10", cex.lab=2, cex.axis=2, cex.main=2)
axis(1, cex.axis=2)
points(var_10$PC01, var_10$PC02, col = "black", pch = 20)

dev.off()