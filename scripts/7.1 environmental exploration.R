# Open libraries
library(raster)
library(terra)
#library(viridis)

# Set working directory
setwd("")

# Open predictions for sample 3 in North America
consen_median_E_3<- rast(".../sample_3_new/Final_model_stats/Statistics_E/current_med.tif")

# Plot raster
plot(consen_median_E_3)

# Convert raster to a data frame
consen_median_E_3_df <- as.data.frame(consen_median_E_3)
colnames(consen_median_E_3_df)

# Open environmental variables for the final set of model for sample 3
pc<- rast(list.files(".../sample_3_new/G_variables/Set_39/current",
                full.names = T)[c(2, 3, 4, 5)])
names(pc) <- c("PC01","PC02", "PC04", "PC05")

# Convert environmental information into a data frame
pc_df <- as.data.frame(pc)

# Plot suitability (clog log output) from model 3 in environmental space (PC01)
plot(pc_df$PC01,consen_median_E_3_df$current_med, col = "#7f0000", pch=1, xlab= "PC01", ylab="Cloglog output")

# Fit a quadratic model

# Combine the data frames into a new data frame
combined_df <- data.frame(
  PC01 = pc_df$PC01,
  NA_med = consen_median_E_3_df$current_med
)

# Check the structure of the new data frame
print(dim(combined_df))  # Should have two columns

# Fit the quadratic model
quadratic_model <- lm(NA_med ~ poly(PC01, 2, raw = TRUE), data = combined_df)

# Create a sequence of 100 x-values (PC01) for predictions
x_vals <- seq(min(combined_df$PC01), max(combined_df$PC01), length.out = 100)

# Generate the new data for prediction
new_data <- data.frame(PC01 = x_vals)

# Predict the quadratic scores
predicted_scores <- predict(quadratic_model, newdata = new_data)

# Check if the lengths match
print(length(x_vals))  
print(length(predicted_scores))  

# Now plot the original data and add the quadratic line

# Define the number of classes
n_classes <- 9

# Define the custom color palette
custom_colors <- c(
  "#4575b4",  # lowest
  "#74add1",  
  "#abd9e9",  
  "#e0f3f8",  
  "#ffffbf",  
  "#fee090",  
  "#fdae61",  
  "#f46d43",  
  "#d73027"   #highest
)

# Cut the NA_med values into classes
combined_df$class <- cut(combined_df$NA_med, breaks = n_classes, labels = 1:n_classes)

# Assign colors based on the classes
point_colors <- custom_colors[as.numeric(combined_df$class)]

tiff("./Figures/MAP_PC1.tif", width = 215, height = 279, units = "mm", res = 600)  # Ajusta el tamaño y la resolución como desees
layout(matrix(c(1, 2), nrow = 1), widths = c(2, 1), heights = c(3, 1))  # Ajusta las proporciones aquí

par(mar = c(5, 2, 2, 2))
# Plot the raster map on the left with smaller title
plot(consen_median_E_3, main = "Areas predicted suitable for Aedes vittatus (sample 3)", 
     col = custom_colors, legend = TRUE, cex.main = 1)
north(type = 2, col = "black", size = 0.5)
# Use ext to place scalebar
scalebar(1000,                # length in map units
         type = "line", 
         divs = 3, 
         below = "km",
         #adj=c(0.5, 1.2),
         xy=click())         # Tama?o del texto

# Now plot the original data and add the quadratic line on the right
# Adjust the margins for the right plot
par(mar = c(4, 4, 2, 1))  
plot(
  combined_df$PC01, 
  combined_df$NA_med, 
  main = "Suitability values for PC01", 
  cex.main = 1,
  xlab = "PC01", 
  ylab = "Suitability",
  pch = 16,  # Point character
  col = point_colors  # Use the color vector
)

# Add the quadratic line to the plot
lines(x_vals, predicted_scores, col = "black", lwd = 2)

# Reset layout to default
layout(1)
dev.off()

# END