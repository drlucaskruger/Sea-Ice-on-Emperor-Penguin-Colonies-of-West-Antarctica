# Load libraries
library(ggplot2)
library(sf)
library(terra)
library(rasterVis)
library(animation)
library(raster)
library(latticeExtra)
library(patchwork)


#### ----------------------- plots without histogram-------------------
### ---------noville-------------

# Set your working directory to the folder containing the raster files
setwd("C:/Emperors/SciRep/Sentinel1_Radar/Noville")

# Get a list of all raster files in the folder
tif_files <- list.files(pattern = "\\.tiff$", full.names = TRUE)
site_reference <- vect("C:/Emperors/SentinelHub_EO_BrowserV2/06_Noville/c2022_10_11.shp")

# convert 
rasterVis::levelplot(raster(tif_files[4]))
coordsTO<-terra::crs(raster(tif_files[4]))
coordsFROM<-terra::crs(site_reference)


rst<-terra::project(site_reference,coordsTO)
plot(rst)

lns <- as.lines(rst)

# Define color scale range
color_scale_range <- c(0, 2)

# Create a function to generate a plot for each raster file
generate_plot <- function(file_path) {
  # Load raster file
  raster_data <- raster(file_path)
  
  # Project shapefile to match raster coordinate system
  coordsTO <- terra::crs(raster_data)
  coordsFROM <- terra::crs(site_reference)
  rst <- terra::project(site_reference, coordsTO)
  
  # Extract date from file name (assuming YYYY-MM-DD format)
  date <- substr(basename(file_path), 1, 10)
  
  # Create plot with outlines, a specified color scale range, and no marginal plots
  plt <- rasterVis::levelplot(raster_data, margin = F,scales = list(draw = FALSE),
                              xlim=c(-11000000, -10950000), ylim=c(-11680000, -11640000),
                              col.regions = colorRampPalette(c("blue", "yellow", "red", "red4")),
                              
                              at = seq(color_scale_range[1], color_scale_range[2], length.out = 100),
                              main = paste("Date: ", date))  # Add date as the main title
  
  # Add shapefile polygons to the plot

  plt + layer(llines(lns), theme = simpleTheme(col = "black", lty = 15, lwd = 5))
}

# Create an animation
saveGIF({
  for (file_path in tif_files) {
    plot <- generate_plot(file_path)
    print(plot)
  }
}, interval = 1, movie.name = "C:/Emperors/SciRep/animation_noville2.gif")



####------------ Smyley---------------------
rm(list=ls())

# Set your working directory to the folder containing the raster files
setwd("C:/Emperors/SciRep/Sentinel1_Radar/Smyley")

# Get a list of all raster files in the folder
tif_files <- list.files(pattern = "\\.tiff$", full.names = TRUE)
site_reference <- vect("C:/Emperors/SentinelHub_EO_BrowserV2/03_Smyley/c2021_11_22.shp")

# convert 
rasterVis::levelplot(raster(tif_files[4]))
coordsTO<-terra::crs(raster(tif_files[4]))
coordsFROM<-terra::crs(site_reference)


rst<-terra::project(site_reference,coordsTO)
plot(rst)

lns <- as.lines(rst)

# Define color scale range
color_scale_range <- c(0, 2)

# Create a function to generate a plot for each raster file
generate_plot <- function(file_path) {
  # Load raster file
  raster_data <- raster(file_path)
  
  # Project shapefile to match raster coordinate system
  coordsTO <- terra::crs(raster_data)
  coordsFROM <- terra::crs(site_reference)
  rst <- terra::project(site_reference, coordsTO)
  
  # Extract date from file name (assuming YYYY-MM-DD format)
  date <- substr(basename(file_path), 1, 10)
  
  # Create plot with outlines, a specified color scale range, and no marginal plots
  plt <- rasterVis::levelplot(raster_data, margin = F,scales = list(draw = FALSE),
                              xlim=c(-8800000, -8760000), ylim=c(-11871000, -11850800),### CHANGE LIMS 
                              col.regions = colorRampPalette(c("blue", "yellow", "red", "red4")),
                              
                              at = seq(color_scale_range[1], color_scale_range[2], length.out = 100),
                              main = paste("Date: ", date))  # Add date as the main title
  
  # Add shapefile polygons to the plot
  
  plt + layer(llines(lns), theme = simpleTheme(col = "black", lty = 15, lwd = 5))
}

# Create an animation
saveGIF({
  for (file_path in tif_files) {
    plot <- generate_plot(file_path)
    print(plot)
  }
}, interval = 1, movie.name = "C:/Emperors/SciRep/animation_smyley.gif")


### ------------PFROGNER ---------------

rm(list=ls())

# Set your working directory to the folder containing the raster files
setwd("C:/Emperors/SciRep/Sentinel1_Radar/PFROGNER")

# Get a list of all raster files in the folder
tif_files <- list.files(pattern = "\\.tiff$", full.names = TRUE)
site_reference <- vect("C:/Emperors/SentinelHub_EO_BrowserV2/05_Pfrogner/c2022_10_29.shp")

# convert 
rasterVis::levelplot(raster(tif_files[4]))
coordsTO<-terra::crs(raster(tif_files[4]))
coordsFROM<-terra::crs(site_reference)


rst<-terra::project(site_reference,coordsTO)
plot(rst)

lns <- as.lines(rst)

# Define color scale range
color_scale_range <- c(0, 2)

# Create a function to generate a plot for each raster file
generate_plot <- function(file_path) {
  # Load raster file
  raster_data <- raster(file_path)
  
  # Project shapefile to match raster coordinate system
  coordsTO <- terra::crs(raster_data)
  coordsFROM <- terra::crs(site_reference)
  rst <- terra::project(site_reference, coordsTO)
  
  # Extract date from file name (assuming YYYY-MM-DD format)
  date <- substr(basename(file_path), 1, 10)
  
  # Create plot with outlines, a specified color scale range, and no marginal plots
  plt <- rasterVis::levelplot(raster_data, margin = F,scales = list(draw = FALSE),
                              xlim=c(-10050000, -10000000), ylim=c(-11980000, -11940800),### CHANGE LIMS 
                              col.regions = colorRampPalette(c("blue", "yellow", "red", "red4")),
                              
                              at = seq(color_scale_range[1], color_scale_range[2], length.out = 100),
                              main = paste("Date: ", date))  # Add date as the main title
  
  # Add shapefile polygons to the plot
  
  plt + layer(llines(lns), theme = simpleTheme(col = "black", lty = 15, lwd = 5))
}

# Create an animation
saveGIF({
  for (file_path in tif_files) {
    plot <- generate_plot(file_path)
    print(plot)
  }
}, interval = 1, movie.name = "C:/Emperors/SciRep/animation_pfrogner.gif")


#### -------------Verdi----------------


rm(list=ls())

# Set your working directory to the folder containing the raster files
setwd("C:/Emperors/SciRep/Sentinel1_Radar/Verdi")

# Get a list of all raster files in the folder
tif_files <- list.files(pattern = "\\.tiff$", full.names = TRUE)
site_reference <- vect("C:/Emperors/SentinelHub_EO_BrowserV2/02_Verdi/c2021_10_30.shp")

# convert 
rasterVis::levelplot(raster(tif_files[4]))
coordsTO<-terra::crs(raster(tif_files[4]))
coordsFROM<-terra::crs(site_reference)


rst<-terra::project(site_reference,coordsTO)
plot(rst)

lns <- as.lines(rst)

# Define color scale range
color_scale_range <- c(0, 2)

# Create a function to generate a plot for each raster file
generate_plot <- function(file_path) {
  # Load raster file
  raster_data <- raster(file_path)
  
  # Project shapefile to match raster coordinate system
  coordsTO <- terra::crs(raster_data)
  coordsFROM <- terra::crs(site_reference)
  rst <- terra::project(site_reference, coordsTO)
  
  # Extract date from file name (assuming YYYY-MM-DD format)
  date <- substr(basename(file_path), 1, 10)
  
  # Create plot with outlines, a specified color scale range, and no marginal plots
  plt <- rasterVis::levelplot(raster_data, margin = F,scales = list(draw = FALSE),
                              xlim=c(-8350000, -8300000), ylim=c(-11620000, -11580000),### CHANGE LIMS 
                              col.regions = colorRampPalette(c("blue", "yellow", "red", "red4")),
                              
                              at = seq(color_scale_range[1], color_scale_range[2], length.out = 100),
                              main = paste("Date: ", date))  # Add date as the main title
  
  # Add shapefile polygons to the plot
  
  plt + layer(llines(lns), theme = simpleTheme(col = "black", lty = 15, lwd = 5))
}

# Create an animation
saveGIF({
  for (file_path in tif_files) {
    plot <- generate_plot(file_path)
    print(plot)
  }
}, interval = 1, movie.name = "C:/Emperors/SciRep/animation_verdi.gif")




#### ----------------------- plots with histogram-------------------


rm(list=ls())

### ---------noville-------------

# Set your working directory to the folder containing the raster files
setwd("C:/Emperors/SciRep/Sentinel1_Radar/Noville")

# Get a list of all raster files in the folder
tif_files <- list.files(pattern = "\\.tiff$", full.names = TRUE)
site_reference <- vect("C:/Emperors/SentinelHub_EO_BrowserV2/06_Noville/c2022_10_11.shp")

# convert 
rasterVis::levelplot(raster(tif_files[4]))
coordsTO<-terra::crs(raster(tif_files[4]))
coordsFROM<-terra::crs(site_reference)


rst<-terra::project(site_reference,coordsTO)
plot(rst)

lns <- as.lines(rst)

# Define color scale range
color_scale_range <- c(0, 2)

# Create a function to generate a plot for each raster file
generate_plot <- function(file_path) {
  # Load raster file
  raster_data <- raster(file_path)
  
  # Project shapefile to match raster coordinate system
  coordsTO <- terra::crs(raster_data)
  coordsFROM <- terra::crs(site_reference)
  rst <- terra::project(site_reference, coordsTO)
  
  # Extract date from file name (assuming YYYY-MM-DD format)
  date <- substr(basename(file_path), 1, 10)
  
  # Create plot with outlines, a specified color scale range, and no marginal plots
  plt <- rasterVis::levelplot(raster_data, margin = T,scales = list(draw = FALSE),
                              #xlim=c(-11000000, -10950000), ylim=c(-11680000, -11640000),
                              col.regions = colorRampPalette(c("blue", "yellow", "red", "red4")),
                              
                              at = seq(color_scale_range[1], color_scale_range[2], length.out = 100),
                              main = paste("Date: ", date))  # Add date as the main title
  
  # Add shapefile polygons to the plot
  
  plt + layer(llines(lns), theme = simpleTheme(col = "black", lty = 15, lwd = 5))
}

# Create an animation
saveGIF({
  for (file_path in tif_files) {
    plot <- generate_plot(file_path)
    print(plot)
  }
}, interval = 1, movie.name = "C:/Emperors/SciRep/animation_noville_hist.gif")



####------------ Smyley---------------------
rm(list=ls())

# Set your working directory to the folder containing the raster files
setwd("C:/Emperors/SciRep/Sentinel1_Radar/Smyley")

# Get a list of all raster files in the folder
tif_files <- list.files(pattern = "\\.tiff$", full.names = TRUE)
site_reference <- vect("C:/Emperors/SentinelHub_EO_BrowserV2/03_Smyley/c2021_11_22.shp")

# convert 
rasterVis::levelplot(raster(tif_files[4]))
coordsTO<-terra::crs(raster(tif_files[4]))
coordsFROM<-terra::crs(site_reference)


rst<-terra::project(site_reference,coordsTO)
plot(rst)

lns <- as.lines(rst)

# Define color scale range
color_scale_range <- c(0, 2)

# Create a function to generate a plot for each raster file
generate_plot <- function(file_path) {
  # Load raster file
  raster_data <- raster(file_path)
  
  # Project shapefile to match raster coordinate system
  coordsTO <- terra::crs(raster_data)
  coordsFROM <- terra::crs(site_reference)
  rst <- terra::project(site_reference, coordsTO)
  
  # Extract date from file name (assuming YYYY-MM-DD format)
  date <- substr(basename(file_path), 1, 10)
  
  # Create plot with outlines, a specified color scale range, and no marginal plots
  plt <- rasterVis::levelplot(raster_data, margin = T,scales = list(draw = FALSE),
                              #xlim=c(-8800000, -8760000), ylim=c(-11871000, -11850800),### CHANGE LIMS 
                              col.regions = colorRampPalette(c("blue", "yellow", "red", "red4")),
                              
                              at = seq(color_scale_range[1], color_scale_range[2], length.out = 100),
                              main = paste("Date: ", date))  # Add date as the main title
  
  # Add shapefile polygons to the plot
  
  plt + layer(llines(lns), theme = simpleTheme(col = "black", lty = 15, lwd = 5))
}

# Create an animation
saveGIF({
  for (file_path in tif_files) {
    plot <- generate_plot(file_path)
    print(plot)
  }
}, interval = 1, movie.name = "C:/Emperors/SciRep/animation_smyley_hist.gif")


### ------------PFROGNER ---------------

rm(list=ls())

# Set your working directory to the folder containing the raster files
setwd("C:/Emperors/SciRep/Sentinel1_Radar/PFROGNER")

# Get a list of all raster files in the folder
tif_files <- list.files(pattern = "\\.tiff$", full.names = TRUE)
site_reference <- vect("C:/Emperors/SentinelHub_EO_BrowserV2/05_Pfrogner/c2022_10_29.shp")

# convert 
rasterVis::levelplot(raster(tif_files[4]))
coordsTO<-terra::crs(raster(tif_files[4]))
coordsFROM<-terra::crs(site_reference)


rst<-terra::project(site_reference,coordsTO)
plot(rst)

lns <- as.lines(rst)

# Define color scale range
color_scale_range <- c(0, 2)

# Create a function to generate a plot for each raster file
generate_plot <- function(file_path) {
  # Load raster file
  raster_data <- raster(file_path)
  
  # Project shapefile to match raster coordinate system
  coordsTO <- terra::crs(raster_data)
  coordsFROM <- terra::crs(site_reference)
  rst <- terra::project(site_reference, coordsTO)
  
  # Extract date from file name (assuming YYYY-MM-DD format)
  date <- substr(basename(file_path), 1, 10)
  
  # Create plot with outlines, a specified color scale range, and no marginal plots
  plt <- rasterVis::levelplot(raster_data, margin = T,scales = list(draw = FALSE),
                             # xlim=c(-10050000, -10000000), ylim=c(-11980000, -11940800),### CHANGE LIMS 
                              col.regions = colorRampPalette(c("blue", "yellow", "red", "red4")),
                              
                              at = seq(color_scale_range[1], color_scale_range[2], length.out = 100),
                              main = paste("Date: ", date))  # Add date as the main title
  
  # Add shapefile polygons to the plot
  
  plt + layer(llines(lns), theme = simpleTheme(col = "black", lty = 15, lwd = 5))
}

# Create an animation
saveGIF({
  for (file_path in tif_files) {
    plot <- generate_plot(file_path)
    print(plot)
  }
}, interval = 1, movie.name = "C:/Emperors/SciRep/animation_pfrogner_hist.gif")


#### -------------Verdi----------------


rm(list=ls())

# Set your working directory to the folder containing the raster files
setwd("C:/Emperors/SciRep/Sentinel1_Radar/Verdi")

# Get a list of all raster files in the folder
tif_files <- list.files(pattern = "\\.tiff$", full.names = TRUE)
site_reference <- vect("C:/Emperors/SentinelHub_EO_BrowserV2/02_Verdi/c2021_10_30.shp")

# convert 
rasterVis::levelplot(raster(tif_files[4]))
coordsTO<-terra::crs(raster(tif_files[4]))
coordsFROM<-terra::crs(site_reference)


rst<-terra::project(site_reference,coordsTO)
plot(rst)

lns <- as.lines(rst)

# Define color scale range
color_scale_range <- c(0, 2)

# Create a function to generate a plot for each raster file
generate_plot <- function(file_path) {
  # Load raster file
  raster_data <- raster(file_path)
  
  # Project shapefile to match raster coordinate system
  coordsTO <- terra::crs(raster_data)
  coordsFROM <- terra::crs(site_reference)
  rst <- terra::project(site_reference, coordsTO)
  
  # Extract date from file name (assuming YYYY-MM-DD format)
  date <- substr(basename(file_path), 1, 10)
  
  # Create plot with outlines, a specified color scale range, and no marginal plots
  plt <- rasterVis::levelplot(raster_data, margin = T,scales = list(draw = FALSE),
                             # xlim=c(-8350000, -8300000), ylim=c(-11620000, -11580000),### CHANGE LIMS 
                              col.regions = colorRampPalette(c("blue", "yellow", "red", "red4")),
                              
                              at = seq(color_scale_range[1], color_scale_range[2], length.out = 100),
                              main = paste("Date: ", date))  # Add date as the main title
  
  # Add shapefile polygons to the plot
  
  plt + layer(llines(lns), theme = simpleTheme(col = "black", lty = 15, lwd = 5))
}

# Create an animation
saveGIF({
  for (file_path in tif_files) {
    plot <- generate_plot(file_path)
    print(plot)
  }
}, interval = 1, movie.name = "C:/Emperors/SciRep/animation_verdi_hist.gif")


