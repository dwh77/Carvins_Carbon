#### Plotting raster data of NDWI from GEE

####read in packages
library(tidyverse)
library(raster)

##a nEON working with raster data tutorial: https://www.neonscience.org/resources/learning-hub/tutorials/plot-raster-data-r

#### how to plot real RGB image
image_sep22 <- raster::stack("./Map_Resources/GEE/image_export2024-09-22.tif")

raster::plotRGB(image_sep22)



#### Some first try's of reading in data and plotting ----

## Read in a raster from GEE output
ccrraster_sep22 <- raster::raster("./Map_Resources/GEE/NDWI_2024-09-22.tif")

##basic ploting raster
raster::plot(ccrraster_sep22, main="22 Sep 2024")

##more detailed plotting and setting color pallette
pal <- colorRampPalette(c("white", "#0096C7", "#023E8A"))

raster::plot(
  ccrraster_sep22,
  main = "22 Sep 2024",
  col = pal(100),
  zlim = c(-1, 1),     # NDWI range
  axes = TRUE,
  legend = TRUE
)


##make a dataframe and plot using ggplot
# Convert raster to data frame
ccrraster_sep22_df <- as.data.frame(ccrraster_sep22, xy = TRUE)
# names(df)[3] <- "NDWI"

ccrraster_sep22_df |> 
  ggplot() +
  geom_raster(aes(x = x, y = y, fill = NDWI)) +
  scale_fill_gradientn(
    colours = c("white", "#0096C7", "#023E8A"),
    limits = c(-1, 1),
    name = "NDWI"
  ) +
  coord_equal() +
  labs(x = "Longitude", y = "Latitude", title = "22 Sep 2024") +
  #guides(fill = guide_colorbar(frame.colour = "black", frame.linewidth = 0.25))+ #option to but black box arround color scale
  theme_minimal(base_size = 14)



#### make a function that takes read in raster and makes it a data frame then ggplot ----

raster_ggplot <- function(df){
  
  ####get date of raster
  
##for when reading in as CCR_raster_22sep24
  # Get the name of the object passed in
  df_name <- deparse(substitute(df))

  # Extract the embedded date (e.g., "24sep24")
  raw_date <- sub(".*_(\\d{2}[a-z]{3}\\d{2}).*", "\\1", df_name, ignore.case = TRUE)

  # Convert to Date
  date_obj <- as.Date(raw_date, format = "%d%b%y")

  # Format nicely
  pretty_date <- format(date_obj, "%d %B %Y")
  
##for when reading in NDWI_2024-06-01
  # # Get the name of the object passed in
  # df_name <- deparse(substitute(df))
  # 
  # # Pull out the YYYY-MM-DD part
  # raw_date <- sub(".*_(\\d{4}-\\d{2}-\\d{2}).*", "\\1", df_name)
  # 
  # # Convert to Date
  # date_obj <- as.Date(raw_date, format = "%Y-%m-%d")
  # 
  # # Pretty format
  # pretty_date <- format(date_obj, "%d %B %Y")
  
  
  #### to crop to HPB region if interested
  bbox_coords <- raster::extent( -79.9735, -79.9590, 37.3645, 37.3725)
  
  df <- raster::crop(df, bbox_coords)
  
  
  
  #### make the raster file a dataframe
  raster_df <- as.data.frame(df, xy = TRUE)
  
  #### plot the data
  p <- raster_df |> 
    ggplot() +
    geom_raster(aes(x = x, y = y, fill = NDWI)) +
    scale_fill_gradientn(
      colours = c("white", "#0096C7", "#023E8A"),
      limits = c(-1, 1),
      name = "NDWI") +
    coord_equal() +
    labs(x = "Longitude", y = "Latitude", title = pretty_date) +
    #guides(fill = guide_colorbar(frame.colour = "black", frame.linewidth = 0.25))+ #option to but black box arround color scale
    theme_minimal(base_size = 14)
  
  return(p)

}#end of function


#### test function 
# NDWI_2024-09-22 <- raster::raster("./Map_Resources/GEE/NDWI_2024-09-22.tif")
# raster_ggplot(NDWI_2024-09-22)



#### make plots for all NDWI images
# folder <- "./Map_Resources/GEE/"
# # List all files beginning with "NDWI"
# files <- list.files(folder, pattern = "^NDWI.*\\.tif$", full.names = TRUE)


# ### read in files and plot
# 22 may 2024
CCR_raster_22may24 <- raster::raster("./Map_Resources/GEE/NDWI_2024-05-22.tif")
plot_22may24 <- raster_ggplot(CCR_raster_22may24)

# 27 may 2024
CCR_raster_27may24 <- raster::raster("./Map_Resources/GEE/NDWI_2024-05-27.tif")
plot_27may24 <- raster_ggplot(CCR_raster_27may24)

# 26 June 2024
CCR_raster_26jun24 <- raster::raster("./Map_Resources/GEE/NDWI_2024-06-26.tif")
plot_26jun24 <- raster_ggplot(CCR_raster_26jun24)

# 11 july 2024
CCR_raster_11jul24 <- raster::raster("./Map_Resources/GEE/NDWI_2024-07-11.tif")
plot_11jul24 <- raster_ggplot(CCR_raster_11jul24)

# 15 august 2024
CCR_raster_15aug24 <- raster::raster("./Map_Resources/GEE/NDWI_2024-08-15.tif")
plot_15aug24 <- raster_ggplot(CCR_raster_15aug24)

# 9 sep 2024
CCR_raster_09sep24 <- raster::raster("./Map_Resources/GEE/NDWI_2024-09-09.tif")
plot_09sep24 <- raster_ggplot(CCR_raster_09sep24)

# 22 sep 2024
CCR_raster_22sep24 <- raster::raster("./Map_Resources/GEE/NDWI_2024-09-22.tif")
plot_22sep24 <- raster_ggplot(CCR_raster_22sep24)

# 7 oct 2024
CCR_raster_07oct24 <- raster::raster("./Map_Resources/GEE/NDWI_2024-10-07.tif")
plot_07oct24 <- raster_ggplot(CCR_raster_07oct24)

# 9 oct 2024
CCR_raster_09oct24 <- raster::raster("./Map_Resources/GEE/NDWI_2024-10-09.tif")
plot_09oct24 <- raster_ggplot(CCR_raster_09oct24)

# 29 oct 2024
CCR_raster_29oct24 <- raster::raster("./Map_Resources/GEE/NDWI_2024-10-29.tif")
plot_29oct24 <- raster_ggplot(CCR_raster_29oct24)

# 23 dec 2024
CCR_raster_23dec24 <- raster::raster("./Map_Resources/GEE/NDWI_2024-12-23.tif")
plot_23dec24 <- raster_ggplot(CCR_raster_23dec24)

# 4 feb 2025
CCR_raster_04feb25 <- raster::raster("./Map_Resources/GEE/NDWI_2025-02-04.tif")
plot_04feb25 <- raster_ggplot(CCR_raster_04feb25)

# 21 feb 2025
CCR_raster_21feb25 <- raster::raster("./Map_Resources/GEE/NDWI_2025-02-21.tif")
plot_21feb25 <- raster_ggplot(CCR_raster_21feb25)

# 3 march 2025
CCR_raster_03mar25 <- raster::raster("./Map_Resources/GEE/NDWI_2025-03-03.tif")
plot_03mar25 <- raster_ggplot(CCR_raster_03mar25)

# 17 april 2025
CCR_raster_17apr25 <- raster::raster("./Map_Resources/GEE/NDWI_2025-04-17.tif")
plot_17apr25 <- raster_ggplot(CCR_raster_17apr25)


#### plot all data
library(patchwork)
(plot_22may24 | plot_27may24 | plot_26jun24 | plot_11jul24 | plot_15aug24) /
(plot_09sep24 | plot_22sep24 | plot_07oct24 | plot_09oct24 | plot_29oct24) /
(plot_23dec24 | plot_04feb25 | plot_21feb25 | plot_03mar25 | plot_17apr25)

#### pre post helene
plot_22sep24 | plot_07oct24

#### pre post feb storms
plot_23dec24 | plot_04feb25 | plot_21feb25 | plot_03mar25

#### trim plots
(plot_22may24 |  plot_11jul24 | plot_15aug24) /
  (plot_09sep24 | plot_09oct24 | plot_29oct24) /
  (plot_23dec24  | plot_21feb25  | plot_17apr25)



####trying cropping to HPB area
bbox_coords <- raster::extent( -79.9735, -79.9590, 37.3630, 37.3725)

cropped_raster <- raster::crop(CCR_raster_17apr25, bbox_coords)
raster_ggplot(cropped_raster)

