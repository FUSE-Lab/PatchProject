#Adding social and environmental attibutes to plots
#By: Lindsay Darling
#Started on 8/31/2021

#Load libraries---------

library(plyr)
library(tidyverse)    
library(tidylog)    #Make tidyverse more verbose
library(sf)         #Spatial manipulation
library(raster)     #Rasters!
library(spatialEco)

setwd("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject")

#Load plots

plotTree <- st_read('allPlotTreeBA1.shp') %>% 
  st_buffer(., 9.36) #Buffer to plot radius (30.7' = 9.36 m)

#Add census data---------------

#2015 ACS data. Code for production else where
census<-st_read('2015Clean.shp') %>% 
  sf::st_transform(., crs = 5070) %>% 
  #Change 0 in house age to NA
  mutate(House_age = ifelse(House_age==0,NA,House_age)) 

censusPlot<-st_interpolate_aw(census[c('TotPopD', 'WhtPopP', 'BlkPopP', 'AsnPopP', 
                                        'OthPopP', 'UnivP', 'PovP', 'UnplydP', 
                                        'HouseD', 'OwnerP', 'RenterP', 'House_age',
                                        'Med_ncm')], 
                               plotTree,extensive = FALSE, na.rm = TRUE) %>% 
  st_drop_geometry() 

#Add land use data---------------------

lu<-st_read('LandUse2015.shp')%>% 
  sf::st_transform(., crs = 5070) %>% 
  st_make_valid(.) #fix topology error

plotLU<-st_intersection(plotTree, lu) %>% 
  mutate(intersect_area = st_area(.)) %>%   # create new column with shape area
  dplyr::select(PlotID, STRATIFIED, intersect_area) %>%   # only select columns needed to merge
  mutate(intArea = as.numeric(intersect_area)) %>%  
  group_by(PlotID, STRATIFIED) %>% #Dissolve area of cells that have several patches
  summarize(sum(intArea)) %>% #Add intersect area
  st_drop_geometry(.) %>%  
  spread(STRATIFIED, 'sum(intArea)') #Spread long data into columns for each lu

#Slope and aspect---------------------

slope <- raster('D:/Dropbox/Forest Composition/composition/Maps/shapefiles/INILProject/geomorphon/DEM10m/Slope.tif')

slope_stats <- zonal.stats(plotTree, slope, stats = c("mean"))

aspect <- raster('D:/Dropbox/Forest Composition/composition/Maps/shapefiles/INILProject/geomorphon/DEM10m/Aspect.tif')

aspect_stats <- zonal.stats(plotTree, aspect, stats = c("mean"))

#Join together------------

plotAttributes <- cbind(plotTree, censusPlot, plotLU, slope_stats, aspect_stats)

View(plotAttributes)

#Write it out

plotAttributes %>% 
  st_drop_geometry(.) %>% 
  write_csv(., 'plotAttributes.csv')

