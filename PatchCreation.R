#Separating forest patches into types
#Lindsay Darling
#6/10/2021

#Load libraries------------

library(tidyverse)    
library(tidylog)    #Make tidyverse more verbose
library(sf)         #Spatial manipulation
library(magrittr)   #Piping %>% 

#load data----------------

setwd("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject")

veg <- st_read('AllVeg.shp')%>% 
  sf::st_transform(., crs = 5070) #Change to albers equal conic

midCen <- st_read('1939_Oaks_CW.shp')%>% 
  sf::st_transform(., crs = 5070) %>% 
  st_make_valid(.)

currentedge <- st_read('AllEdge.shp')%>% 
  sf::st_transform(., crs = 5070) %>% 
  st_make_valid(.) #Fix topology

currentcore <- st_read('ChiCoreOnly.shp')%>% 
  sf::st_transform(., crs = 5070) %>% 
  st_make_valid(.) #Fix topology

#Isolate wooded areas in the PLS------------

woods <- veg %>% 
  st_make_valid(.) %>% #Fix issue with topology
  filter(ECOSYSTEM %in% c('Thicket', 'Barrens', 'Woodland')) #Only wooded ecosystem types
  

woodsBuff <- st_buffer(woods, dist = 100) #Create buffer since boundaries are fuzzy. Distance in m

#Create layers for each patch type----------

#Remnant cores
remCore <- st_intersection(currentcore, midCen) %>% 
  sf::st_transform(., crs = 5070) #%>% #Commenting out the write so it will run
  #st_write(., 'RemnantPatch.shp', driver = 'ESRI Shapefile')

#Remnant edges 
remEdge <- st_intersection(currentedge, midCen) #%>% 
  #st_write(., 'RemnantEdge.shp', driver = 'ESRI Shapefile')

#Regrowth cores
#!!! This code should work, but the file is too big. It keeps crashing during the difference step
#I am running the exact same process (clip to presettlement woodlands, erase remnant woodlands) in ArcGIS
#And saving the file to the same name. I'll do the same for the edges.

# regrowthCore <- st_intersection(currentcore, woods) %>% #Everything in preset woodlands
#   st_difference(.,remCore) %>% #Erase things that are remnants (ie forested in 1939)
#   st_write(., 'RegrowthPatch.shp', driver = 'ESRI Shapefile')

regrowthCore <- st_read('regrowthCore.shp') %>% 
  sf::st_transform(., crs = 5070) %>% 
  st_make_valid(.) #Fix topology

# #Regrowth edges
# regrowthEdge <- st_intersection(currentedge, woods) %>% 
#   st_write(., 'RegrowthEdge.shp', driver = 'ESRI Shapefile')

regrowthEdge <- st_read('regrowthEdge.shp') %>% 
  sf::st_transform(., crs = 5070) %>% 
  st_make_valid(.) #Fix topology

#!!! Again, st_difference keeps failing. I am doing the same process using 'erase' in arcgis 
#then loading those files into here.  

# #New cores
# newCore <- st_difference(currentcore, woodsBuff) %>% 
#   st_difference(., midCen) %>% #Sometimes 1939 patches are outside the preset boundaries.
#   st_write(., 'NovelPatch.shp', driver = 'ESRI Shapefile')
# 
# #New edges
# newEdge <- st_difference(currentedge, woodsBuff) #%>%  
#   st_difference(., midCen) %>% 
#   st_write(., 'NovelEdge.shp', driver = 'ESRI Shapefile')

newCore <- st_read('NovelPatch.shp')%>% 
  sf::st_transform(., crs = 5070)
newEdge <- st_read('NovelEdge.shp')%>% 
  sf::st_transform(., crs = 5070) %>% 
  st_make_valid(.)

#Load plot data------------------

#2010 data

plots <- read_csv('iTree2010Plots.csv') %>%
  sf::st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326) %>% 
  sf::st_transform(., crs = 5070)

#2020 data

plots2 <- st_read('PlotsAll2020.shp')%>% 
  sf::st_transform(., crs = 5070)

#Join to patch types-----------------

#Remnant
remCorePlot10 <- st_intersection(plots, remCore) %>% 
  st_write(.,'remCorePlot10.shp', driver = 'ESRI Shapefile') #31 plots

remCorePlot20 <- st_intersection(plots2, remCore) %>% 
  st_write(.,'remCorePlot20.shp', driver = 'ESRI Shapefile') #26 plots

remEdgePlot10 <- st_intersection(plots, remEdge) %>% 
  st_write(.,'remEdgePlot10.shp', driver = 'ESRI Shapefile') #18 plots

remEdgePlot20 <- st_intersection(plots2, remEdge) %>%  
  st_write(.,'remEdgePlot20.shp', driver = 'ESRI Shapefile') #17 plots

#Regrowth

regrowthCorePlot10 <- st_intersection(plots, regrowthCore) %>% 
  st_write(.,'regrowthCorePlot10.shp', driver = 'ESRI Shapefile') #13 plots

regrowthCorePlot20 <- st_intersection(plots2, regrowthCore) %>% 
  #It's having issues writing because of similar column names. Let's drop some.
  select(-OBJECTID_1, -OBJECTID.1, Id) %>% 
  st_write(.,'regrowthCorePlot20.shp', driver = 'ESRI Shapefile') #10 plots

regrowthEdgePlot10 <- st_intersection(plots, regrowthEdge) %>% 
  st_write(.,'regrowthEdgePlot10.shp', driver = 'ESRI Shapefile') #16 plots

regrowthEdgePlot20 <- st_intersection(plots2, regrowthEdge) %>% 
  select(-OBJECTID_1, -OBJECTID.1, Id) %>% 
  st_write(.,'regrowthEdgePlot20.shp', driver = 'ESRI Shapefile') #15 plots

#Novel

novelCorePlot10 <- st_intersection(plots, newCore) %>% 
  st_write(.,'novelCorePlot10.shp', driver = 'ESRI Shapefile') #10 plots

novelCorePlot20 <- st_intersection(plots2, newCore) %>% 
  select(-OBJECTID_1, -OBJECTID.1, Id) %>%
  st_write(.,'novelCorePlot20.shp', driver = 'ESRI Shapefile') #11 plots

novelEdgePlot10 <- st_intersection(plots, newEdge) %>% 
  st_write(.,'novelEdgePlot10.shp', driver = 'ESRI Shapefile') #18 plots

novelEdgePlot20 <- st_intersection(plots2, newEdge) %>% 
  select(-OBJECTID_1, -OBJECTID.1, Id) %>% 
  st_write(.,'novelEdgePlot20.shp', driver = 'ESRI Shapefile') #10 plots

