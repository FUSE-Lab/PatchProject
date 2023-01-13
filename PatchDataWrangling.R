#Aggregating forest, land use, and census data
#Lindsay Darling
#4/15/2021

#Load libraries------------

library(tidyverse)    
library(tidylog)    #Make tidyverse more verbose
library(sf)         #Spatial manipulation
library(magrittr)   #Piping %>% 

#load data----------------

setwd("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject")

#2015 ACS data. Code for production else where
census<-st_read('2015Clean.shp') %>% 
  sf::st_transform(., crs = 5070) %>% 
  #Change 0 in house age to NA
  mutate(House_age = ifelse(House_age==0,NA,House_age)) 

#New growth patches
new<-st_read('NewPatch.shp')%>% 
  sf::st_transform(., crs = 5070) %>% 
  st_make_valid(.) #fix topology error

#Remnant patches
rem<-st_read('RemnantPatch.shp') %>% 
  sf::st_transform(., crs = 5070) %>% 
  st_make_valid(.) #fix topology error

#2 km grid of Chi region
grid<-st_read('TwoKmPoly.shp')%>% 
  sf::st_transform(., crs = 5070) %>% 
  mutate(ID = row_number())

#Land use

lu<-st_read('C:/Users/ledarlin/Dropbox/Forest Composition/composition/Maps/shapefiles/LandUse/LUI15_shapefile_v1/LUStratify.shp')%>% 
  sf::st_transform(., crs = 5070) %>% 
  st_make_valid(.) #fix topology error

#Calculate acreage of forest intersection-------------
# 
# GridNew <- st_intersection(grid, new) %>% 
#   mutate(intersect_area = st_area(.)) %>%   # create new column with shape area
#   dplyr::select(ID, intersect_area) %>%   # only select columns needed to merge
#   mutate(intArea = as.numeric(intersect_area)) %>%  
#   group_by(ID) %>% #Dissolve area of cells that have several patches
#   summarize(NewArea=sum(intArea)) %>% 
#   st_drop_geometry(.) %>% 
#   as_data_frame(.)
# 
# GridRem <- st_intersection(grid, rem) %>% 
#   mutate(intersect_area = st_area(.)) %>%   # create new column with shape area
#   dplyr::select(ID, intersect_area) %>%   # only select columns needed to merge
#   mutate(intArea = as.numeric(intersect_area)) %>%  
#   group_by(ID) %>% #Dissolve area of cells that have several patches
#   summarize(RemArea=sum(intArea))%>% 
#   st_drop_geometry(.)%>% 
#   as_data_frame(.)

#Calculate census variables------------------

#Weighted average of density variables
census1<-st_interpolate_aw(census[c('TotPopD', 'WhtPopP', 'BlkPopP', 'AsnPopP', 
                                    'OthPopP', 'UnivP', 'PovP', 'UnplydP', 
                                    'HouseD', 'OwnerP', 'RenterP', 'House_age',
                                    'Med_ncm')], 
                  grid,extensive = FALSE, na.rm = TRUE) %>% 
  st_drop_geometry() 

#Area of land use types---------------------

Gridlu<-st_intersection(grid, lu) %>% 
  mutate(intersect_area = st_area(.)) %>%   # create new column with shape area
  dplyr::select(ID, NEW, intersect_area) %>%   # only select columns needed to merge
  mutate(intArea = as.numeric(intersect_area)) %>%  
  group_by(ID, NEW) %>% #Dissolve area of cells that have several patches
  summarize(sum(intArea)) %>% #Add intersect area
  st_drop_geometry(.) %>% 
  as_data_frame(.) %>% 
  spread(NEW, 'sum(intArea)') #Spread long data into columns for each lu

#Join together---------------------
  
gridNoGeo<-st_drop_geometry(grid) %>% #Drop geometry
  dplyr::select(-c("OID_", "Shape_Leng", "Shape_Area"))

df<-cbind(gridNoGeo, census1) %>% #join census variables
  left_join(., Gridlu, by = 'ID') #remnant forests

df[is.na(df)] = 0 #Replace NA with 0

df %<>% mutate(House_age = ifelse(House_age==0,NA,House_age), 
               Med_ncm = ifelse(Med_ncm==0,NA,Med_ncm)) #except for median income and house age

summary(df)
#write the csv----------------
write_csv(df, 'GriddedPatchData.csv')

#Maybe you wanted spatial data instead?
df2<-cbind(grid, census1) %>% 
  #cbind(.,census2) %>% 
  #left_join(., GridRem) %>% 
  #left_join(., GridNew, by = 'ID') %>% 
  left_join(., Gridlu, by = 'ID')

df2[is.na(df2)] = 0
df2%<>%   mutate(House_age = ifelse(House_age==0,NA,House_age)) %>% 
  mutate(Med_ncm = ifelse(Med_ncm==0,NA,Med_ncm))

summary(df2)

st_write(df2, 'GriddedPatchData.shp', driver = 'ESRI Shapefile', append = FALSE)


#Buffered patches------------------

#Maybe a grid isn't the best way to aggregate data. Let's see what's around
#each forest patch.

newBuff<- st_buffer(new, 1000) #1000m buffer
remBuff<-st_buffer(rem, 1000)

#Calculate buffer census variables------------------

#Weighted average of density variables
censusNewBuff<-st_interpolate_aw(census[c('TotPopD', 'WhtPopP', 'BlkPopP', 
                                    'AsnPopP', 'OthPopP', 'UnivP', 'PovP', 'UnplydP', 
                                    'HouseD', 'OwnerP', 'RenterP', 'House_age')], 
                           newBuff,extensive = FALSE, na.rm = TRUE) %>% 
  st_drop_geometry() 

#Weighted average of additive variable.
st_interpolate_aw(census[c('Med_ncm')], newBuff, extensive = TRUE) %>% 
  st_drop_geometry() -> censusNewBuff2


#Weighted average of density variables
censusRemBuff<-st_interpolate_aw(census[c('TotPopD', 'WhtPopP', 'BlkPopP', 'AsnPopP', 
                                    'OthPopP', 'UnivP', 'PovP', 'UnplydP', 
                                    'HouseD', 'OwnerP', 'RenterP', 'House_age')], 
                           remBuff,extensive = FALSE, na.rm = TRUE) %>% 
  st_drop_geometry() 

#Weighted average of additive variable.
st_interpolate_aw(census[c('Med_ncm')], remBuff, extensive = TRUE) %>% 
  st_drop_geometry() -> censusRemBuff2


#Area of buffer land use types---------------------

newBufflu<-st_intersection(newBuff, lu) %>% 
  mutate(intersect_area = st_area(.)) %>%   # create new column with shape area
  dplyr::select(ID, STRATIFIED, intersect_area) %>%   # only select columns needed to merge
  mutate(intArea = as.numeric(intersect_area)) %>%  
  group_by(ID, STRATIFIED) %>% #Dissolve area of cells that have several patches
  summarize(sum(intArea)) %>% #Add intersect area
  st_drop_geometry(.) %>% 
  as_tibble(.) %>% 
  spread(STRATIFIED, 'sum(intArea)') #Spread long data into columns for each lu

#Area of land use types---------------------

remBufflu<-st_intersection(remBuff, lu) %>% 
  mutate(intersect_area = st_area(.)) %>%   # create new column with shape area
  dplyr::select(ID, STRATIFIED, intersect_area) %>%   # only select columns needed to merge
  mutate(intArea = as.numeric(intersect_area)) %>%  
  group_by(ID, STRATIFIED) %>% #Dissolve area of cells that have several patches
  summarize(sum(intArea)) %>% #Add intersect area
  st_drop_geometry(.) %>% 
  as_tibble(.) %>% 
  spread(STRATIFIED, 'sum(intArea)') #Spread long data into columns for each lu


#Write
dfRemBuff<-cbind(remBuff, censusRemBuff) %>% 
  cbind(.,censusRemBuff2) 

dfRemBuff[is.na(dfRemBuff)] = 0
dfRemBuff%<>%   mutate(House_age = ifelse(House_age==0,NA,House_age)) %>% 
  mutate(Med_ncm = ifelse(Med_ncm==0,NA,Med_ncm))

summary(dfRemBuff)

st_write(dfRemBuff, 'remCensusBuff.shp', driver = 'ESRI Shapefile')

#Write
dfNewBuff<-cbind(newBuff, censusNewBuff) %>% 
  cbind(.,censusNewBuff2) 

dfNewBuff[is.na(dfNewBuff)] = 0
dfNewBuff%<>%   mutate(House_age = ifelse(House_age==0,NA,House_age)) %>% 
  mutate(Med_ncm = ifelse(Med_ncm==0,NA,Med_ncm))

summary(dfRemBuff)
