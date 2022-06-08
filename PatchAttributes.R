#Aggregating forest, land use, and census data to patches
#Lindsay Darling
#4/15/2021

#Load libraries------------

library(tidyverse)    
library(tidylog)    #Make tidyverse more verbose
library(magrittr)
library(sf)         #Spatial manipulation

#load data----------------

setwd("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject")

#Load patches with land use

#Note, this join was done in QGIS because R couldn't handle it.
#I turned the CMAP land use layer into a raster of the same resolution as the
#forest patch raster (3 m), then used tabulate area to calculate the area
#of each land use type within the half mile buffer of the patch

patchBuff <- st_read('BufferWLandUse.shp') %>% 
  sf::st_transform(., crs = 5070)

#Calculate census variables------------------

#2015 ACS data. Code for production elsewhere
census<-st_read('2015Clean.shp') %>% 
  sf::st_transform(., crs = 5070) %>% 
  #Change 0 in house age to NA
  mutate(House_age = ifelse(House_age==0,NA,House_age)) %>% 
  mutate(Med_ncm = ifelse(Med_ncm==0,NA,Med_ncm))

#Weighted average of density variables in new
censusPatch<-st_interpolate_aw(census[c('TotPopD', 'WhtPopP', 'BlkPopP', 'AsnPopP', 
                                        'OthPopP', 'UnivP', 'PovP', 'UnplydP', 
                                        'HouseD', 'OwnerP', 'RenterP', 'House_age',
                                        'Med_ncm')], 
                               patchBuff,extensive = FALSE, na.rm = TRUE) %>% 
  st_drop_geometry() 

rm(census)


#Join together---------------------

PatchLUCensus <- cbind(patchBuff, censusPatch)

#Clean up--------------------------
df <- PatchLUCensus %>% 
  st_drop_geometry(.) %>% 
  #Calculate percentage of each LU type
  mutate(totalArea = rowSums(.[7:16])) %>%  #First, get total area
  mutate(pResidentiall = `Residentia`/`totalArea`,
         pCommercial = `Commercial`/`totalArea`,
         pIndustrial = `Industrial`/`totalArea`,
         pInstitutional = `Institutio`/`totalArea`,
         pTransit = `Transit`/`totalArea`,
         pAg = `Ag`/`totalArea`,
         pOther = `Other`/`totalArea`,
         pResidential = `Water`/`totalArea`,
         pResidential = `Vacant`/`totalArea`,
         pOpen = `Open`/`totalArea`,) %>% 
  select(-Shape_Leng, #Drop extra things
         -Shape_Area,
         -allHalfMiB,
         -HISTO_NODA) %>% 
  rename(`PatchType` = `allHalfM_1`) 

summary(df)

#Land cover

landCover <- raster('D:/Dropbox/Forest Composition/composition/Maps/LandCover/landcover_2010_chicagoregion_illinois.img')


# Extraction function written by Quentin Red from SESYNC.

extractr_extract <- function(poly, ras, classes) {
  ext <- exactextractr::exact_extract(ras, poly)
  tab <- lapply(ext, function(x) sapply(classes, function(class) sum(x$coverage_fraction[x$value == class], na.rm = TRUE)))
  z <- do.call(rbind, tab)
  # out <- data.frame(plot_ID = poly$plot_ID, z)
  out <- data.frame(plot_ID = poly$row, z)
  setNames(out, c('plot_ID', 1:length(classes)))
}


PatchLandCover <- merge(df, extractr_extract(patchBuff, 
                                             landCover,classes = (1:7))) %>% 
  rename(Canopy = '1',
         Vegetation = '2',
         BareSoil = '3',
         Water = '4',
         Buildings = '5',
         Roads = '6',
         OtherPaved = '7') %>% 
  mutate(Impervious = `Buildings` +`Roads` + `OtherPaved`)
  

#write out----------------
write_csv(PatchLandCover, 'PatchAttributes.csv')

#Maybe you wanted spatial data instead?
df2<- patchBuff %>% 
  select(UniqueID) %>% 
  left_join(., df, by = 'UniqueID')

summary(df2)

st_write(df2, 'PatchAttributes.shp', driver = 'ESRI Shapefile')
