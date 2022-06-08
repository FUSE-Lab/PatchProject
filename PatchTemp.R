#Temperature in of patches
#Lindsay Darling
#2/28/2022

#Load libraries------------

library(tidyverse)    
library(tidylog)    #Make tidyverse more verbose
library(sf)         #Spatial manipulation
library(raster)     #raster!
library(magrittr)   #Piping %>% 
library(spatialEco) #Ecological tools

setwd("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject")

#Load patch data
patch <- st_read("PatchWithTypeDissolve.shp") %>% 
  st_make_valid(.) %>% #Fix topology error
  mutate(areaM2 = st_area(.)) #add area

#Load temperature data

temp <- raster('D:/Dropbox/Forest Composition/composition/Maps/Temperature/RegionSept2017.tif')

#Calculate temp stats

pTemp <- zonal.stats(patch, temp, stats = c("min", "mean", "max")) %>% 
  rename(minTemp = `min.RegionSept2017`,
         meanTemp = `mean.RegionSept2017`,
         maxTemp = `max.RegionSept2017`)

patchTemp <- cbind(patch, patchTemp) %>% 
  st_drop_geometry(.)

ggplot(patchTemp, aes(x = PatchType, y = meanTemp, fill = PatchType))+
  geom_violin(trim = FALSE, show.legend = FALSE) +
  geom_boxplot(width = 0.2, fill = '#FDF7F1') +
  #stat_summary(fun.data = "mean_cl_boot", geom = "crossbar", color="#550135",
  #             fill = '#FDF7F1', width = 0.2) +
  scale_fill_manual(values=c("Remnant" = "#415c57", 
                             "Regrowth" = "#6baa35", "Novel" = '#fe941c')) +
  labs(title = 'Average surface temperature across patch types', 
       x = 'Patch type', y = 'Temperature (Degrees Celsius)') +
  guides(fill = guide_legend(title = "Forest type")) #+
#theme(legend.background = element_rect(fill = '#FDF7F1')) +
#theme(plot.background = element_rect(fill = "#FDF7F1"))

#Surrounding area data---------------

patchAtt <- st_read('PatchAttributesLandCover.shp') %>% 
  st_drop_geometry(.) 

#Join to patchTemp

patchTempLU <- patchTemp %>% 
  left_join(patchAtt)

TempTest<-lm(data = patchTemp, meanTemp ~ PatchType)
anova <- anova(TempTest) #Yes
par(mfcol=c(2,2))
plot(TempTest)
