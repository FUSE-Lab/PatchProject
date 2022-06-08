#Area of patch types in census tracts
#Lindsay Darling
#2/24/2022

#Load libraries------------

library(tidyverse)    
library(tidylog)    #Make tidyverse more verbose
library(sf)         #Spatial manipulation
library(magrittr)   #Piping %>% 
library(glmnet)     #More glms

setwd("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject")

#Load patch data
patch <- st_read("PatchWithTypeDissolve.shp") %>% 
  st_make_valid(.) #Fix topology error

#Load project area

region <- st_read("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/Origin/2015/SevenCountyRegion.shp") %>%
  st_make_valid(.) %>% 
  sf::st_transform(., crs = 5070) %>% #Put into proper projection
  st_union() #dissolve boundaries


#zip code census data

census <- st_read("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/Origin/2015/2015CleanZip.shp") %>% 
  st_make_valid(.) %>% 
  sf::st_transform(., crs = 5070) %>% #Put into proper projection
  st_intersection(., region) #Clip to seven-county region
  

int <- st_intersection(patch, census) #Intersect patches with census data

int$PatchAreaM2 <- st_area(int) #Add patch area

int %<>%st_drop_geometry() #Drop spatial data. Pivot doesn't work with spatial data

#This works like a pivot table in Excel

patchSums <- int %>% 
  select('FIPS', 'PatchType', 'PatchAreaM2') %>%  #Select columns that I need
  pivot_wider(names_from = PatchType, #Column names
              values_from = PatchAreaM2, #Values to put in cells
              values_fn = sum) #Add all of the values within a tract/patchType

#Join back to the spatial data

censusPatch <- census %>% 
  left_join(., patchSums) %>% 
  mutate(novelRatio = as.numeric(Novel/AreaHa/10000), #Calculate ratios. Divide by 10000 to 
         remRatio = as.numeric(Remnant/AreaHa/10000), #convert Ha to m2. Change from units to number
         regRatio = as.numeric(Regrowth/AreaHa/10000)) %>% 
  #Add column with low median high income
  mutate(incomeLevel = as.numeric(cut_number(Med_ncm, 3))) %>% #Split into three even groups
  mutate(incomeLevel=recode(incomeLevel, '1' = 'low', #Rename to something clearer
                            '2' = 'medium',
                            '3' = 'high')) 
    
  
#View(censusPatch) #Check results

#Patch distribution is not normal. Need to fix that.
#They are zero inflated. Need to change NA to minimum column value

#I can't make this work in tidyverse, so Base R it is
censusPatch$novelRatio[is.na(censusPatch$novelRatio)] <- 
  min(censusPatch$novelRatio, na.rm = T) #Change NA to minimum value

censusPatch$remRatio[is.na(censusPatch$remRatio)] <- 
  min(censusPatch$remRatio, na.rm = T) #Change NA to minimum value

censusPatch$regRatio[is.na(censusPatch$regRatio)] <- 
  min(censusPatch$regRatio, na.rm = T) #Change NA to minimum value

#Now, log transform to turn ratio into something more normal

#This is a function for arcsin transform, but I think log works better here.
#McCune and Grace asin(sqrt()) from Gord
# asin.sqrt = function(x) {
#   (2/pi)*asin(sqrt(x))
# }

censusPatch$novelRatio <- log(censusPatch$novelRatio)

censusPatch$remRatio <- log(censusPatch$remRatio)

censusPatch$regRatio <- log(censusPatch$regRatio)


#Write it out

st_write(censusPatch, 'ZipPatchTypeArea.shp', driver = 'ESRI Shapefile', append = FALSE)


#Regressions--------

censusPatch <- st_read('ZipPatchTypeArea.shp')

#Convert data back to long form for visualizations
long <- censusPatch %>% 
  pivot_longer(cols = c(novelRatio, remRatio, regRatio),
               names_to = 'PatchType') %>% 
  mutate(patchRatio = as.numeric(value))

#Violin plots. Not super clear, but higher income groups have more rem and reg
#patches. All groups are pretty even on novel.
#TODO: fix order of groups to low, medium, high.

ggplot(long, aes(x = PatchType, y = patchRatio, fill = incomeLevel))+
  geom_violin(trim = FALSE, show.legend = TRUE) +
  #geom_boxplot(width = 0.2, fill = '#FDF7F1') +
  scale_fill_manual(values=c("low" = "#415c57", 
                             "medium" = "#6baa35", "high" = '#fe941c')) +
  labs(title = 'Coverage of forest patch types by income level', 
       x = 'Patch type', y = 'Ratio forested (log)') +
  guides(fill = guide_legend(title = "Income level")) #+
#theme(legend.background = element_rect(fill = '#FDF7F1')) +
#theme(plot.background = element_rect(fill = "#FDF7F1"))


#And the messiest scatterplot ever. Again, higher income zips have more rem and 
#reg forests and novel is consistent across.
ggplot(long, aes(Med_ncm, patchRatio, shape=PatchType, colour=PatchType, fill=PatchType)) +
  geom_smooth(method="lm") +
  geom_point(size=3) +
  theme_bw() + 
  xlab("Median income") +
  ylab("Patch area (log)") +
  expand_limits(y=0)

#Linear regressions to see which variables are predictive

lmNovel <- glm(data = censusPatch, novelRatio~Med_ncm + House_g + Med_rnt +
               WhtPopP + OwnerP + TotPopD)


summary(lmNovel)

lmRemnant <- glm(data = censusPatch, remRatio~Med_ncm + House_g + Med_rnt +
               WhtPopP + OwnerP + TotPopD)

summary(lmRemnant)

lmReg <- glm(data = censusPatch, regRatio~ Med_ncm + House_g + Med_rnt +
               WhtPopP + OwnerP + TotPopD)

summary(lmReg)
