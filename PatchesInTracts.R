#Area of patch types in census tracts
#Lindsay Darling
#2/24/2022

#Load libraries------------

library(tidyverse)    
library(tidylog)    #Make tidyverse more verbose
library(sf)         #Spatial manipulation
library(magrittr)   #Piping %>% 
library(broom)
library(car)
library(spatialreg)
library(knitr)
library(stargazer)
library(randomForest)
library(vip)
library(glmmfields)
library(units)
library(ggridges)

setwd("C:/Users/ledarlin/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject")

#Load patch data
patch <- st_read("PatchWithTypeDissolve.shp") %>% 
  st_make_valid(.) #Fix topology error

census <- st_read("C:/Users/ledarlin/Dropbox/Forest Composition/composition/Maps/shapefiles/Origin/2015/2015Clean.shp") %>% 
  st_make_valid(.) %>% 
  sf::st_transform(., crs = 5070) #Put into proper projection

int <- st_intersection(patch, census) #Intersect patches with census data

int$PatchAreaM2 <- st_area(int) #Add patch area

int %<>%st_drop_geometry() #Drop spatial data

#This works like a pivot table in Excel

patchSums <- int %>% 
  select('FIPS', 'PatchType', 'PatchAreaM2') %>%  #Select columns that I need
  pivot_wider(names_from = PatchType, #Column names
              values_from = PatchAreaM2, #Values to put in cells
              values_fn = sum) #Add all of the values within a tract/patchType

#Join back to the spatial data

censusPatch <- census %>% 
  left_join(., patchSums) %>% 
  mutate(novelRatio = drop_units(Novel/AreaHa/10000), #Calculate ratios. Divide by 10000 to 
         remRatio = drop_units(Remnant/AreaHa/10000), #convert Ha to m2. Change from units to number
         regRatio = drop_units(Regrowth/AreaHa/10000),
         TotPop = TotPopD*AreaHa) %>% 
  #Add column with low median high income
  mutate(incomeLevel = as.numeric(cut_number(Med_ncm, 3)), #Split into three even groups
        novelRatio = ifelse(is.na(novelRatio), min(novelRatio, na.rm = TRUE), novelRatio),
        regRatio = ifelse(is.na(regRatio), min(regRatio, na.rm = TRUE), regRatio),
        remRatio = ifelse(is.na(remRatio), min(remRatio, na.rm = TRUE), remRatio),
        NovelLog = log(novelRatio),
        RegrowthLog = log(regRatio),
        RemnantLog = log(remRatio),
        novelCapita = as.numeric(log(Novel/TotPop)),
        regCapita = as.numeric(log(Regrowth/TotPop)),
        remCapita = as.numeric(log(Remnant/TotPop)),
        incomeLevel = as.character(incomeLevel)) #Replace NA with 0

#I don't know why this doesn't work
# censusPatch%<>%
#   mutate(incomeLevel = as.character(incomeLevel), 
#          incomeLevel = recode(incomeLevel, 
#                               `1` = 'Low',
#                               `2` = 'Medium',
#                               `3` = 'High')) 


#Write it out

#st_write(censusPatch, 'CensusPatchTypeArea.shp', driver = 'ESRI Shapefile', append = TRUE)


#Regressions--------

#Don't recommend reading this in. The .shp shortens the names and makes the code not work.
#censusPatch <- st_read('CensusPatchTypeArea.shp')

long <- censusPatch %>%
  select(novelCapita, remCapita, regCapita, incomeLevel, Med_ncm) %>%
  rename(Novel = novelCapita,
         Regrowth = regCapita,
         Remnant = remCapita) %>% 
  pivot_longer(cols = c(Novel, Remnant, Regrowth),
               names_to = 'PatchType') %>% 
  mutate(patchRatio = as.numeric(value))


ggplot(long, aes(x = PatchType, y = patchRatio, fill = incomeLevel))+
  geom_boxplot(show.legend = TRUE) +
  scale_fill_manual(values=c("1" = "#c9a22f", 
                             "2" = "#9dc473", "3" = '#53813d')) +
  labs(title = 'Coverage of forest patch types by income level', 
       x = 'Patch type', y = 'Log of ratio forested') +
  guides(fill = guide_legend(title = "Income level")) +
  #ylim(-10, 0) +
  theme(legend.position = c(.25,.9),
        legend.direction = 'horizontal') 
  
    #theme(legend.background = element_rect(fill = '#FDF7F1')) +
  #theme(plot.background = element_rect(fill = "#FDF7F1")) 

#Get ready for some ugly
ggplot(long, aes(Med_ncm, value, shape=PatchType, colour=PatchType, fill=PatchType)) +
  geom_smooth(method="lm") +
  geom_point(size=3) +
  theme_bw() + 
  xlab("Median income") +
  ylab("Patch ratio") +
  expand_limits(y=0) 

#Linear models---------

lmNovel <- glm(novelCapita ~ TotPopD+Med_ncm+House_age+Med_rent+OwnerP, 
               family = gaussian, data = censusPatch)

summary(lmNovel)

lmReg <- glm(regCapita ~ TotPopD+Med_ncm+House_age+Med_rent+OwnerP,
             family = gaussian, data = censusPatch)

summary(lmReg)

lmRem <- lm(remRatio ~ TotPopD+Med_ncm+House_age+Med_rent+OwnerP,
             family = gaussian, data = censusPatch)

anova(lmRem)
coef(lmRem)

lmGrouped <- lm(remRatioReg ~ incomeLevel, data = censusPatch)

anova(lmGrouped)

lmNovelGroup <- long %>%
  filter(PatchType == 'novelRatioReg') %>% 
  lm(patchRatio ~ incomeLevel, data = .)

anova(lmNovelGroup)

#Spatial models--------



#Random forest---------

rfData <- st_drop_geometry(censusPatch)

rfData[is.na(rfData)] <- 0


rando<-randomForest(novelRatio~., data=rfData[,c(2:4, 6:12, 14:16,20)])
rando

#Visualize importance
vip(rando, horizontal = TRUE, 
    aesthetics = list(fill = '#415c57')) +
  theme(plot.background = element_rect(fill = "#FDF7F1")) +
  labs(title = 'Variable importance of remnant forests in all tracts')


#PDP values for random forest 1
Ag <- partial(rando, pred.var = 'Ag')
Transit <- partial(rando, pred.var = 'Transit')
TotPopD <- partial(rando, pred.var = 'TotPopD')
Med_ncm <- partial(rando, pred.var = 'Med_ncm')

#Plot them side by side
par(mfcol=c(1,4))
plot(Ag, xlab = 'Ag', ylab = 'Partial dependence')
plot(Transit, xlab = 'Transit', ylab = 'Partial dependence')
plot(TotPopD, xlab = 'Population density', ylab = 'Partial dependence')
plot(Med_ncm, xlab = 'Median income', ylab = 'Partial dependence')

