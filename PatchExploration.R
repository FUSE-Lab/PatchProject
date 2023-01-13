#Playing with my data
#Lindsay Darling
#Started 4/20/2021

#This project includes a lot of half-baked methods to try to 
#wrangle and analyze my data. There is a lot of work needed
#to clean this up.

#Load libraries and data------------------

library(tidyverse)
library(tidylog)          #More verbose tidyverse
library(magrittr)         #Piping %>% 
library(ggfortify)        #For visualizing PCA
library(randomForest)     #Random forest
library(vegan)            #NMDS
#library(ggbiplot)
library(lattice)          #Side-by-side figures
#library(MASS)
#library(pscl) # alternatively can use package ZIM for zero-inflated models
#library(lmtest)
#library(mgcv)
#library(glmmTMB)
library(sf)             #Spatial manipulation
#library(viridis)
library(units)          #Better axes
#library(DHARMa)
#library(glmnet)
library(scales)        #Rescaling data
library(vip)           #Prettier, easier vip plots
library(VSURF)         #Variable reduction for RF


setwd("C:/Users/ledarlin/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject")

#prepare data----

grid<-st_read('GriddedPatchData.shp') 

region <- grid %>% 
  st_union(.) #dissolve boundaries

#Load patch data
patch1 <- st_read('ChiPatchN.shp') %>% 
  st_make_valid(.) %>% 
  sf::st_transform(., crs = 5070) 

#Clip to region
patchClip <-  st_intersection(st_make_valid(patch1), region)

#Add area
patchClip$area <- st_area(patchClip)

#Check it out
dim(patchClip) #63432 patches
summary(patchClip)

#Patches that have already been intersected with historic layers
patch <- st_read("PatchWithTypeDissolve.shp") %>% 
  st_make_valid(.) #Fix topology error

#Clip to region  
patchClip2 <- st_intersection(st_make_valid(patch), region)

#Add area
patchClip2$area <- st_area(patchClip2) 

#Summarize by patch type
patchClip2 %>%  
  st_drop_geometry(.) %>% 
  group_by(PatchType) %>% 
  summarize(value = sum(area))

#Calculate patches in grids

int <- st_intersection(patch, grid) #Intersect patches with census data

int$PatchAreaM2 <- st_area(int) #Add patch area

int %<>%st_drop_geometry() #Drop spatial data

#This works like a pivot table in Excel

patchSums <- int %>% 
  dplyr::select('ID', 'PatchType', 'PatchAreaM2') %>%  #Select columns that I need
  pivot_wider(names_from = PatchType, #Column names
              values_from = PatchAreaM2, #Values to put in cells
              values_fn = sum) #Add all of the values within a tract/patchType

#This normalized the data in a couple of ways for adventures
#in stats
patchGrid <- left_join(grid, patchSums, by = 'ID') %>%
  mutate(Novel2 = drop_units(Novel),
         Regrowth2 = drop_units(Regrowth),
         Remnant2 = drop_units(Remnant),
         Novel0 = replace_na(Novel2, 0),
         Regrowth0 = replace_na(Regrowth2, 0),
         Remnant0 = replace_na(Remnant2, 0),
         remRatio = Remnant2/4000000,
         regRatio = Regrowth2/4000000,
         novelRatio = Novel2/4000000,
         novelMin = ifelse(is.na(Novel2), min(Novel2, na.rm = TRUE), Novel2),
         regMin = ifelse(is.na(Regrowth2), min(Regrowth2, na.rm = TRUE), Regrowth2),
         remMin = ifelse(is.na(Remnant2), min(Remnant2, na.rm = TRUE), Remnant2),
         incomeLevel = as.numeric(cut_number(Med_ncm, 3)),
         whiteLevel = as.numeric(cut_number(WhtPopP, 3)),
         remLogMin = log(remMin),
         regLogMin = log(regMin),
         novelLogMin = log(novelMin),
         TotPop = TotPopD*400,
         novelCapita = log(as.numeric(Novel/TotPop)),
         regCapita = log(as.numeric(Regrowth/TotPop)),
         remCapita = log(as.numeric(Remnant/TotPop))) %>% 
  filter(TotPop > 0)

#Core and edge------

core <- st_buffer(patch, (-15)) %>%   #remove edge area
  dplyr::select(-PatchType) %>% 
  mutate(type = 'core')

coreint <- st_intersection(core, grid)#Intersect cores with census data

coreint$PatchAreaM2 <- st_area(coreint) #Add core area

coreint %<>%st_drop_geometry() #Drop spatial data


#This works like a pivot table in Excel

coreSums <- coreint %>% 
  dplyr::select('ID', 'type', 'PatchAreaM2') %>%  #Select columns that I need
  group_by(ID) %>%  #Values to put in cells
  summarize(core = sum(PatchAreaM2)) #Add all of the values within a tract/patchType

df <- left_join(patchGrid, coreSums, by = 'ID') %>% 
  mutate(edge = Novel + Regrowth + Remnant - core,
         core0 = replace_na(as.numeric(core, 0)),
         edge0 = replace_na(as.numeric(edge, 0)),
         Regrowth0 = replace_na(as.numeric(Regrowth2, 0)),
         Regrowth0 = replace_na(Regrowth2, 0),
         edgeCapita = log(as.numeric(edge)),
         coreCapita = log(as.numeric(core)))

#write_csv(df, 'griddedData.csv')

#Groups and anova--------

#You can skip all of the early stuff and just read this guy
df <- read_csv('griddedData.csv')

#Make long version for visualization
long <- df %>% 
  dplyr::select(novelCapita, remCapita, regCapita, incomeLevel, Med_ncm, WhtPopP) %>%
  rename(Novel = novelCapita,
         Regrowth = regCapita,
         Remnant = remCapita) %>% 
  pivot_longer(cols = c(Novel, Remnant, Regrowth),
               names_to = 'PatchType') %>% 
  mutate(patchRatio = as.numeric(value)) %>% 
  filter(value <=4000)

# ggplot(long, aes(x = PatchType, y = patchRatio, fill = incomeLevel))+
#   geom_boxplot(show.legend = TRUE) +
#   scale_fill_manual(values=c("1" = "#c9a22f", 
#                              "2" = "#9dc473", "3" = '#53813d')) +
#   labs(title = 'Coverage of forest patch types by income level', 
#        x = 'Patch type', y = 'Log of ratio forested') +
#   guides(fill = guide_legend(title = "Income level")) +
#   #ylim(-10, 0) +
#   theme(legend.position = c(.25,.9),
#         legend.direction = 'horizontal') 

#Access to patch type by whiteness
whtPatch <- ggplot(long, aes(WhtPopP, patchRatio, colour=PatchType, fill=PatchType)) +
 # geom_point(size=3, alpha = 0.2) +
  geom_smooth(aes(colour = PatchType), method="lm", alpha = 0.2) +
  scale_colour_manual(values=c("Remnant" = "#b2a594", 
                             "Regrowth" = "#d56639", "Novel" = '#59B6BE')) +
  scale_fill_manual(values=c("Remnant" = "#b2a594", 
                             "Regrowth" = "#d56639", "Novel" = '#59B6BE')) +
  xlab("Percent white population") +
  ylab("Log of patch area per capita") +
  scale_x_continuous(labels = scales::percent) +
  theme(legend.position = c(.8,.2),
        text = element_text(size = 24))

whtPatch

#Is significant?
lmRem <- glm(novelCapita ~ WhtPopP, data = df)
summary(lmRem)
anova(lmRem)
with(summary(lmRem), 1 - deviance/null.deviance)

#Save it out. This is poster res right now
ggsave('C:/Users/ledarlin/Dropbox/LindsayWorking/GradSchool/Dissertation/Figures/PatchWhtRegression.png', 
       whtPatch, width = 9, height = 6, unit = "in", dpi = 300)

#Core plot

#Make long for vis
long2 <- df %>% 
  dplyr::select(coreCapita, edgeCapita, whiteLevel, Med_ncm, WhtPopP) %>%
  rename(Core = coreCapita,
         Edge = edgeCapita) %>% 
  pivot_longer(cols = c(Core, Edge),
               names_to = 'Patch section') 

#Edge/core by whiteness
whtCore <- ggplot(long2, aes(WhtPopP, value, colour=`Patch section`, fill=`Patch section`)) +
  geom_smooth(aes(colour = `Patch section`), method="lm", alpha = 0.2) +
  scale_colour_manual(values=c("Core" = "#53813d", 
                               "Edge" = "#c9a22f")) +
  #geom_point(size = 3) +
  scale_fill_manual(values=c("Core" = "#53813d", 
                             "Edge" = "#c9a22f")) +
  xlab("Percent white population") +
  ylab("Log of forest cover per capita") +
  scale_x_continuous(labels = scales::percent) +
  theme(legend.position = c(.8,.2),
        text = element_text(size = 24))

whtCore

#Save, poster res.
ggsave('C:/Users/ledarlin/Dropbox/LindsayWorking/GradSchool/Dissertation/Figures/CoreWhtRegression.png', 
       whtCore, width = 9, height = 6, unit = "in", dpi = 300)


Box <- ggplot(long2, aes(x = WhtPopP, y = value, fill = type))+
  geom_boxplot() 
  #scale_fill_manual(values=c("1" = "#c9a22f", 
  #                           "2" = "#9dc473", "3" = '#53813d')) +
  labs(title = 'Coverage of forest patch types by income level', 
       x = 'Patch type', y = 'Log of ratio forested') +
  guides(fill = guide_legend(title = "Income level")) +
  #ylim(-10, 0) +
  theme(legend.position = c(.25,.9),
        legend.direction = 'horizontal')

#Linear models---------

lmNovel <- lm(novelCapita ~ TotPopD+Med_ncm+House_g+OwnerP+WhtPopP+Resdntl, 
               family = poisson, data = df)

summary(lmNovel)
coef(lmNovel)

lmReg <- lm(regCapita ~ TotPopD+Med_ncm+House_g+OwnerP+WhtPopP+Resdntl,
             family = poisson, data = df)

summary(lmReg)
coef(lmReg)

lmRem <- lm(remCapita ~ TotPopD+Med_ncm+House_g+OwnerP+WhtPopP+Resdntl,
            family = gaussian, data = df)

summary(lmRem)
coef(lmRem)

lmGrouped <- lm(remRatioReg ~ incomeLevel, data = censusPatch)

anova(lmGrouped)



#Look at data comparison

#census and forest data
pairs.panels(df[,2:16])

#forest and land use

pairs.panels(df[,15:25])

summary(df)

#Run a PCA------------------

PCA1 <- prcomp(df[,-1], scale. = TRUE)

plot(PCA1, type="lines") 

ggplot2::autoplot(stats::prcomp(df1, scale=TRUE), label = FALSE, loadings.label = TRUE)

ggbiplot::ggbiplot(PCA1, choices = c(1,3))

biplot(PCA1,choices=c(1,3))

summary(PCA1)

#What about without the landuse variables?

PCA2 <- prcomp(df1[,2:16], scale.=TRUE)

ggplot2::autoplot(stats::prcomp(df1[,2:16], scale=TRUE), label = FALSE, loadings.label = TRUE)

biplot(PCA2,choices=c(1,3))

#White and ownership strongly related and opposed to black, poverty, transit, vacant
#industrial, etc. New and remnant forest patches strongly correlated and associated
#with white owners. Median income is confusing.

#NMDS----------------

NMDS1 <- metaMDS(df2[,2:16], k=3, trymax=20)

NMDS2 <- metaMDS(df2[,17:25], k=3, trymax=20) 

NMDS3 <- metaMDS(df2, k=3, trymax=20) 


plot(NMDS1, type = 't') #ugly plot

plot(NMDS3, type = 't') #ugly plot

#Try a prettier one

#Create df with scores for each grid cell
data.scores <- as.data.frame(scores(NMDS3))  #Using the scores function from vegan to extract the site scores and convert to a data.frame

#df with scores for each variable
var.scores <- as_tibble(scores(NMDS3, "variable")) %>% 
  mutate(variable = colnames(df2))

ggplot(data.scores, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(size = 1, color = '#6baa35')+ 
  labs(x = "NMDS1", y = "NMDS2") +
  geom_text(data=var.scores,aes(x=NMDS1,y=NMDS2,label=variable), 
            color ='#232937', size = 4 ) 
#theme(plot.background = element_rect(fill = "#FDF7F1"))

#Random forest---------
rfData <- st_drop_geometry(df)

rfData[is.na(rfData)] <- 0

set.seed(456)
randoNovel<-randomForest(Novel~., data=rfData[,c(5:30)])
randoNovel #30.69% of variance

#Visualize importance
vip(randoNovel, horizontal = TRUE, 
    aesthetics = list(fill = '#59B6BE')) +
  theme_bw() +
  #theme(plot.background = element_rect(fill = "#FDF7F1")) +
  labs(title = 'Variable importance of novel forests (31% of variance)')

randoRegrowth<-randomForest(Regrowth~., data=rfData[,c(5:29,31)])
randoRegrowth #46%

#Visualize importance
vip(randoRegrowth, horizontal = TRUE, 
    aesthetics = list(fill = '#d56639')) +
  theme_bw() +
  #theme(plot.background = element_rect(fill = "#FDF7F1")) +
  labs(title = 'Variable importance of regrowth forests (46% of variance)')

randoRemnant<-randomForest(Remnant~., data=rfData[,c(5:29,32)])
randoRemnant #47%

#Visualize importance
vip(randoRemnant, horizontal = TRUE, 
    aesthetics = list(fill = '#b2a594')) +
  theme_bw() +
  #theme(plot.background = element_rect(fill = "#FDF7F1")) +
  labs(title = 'Variable importance of remnant forests (47% of variance)')

#variable refinement------

vfNovel <- VSURF(novelCapita ~ ., data = rfData[,c(5:29,51)], na.action = na.omit)

summary(vfNovel)

#Rerun with fewer variables

randoNovel2<-randomForest(novelCapita ~., data=rfData[,c((vfNovel$varselect.interp+4),51)], mtry = 6)
randoNovel2

#Visualize importance
vip(randoNovel2, horizontal = TRUE, 
    aesthetics = list(fill = '#59B6BE')) +
  theme_bw() +
  #theme(plot.background = element_rect(fill = "#FDF7F1")) +
  labs(title = 'Variable importance of novel forests (35% of variance)')

NovelPopD <- pdp::partial(randoNovel2, pred.var = 'TotPopD')
NovelHouseD <- pdp::partial(randoNovel2, pred.var = 'HouseD')
NovelNatArea <- pdp::partial(randoNovel2, pred.var = 'Natrlar')

#Plot them side by side
par(mfcol=c(1,3))

plot(NovelPopD, xlab = 'Population density', ylab = 'Partial dependence')
plot(NovelHouseD, xlab = 'Housing density', ylab = 'Partial dependence')
plot(NovelNatArea, xlab = 'Natural area density', ylab = 'Partial dependence')

#Regrowth

vfRegrowth <- VSURF(regCapita ~ ., data = rfData[,c(5:29,52)], na.action = na.omit)

summary(vfRegrowth)

randoRegrowth2<-randomForest(regCapita ~., data=rfData[,c((vfRegrowth$varselect.interp+4),52)], mtry = 6)
randoRegrowth2

vip(randoRegrowth2, horizontal = TRUE, 
    aesthetics = list(fill = '#d56639')) +
  theme_bw() +
  #theme(plot.background = element_rect(fill = "#FDF7F1")) +
  labs(title = 'Variable importance of regrowth forests (54% of variance)')

RegrowthPopD <- pdp::partial(randoRegrowth2, pred.var = 'TotPopD')
RegrowthNatArea <- pdp::partial(randoRegrowth2, pred.var = 'Natrlar')
RegrowthHouseD <- pdp::partial(randoRegrowth2, pred.var = 'HouseD')

#Plot them side by side
par(mfcol=c(1,3))

plot(RegrowthPopD, xlab = 'Population density', ylab = 'Partial dependence')
plot(RegrowthNatArea, xlab = 'Natural area density', ylab = 'Partial dependence')
plot(RegrowthHouseD, xlab = 'Housing density', ylab = 'Partial dependence')

#remnant

vfRemnant <- VSURF(remCapita ~ ., data = rfData[,c(5:29,53)], na.action = na.omit)

summary(vfRemnant)

#Rerun with fewer variables

randoRemnant2<-randomForest(remCapita ~., data=rfData[,c((vfRemnant$varselect.interp+4),53)], mtry = 6)
randoRemnant2

vip(randoRemnant2, horizontal = TRUE, 
    aesthetics = list(fill = '#b2a594')) +
  theme_bw() +
  #theme(plot.background = element_rect(fill = "#FDF7F1")) +
  labs(title = 'Variable importance of remnant forests (57% of variance)')


RemPopD <- pdp::partial(randoRemnant2, pred.var = 'TotPopD')
RemNatArea <- pdp::partial(randoRemnant2, pred.var = 'Natrlar')
RemHouseD <- pdp::partial(randoRemnant2, pred.var = 'HouseD')

#Plot them side by side
par(mfcol=c(1,3))

plot(RemPopD, xlab = 'Population density', ylab = 'Partial dependence')
plot(RemNatArea, xlab = 'Natural area density', ylab = 'Partial dependence')
plot(RemHouseD, xlab = 'Housing density', ylab = 'Partial dependence')

#PDP plot ---------------

Owner <- pdp::partial(randoNovel2, pred.var = 'Resdntl')
HouseAge <- pdp::partial(rando2, pred.var = 'House_age')
presetPrct <- pdp::partial(rando2, pred.var = 'presetPrct')

#Plot them side by side
par(mfcol=c(1,3))

plot(Owner, xlab = 'Percent owner-occupied homes', ylab = 'Partial dependence')
plot(HouseAge, xlab = 'Median house age', ylab = 'Partial dependence')
plot(presetPrct, xlab = 'Percent 1830 forest', ylab = 'Partial dependence')

#Test the new and old versions

#First, create a data frame to store the results
result4 <- data.frame(matrix(nrow = 5, ncol = 7))
colnames(result4) <- c("Fold", "OrigRFIS", "OrigRFOS", "r2", "RefinRFIS", "RefinRFOS", "r2")

for(i in 1:10){
  #Selects everything except fold i
  df.train <- df2 %>% filter(fold != i) %>% dplyr::select(-fold)
  # selects fold i
  df.test  <- df2 %>% filter(fold == i) %>% dplyr::select(-fold)
  # Create the random forest tree
  rando <- randomForest(PCan2010~., data=df.train[,-c(18)])
  #IS data tree
  tree1.pred.IS <- predict(rando, newdata=df.train) 
  #RMSE since it's not categorical
  InSamp2<- sqrt(mean((tree1.pred.IS-df.train$PCan2010)^2)) 
  #Repeat the above three steps with OS data
  tree1.pred.OS <- predict(rando, newdata=df.test) 
  #Print the results to our blank data frame
  result4[i,1] <- i        
  result4[i,2] <- InSamp
  result4[i,3] <- OutSamp
  result4[i,4] <- cor(tree1.pred.OS,df.test$PCan2010)
  # Create the random forest tree
  rando2 <- randomForest(PCan2010~., data=df.train[,c(vf$varselect.pred)])
  #IS data tree
  tree2.pred.IS <- predict(rando2, newdata=df.train) 
  #RMSE since it's not categorical
  InSamp2<- sqrt(mean((tree2.pred.IS-df.train$PCan2010)^2)) 
  #Repeat the above three steps with OS data
  tree2.pred.OS <- predict(rando2, newdata=df.test) 
  OutSamp2 <- sqrt(mean((tree2.pred.OS-df.test$PCan2010)^2))
  result4[i,5] <- InSamp2
  result4[i,6] <- OutSamp2
  result4[i,7] <- cor(tree2.pred.OS,df.test$PCan2010)
}

RMSE4<-colMeans(result4)

RMSE4

abs(RMSE4 - NullTrain)/NullTrain
abs(RMSE4 - NullTest)/NullTest

ggplot(data=df2, aes(x=Prct1940, y = PCan2010)) + 
  xlab('Percent of tract forested in 1940') +
  ylab('Percent tree canopy in 2010') +
  geom_point(color = '#6baa35') +
  geom_smooth(method = 'lm', color ='#415c57') +
  stat_regline_equation(label.y = .80, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = .75, aes(label = ..rr.label..)) #+
#theme(plot.background = element_rect(fill = "#FDF7F1"))

ggplot(df2, aes(x=Prct1940)) +
  geom_histogram(color = "#415c57", fill = "#415c57") +
  xlab('Percent forested in 1940') +
  ylab('Count of census tracts') 


