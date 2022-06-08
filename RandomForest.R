
## Libraries------------------

library(tidyverse)
library(tidylog)
library(randomForest)  #Random forests
library(pdp)           #Partial dependence plots
library(vip)           #Variable importance plots

setwd("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject")

patches <- read_csv('GriddedPatchData.csv') %>% 
  drop_na(.)

## Random forest-------------


#Area of remnant forests

rando<-randomForest(NewArea~., data=patches[,-c(1,15,21)])


#Visualize importance
vip(rando, horizontal = TRUE, 
    aesthetics = list(fill = '#415c57')) +
  theme(plot.background = element_rect(fill = "#FDF7F1")) +
  labs(title = 'Variable importance in all tracts')


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

#Predicted vs. actual chart

#Bind the predicted and actual valeus and turn them into a data frame
PredAct<-cbind(tree2.pred.OS,y.test) %>% 
  as.data.frame(.)

#Plot them
par(mfcol=c(1,1))
ggplot(data=PredAct, aes(x=y.test, y = tree2.pred.OS)) + 
   xlab('Actual values') +
   ylab('Predicted values') +
   scale_x_continuous(labels = comma) +
   geom_point(color = '#6baa35') +
geom_smooth(method = 'lm', color ='#415c57') 

#Let's see what happened in the areas that were forested in preset era

forested<-df2 %>% filter(presetPrct >= 0.25)

rando2<-randomForest(PCan2010~., data=forested[,-c(16,17,18)])

vip(rando2, horizontal = TRUE, 
    aesthetics = list(fill = '#415c57')) +
  theme(plot.background = element_rect(fill = "#FDF7F1"))+
  labs(title = 'Variable importance in forested tracts')

varImpPlot(rando2, type=2)

#QQ plot for random forest 1 
plotres(rando2, which = 4)

#PDP values for random forest 1
Owner <- partial(rando2, pred.var = 'OwnerP')
HouseDe <- partial(rando2, pred.var = 'HouseD')
Poverty <- partial(rando2, pred.var = 'PovP')
Med_ncm <- partial(rando2, pred.var = 'Med_ncm')

#Plot them side by side
par(mfcol=c(1,3))
plot(Owner, xlab = 'Percent owner-occupied homes', ylab = 'Partial dependence')
plot(HouseDe, xlab = 'Housing density', ylab = 'Partial dependence')
plot(Poverty, xlab = 'Percent poverty', ylab = 'Partial dependence')

#And the areas that didn't have presettlement canopy

Noforested<-df2 %>% filter(presetPrct <= 0)

rando3<-randomForest(PCan2010~., data=Noforested[,-c(16,17,18)])

vip(rando3, horizontal = TRUE, 
    aesthetics = list(fill = '#415c57'))+
  theme(plot.background = element_rect(fill = "#FDF7F1"))

#Finally, areas that have and have not seen growth

Growth %<>%
  st_drop_geometry() %>%   
  dplyr::filter(growth == 'Y', #only areas with growth
                FIPS!= '17031030702') %>% #An outlier
  dplyr::select(-FIPS, -pctIncorp, -AreaHa, -Prct1940, -growth, -presetPrct) %>% 
  na.omit()  #Without this the step model won't work


rando6<-randomForest(PCan2010~., data=Growth[,-18])

vip(rando6, horizontal = TRUE, 
    aesthetics = list(fill = '#415c57'))+
  theme(plot.background = element_rect(fill = "#FDF7F1")) +
  labs(title = 'Variable importance in growth tracts')

#QQ plot for growth forest

plotres(rando6, which = 4)

#PDP values for growth forest
Owner <- partial(rando6, pred.var = 'OwnerP')
Med_ncm <- partial(rando6, pred.var = 'Med_ncm')
Poverty <- partial(rando6, pred.var = 'PovP')

#Plot them side by side
par(mfcol=c(1,3))
plot(Owner, xlab = 'Percent owner-occupied homes', ylab = 'Partial dependence')
plot(Med_ncm, xlab = 'Median income', ylab = 'Partial dependence')
plot(Poverty, xlab = 'Percent poverty', ylab = 'Partial dependence')

save(list=ls(all=T),file='ChicagoForestOriginFinal_0430.RData') 

```

