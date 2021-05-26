#Playing with my data
#Lindsay Darling
#4/20/2021

#Load libraries and data------------------

library(tidyverse)
library(tidylog)          #More verbose tidyverse
library(magrittr)         #Piping %>% 
library(ggfortify)        #For visualizing PCA
library(randomForest)
library(psych)            #Pretty pairs panel
library(vegan)            #NMDS
library(ggbiplot)

setwd("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject")

df<-read_csv('GriddedPatchData.csv')

df1 <- na.omit(df) #Get rid of na

df2<-df1

#Look at data comparison

#census and forest data
pairs.panels(df[,2:16])

#forest and land use

pairs.panels(df[,15:25])

#Standardize data

#Let's use an arcsine square root transformation on percentage values

#McCune and Grace asin(sqrt()) from Gord
asin.sqrt = function(x) {
  (2/pi)*asin(sqrt(x))
}

df1$WhtPopP<- asin.sqrt(df1$WhtPopP)
df1$BlkPopP<- asin.sqrt(df1$BlkPopP)
df1$AsnPopP<- asin.sqrt(df1$AsnPopP)
df1$OthPopP<- asin.sqrt(df1$OthPopP)
df1$UnivP<- asin.sqrt(df1$UnivP)
df1$PovP<- asin.sqrt(df1$PovP)
df1$UnplydP<- asin.sqrt(df1$UnplydP)
df1$OwnerP<- asin.sqrt(df1$OwnerP)
df1$RenterP<- asin.sqrt(df1$RenterP)

summary(df1)

#Run a PCA------------------

PCA1 <- prcomp(df1[,-1], scale. = TRUE)

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


#Check out random forest-----------------

rando<-randomForest(RemArea~.,df1[,-c(1,16)]) #Don't include plot ID

varImpPlot(rando, main = 'Remnant forest')

rando2<-randomForest(NewArea~.,df1[,-c(1,15,21)]) #Don't include plot ID

varImpPlot(rando2, main = 'New forest')

