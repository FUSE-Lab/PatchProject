#NMDS of tree species by patch type

#Libraries------------

rm(list=ls())

library(tidyverse)
library(tidylog)
library(magrittr)
library(vegan)
library(ggplot2)

setwd("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject")

#Core plots----------

#This file has plots, the patch type, and BA of each species in the columns

df<-read_csv('coreSpeciesBA20.csv') %>% 
 select(-X72)

#If you want to run genus instead of species, load this.

# df<-read_csv('coreGenusBA20.csv') %>% 
#    select(-X41)

#Run the NMDS. Two axes, try 1000 times

set.seed(123) #Make it reproducible
NMDS<-metaMDS(comm = df[,-c(1:2)], distance = 'bray', k = 2, try = 5000)

#Check results
NMDS

#Ugly plot
ordiplot(NMDS, type = 't')

#Make a prettier plot using instructions from
#https://chrischizinski.github.io/rstats/vegan-ggplot2/

#Create group variable
grp<-df$patchType

#And plot variable
plot<-df$PlotID

#Pull data from NMDS results
data.scores <- as.data.frame(scores(NMDS))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- plot  # create a column of site names, from the rownames of data.scores
data.scores$grp <- grp  #  add the grp variable created earlier
head(data.scores)  #look at the data

#Pull species data
species.scores <- as.data.frame(scores(NMDS, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)  #look at the data


#Make the plot. I removed the plot names because I thought it was hard to read.

ggplot() +
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=grp,colour=grp),size=3) + # add the point markers
  #geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
  scale_colour_manual(values=c("Remnant" = "#415c57", "Regrowth" = "#6baa35", "Novel" = '#fe941c')) +
  coord_equal() +
  theme_bw() +
  stat_ellipse(data = data.scores, aes(x=NMDS1,y=NMDS2, colour=grp))

#Remnant patches cluster to the left, but there is not good resolution between
#regrowth and novel patches.

#Core fit------------

env <- df[,2]

en <- envfit(NMDS, env, permutations = 999, na.rm = TRUE)

en

#Patch type is a highly significant predictor of species composition
#in forest cores

#Edge plots-----------------
#Let's look at the composition of plots that are on the edge of patches

#This file has plots, the patch type, and BA of each species in the columns
df<-read_csv('edgeSpeciesBA20.csv') %>% 
  filter(PlotID != 6125) %>%  #This plot is all white pine and is ruining everything.
  filter(PlotID != 665) %>% 
  select(-X96) #%>% 
  select(-c('Rhamnus cathartica', 'Lonicera maackii')) #Remove invasives
  
#If you want to run genus instead of species, load this.
  
  df<-read_csv('edgeGenusBA20.csv') %>% 
    select(-X48) %>% 
    filter(PlotID != 6125) %>% 
    filter(PlotID != 665)

#Run the NMDS. Two axes, try 250 times

set.seed(123) #Make it reproducible
NMDS<-metaMDS(comm = df[,-c(1:2)], distance = 'bray', k = 2, try = 1000)

#Check results
NMDS

#Ugly plot
ordiplot(NMDS, type = 't')

#Make a prettier plot using instructions from
#https://chrischizinski.github.io/rstats/vegan-ggplot2/

#Create group variable
grp<-df$patchType

#And plot variable
plot<-df$PlotID

#Pull data from NMDS results
data.scores <- as.data.frame(scores(NMDS))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- plot  # create a column of site names, from the rownames of data.scores
data.scores$grp <- grp  #  add the grp variable created earlier
head(data.scores)  #look at the data

#Pull species data
species.scores <- as.data.frame(scores(NMDS, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)  #look at the data


#Make the plot. I removed the plot names because I thought it was hard to read.

ggplot() +
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=grp,colour=grp),size=3) + # add the point markers
  #geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
  scale_colour_manual(values=c("Remnant" = "#415c57", "Regrowth" = "#6baa35", "Novel" = '#fe941c')) +
  coord_equal() +
  theme_bw() +
  stat_ellipse(data = data.scores, aes(x=NMDS1,y=NMDS2, colour=grp))

#Edges seem to be the same regardless of patch type.

#Edge fit ------------

env <- df[,2]

en <- envfit(NMDS, env, permutations = 999, na.rm = TRUE)

en

#Patch type are also significant predictor of composition in edges,
#but the R2 is lower.


