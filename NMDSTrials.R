#NMDS of tree species by patch type

#libraries

library(tidyverse)
library(tidylog)
library(magrittr)
library(vegan)
library(ggplot2)

setwd("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject")

#This file has plots, the patch type, and BA of each species in the columns
df<-read_csv('coreSpeciesBA.csv') %>% 
  select(-X57) %>% #It's adding a weird column. Drop that boy.
  replace(is.na(.),0) #Can't have NA for NMDS. Change to zero.

View(df)

#Run the NMDS. Two axes, try 250 times

set.seed(123) #Make it reproducible
NMDS<-metaMDS(comm = df[,-c(1:2)], distance = 'bray', k = 2, try = 250)

#Check results
NMDS

#Ugly plot
plot(NMDS, type = 't')

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
  scale_colour_manual(values=c("Remnant" = "red", "Regrowth" = "blue", "Novel" = 'Green')) +
  coord_equal() +
  theme_bw()

#Remnant patches cluster to the left, but there is not good resolution between
#regrowth and novel patches.

#Edge plots-----------------
#Let's look at the composition of plots that are on the edge of patches

#This file has plots, the patch type, and BA of each species in the columns
df<-read_csv('edgeSpeciesBA.csv') %>% 
  select(-X86) %>% #It's adding a weird column. Drop that boy.
  filter(PlotID != 6125) #This plot is all white pine and is ruining everything.

#Run the NMDS. Two axes, try 250 times

set.seed(123) #Make it reproducible
NMDS<-metaMDS(comm = df[,-c(1:2)], distance = 'bray', k = 3, try = 250)

#Check results
NMDS

#Ugly plot
plot(NMDS, type = 't')

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
  scale_colour_manual(values=c("Remnant" = "red", "Regrowth" = "blue", "Novel" = 'Green')) +
  coord_equal() +
  theme_bw()

#Edges seem to be the same regardless of patch type.

#Check how well the patch types fit the data

en <- envfit(NMDS, grp, permutations = 999, na.rm = TRUE)
