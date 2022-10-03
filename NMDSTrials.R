#NMDS of tree species by patch type

#Libraries------------

library(tidyverse)
library(tidylog)
library(magrittr)
library(vegan)
library(ggrepel)


setwd("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject")

#Core plots with species----------

#This file has plots, the patch type, and BA of each species in the columns

coreSpecies<-read_csv('plotAttributes.csv') %>% 
  filter(edgeCore == 'Core', #Only core plots
         patchType != "NA")  #Remove rows without trees

#Run the NMDS. Two axes, try 1000 times

set.seed(123) #Make it reproducible
NMDScoreSpecies<-metaMDS(comm = coreSpecies[,-c(1:3,117:140)], 
                         distance = 'bray', k = 3, try = 1000)

#Check results
NMDScoreSpecies

#Make a pretty plot using instructions from
#https://chrischizinski.github.io/rstats/vegan-ggplot2/

#Create group variable
PatchCoreSpecies<-coreSpecies$patchType

#And plot variable
plotCoreSpecies<-coreSpecies$PlotID

#Pull data from NMDS results
data.scoresCoreSpecies <- as.data.frame(scores(NMDScoreSpecies))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scoresCoreSpecies$site <- plotCoreSpecies  # create a column of site names, from the rownames of data.scores
data.scoresCoreSpecies$PatchType <- PatchCoreSpecies  #  add the grp variable created earlier
head(data.scoresCoreSpecies)  #look at the data

#Pull species data
species.scoresCoreSpecies <- as.data.frame(scores(NMDScoreSpecies, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scoresCoreSpecies$species <- rownames(species.scoresCoreSpecies)  # create a column of species, from the rownames of species.scores
species.scoresCoreSpecies$abundance <- colSums(coreSpecies[4:116])
order.abundance<-order(species.scoresCoreSpecies$abundance,species.scoresCoreSpecies$species)
species.scoresCoreSpecies$rank <- NA
species.scoresCoreSpecies$rank[order.abundance] <- 1:nrow(species.scoresCoreSpecies)
head(species.scoresCoreSpecies)  #look at the data
species.scoresCoreSpeciesTop <- species.scoresCoreSpecies %>% filter(rank>103) #Top 20 species


#Make the plot. I removed the plot names because I thought it was hard to read.

ggplot() +
  geom_point(data=data.scoresCoreSpecies,aes(x=NMDS1,y=NMDS2,shape=PatchType,colour=PatchType),size=3) + # add the point markers
  geom_text(data=species.scoresCoreSpeciesTop,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.8) +  # add the species labels
  #geom_text(data=data.scoresCoreSpecies,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
  scale_colour_manual(values=c("Remnant" = "#b2a594", "Regrowth" = "#d56639", "Novel" = '#59b6be')) +
  coord_equal() +
  stat_ellipse(data = data.scoresCoreSpecies, aes(x=NMDS1,y=NMDS2, colour=PatchType)) +
  theme(legend.position="none")
  #theme(legend.background = element_rect(fill = '#FDF7F1')) +
  #theme(plot.background = element_rect(fill = "#FDF7F1")) +
  #guides(fill=guide_legend(title="Forest type")) + #TODO: This isn't working
  #labs(title = 'Ordination of sum basal area of tree species in core plots by patch types') 

#Remnant patches cluster to the left, but there is not good resolution between
#regrowth and novel patches.

#Core species fit------------

envCoreSpecies <- coreSpecies[,c(3)]
        # , 117, 118,122,123,128,129,131,139,140)] #If you want to add the other variables

envCoreSpecies <- envfit(NMDScoreSpecies, envCoreSpecies, perm = 999, na.rm = TRUE)

envCoreSpecies

#Plot with environmental variables

#Technique from https://www.rpubs.com/RGrieger/545184
env.scores <- as.data.frame(scores(envCoreSpecies, display = "vectors")) #extracts relevant scores from envifit
env.scores <- cbind(env.scores, env.variables = rownames(env.scores)) #and then gives them their names
env.scores <- cbind(env.scores, pval = envCoreSpecies$vectors$pvals) # add pvalues to dataframe
sig.env.scrs <- subset(env.scores, pval<=0.05) #subset data to show variables significant at 0.05


ggplot() +
  #geom_text(data=species.scoresCoreSpecies,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
  geom_point(data=data.scoresCoreSpecies,aes(x=NMDS1,y=NMDS2,colour=PatchType),size=3) + # add the point markers
  #geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
  scale_colour_manual(values=c("Remnant" = "#415c57", "Regrowth" = "#6baa35", "Novel" = '#fe941c')) +
  coord_equal() +
  stat_ellipse(data = data.scoresCoreSpecies, aes(x=NMDS1,y=NMDS2, colour=PatchType)) +
  theme(legend.background = element_rect(fill = '#FDF7F1')) +
  theme(plot.background = element_rect(fill = "#FDF7F1")) +
  guides(fill=guide_legend(title="Forest type")) + #TODO: This isn't working
  labs(title = 'Ordination of core plots and species across patch types') +
  geom_segment(data = env.scores, aes(x = 0, xend=NMDS1, y=0, yend=NMDS2), 
               arrow = arrow(length = unit(0.25, "cm")), colour = "grey10", 
               lwd=0.3) + #add vector arrows of significant env variables
  ggrepel::geom_text_repel(data = env.scores, aes(x=NMDS1, y=NMDS2, label = env.variables), cex = 4, direction = "both", segment.size = 0.25) #add labels for env variables

#Core genus---------------

coreGenus<-read_csv('coreGenusBA20.csv') %>% 
  select(-X41) 

#Run the NMDS. Two axes, try 1000 times

set.seed(123) #Make it reproducible
NMDScoreGenus<-metaMDS(comm = coreGenus[,-c(1:3,117:140)], distance = 'bray', k = 2, try = 5000)

#Check results
NMDScoreGenus

#Make a pretty plot using instructions from
#https://chrischizinski.github.io/rstats/vegan-ggplot2/

#Create group variable
PatchCoreGenus<-coreGenus$patchType

#And plot variable
plotCoreGenus<-coreGenus$PlotID

#Pull data from NMDS results
data.scoresCoreGenus <- as.data.frame(scores(NMDScoreGenus))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scoresCoreGenus$site <- plotCoreGenus  # create a column of site names, from the rownames of data.scores
data.scoresCoreGenus$PatchType <- PatchCoreGenus  #  add the grp variable created earlier
head(data.scoresCoreGenus)  #look at the data

#Pull species data
species.scoresCoreGenus <- as.data.frame(scores(NMDScoreGenus, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scoresCoreGenus$species <- rownames(species.scoresCoreGenus)  # create a column of species, from the rownames of species.scores
head(species.scoresCoreGenus)  #look at the data


#Make the plot. I removed the plot names because I thought it was hard to read.

ggplot() +
  geom_text(data=species.scoresCoreGenus,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
  geom_point(data=data.scoresCoreGenus,aes(x=NMDS1,y=NMDS2,shape=PatchType,colour=PatchType),size=3) + # add the point markers
  #geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
  scale_colour_manual(values=c("Remnant" = "#415c57", "Regrowth" = "#6baa35", "Novel" = '#fe941c')) +
  coord_equal() +
  stat_ellipse(data = data.scoresCoreGenus, aes(x=NMDS1,y=NMDS2, colour=PatchType)) +
  theme(legend.background = element_rect(fill = '#FDF7F1')) +
  theme(plot.background = element_rect(fill = "#FDF7F1")) +
  guides(fill=guide_legend(title="Forest type")) + #TODO: This isn't working
  labs(title = 'Ordination of sum basal area of tree genera in core plots by patch types') 

#Remnant patches cluster to the left, but there is not good resolution between

#Core genus fit------------

envCoreGenus <- coreGenus[,c(2)]

enCoreGenus <- envfit(NMDScoreGenus, envCoreGenus, permutations = 999, na.rm = TRUE)

enCoreGenus


#Patch type is a highly significant predictor of species composition
#in forest cores

#Edge plots-----------------
#Let's look at the composition of plots that are on the edge of patches

#This file has plots, the patch type, and BA of each species in the columns
edgeSpecies<-read_csv('plotAttributes.csv') %>% 
  filter(edgeCore == 'Edge', #Only core plots
         patchType != "NA") %>%   #Remove rows without trees%>% 
  filter(PlotID != 6125) %>%  #This plot is all white pine and is ruining everything.
  filter(PlotID != 665) 
  
#Run the NMDS. Two axes, try 250 times

set.seed(123) #Make it reproducible
NMDSedgeSpecies<-metaMDS(comm = edgeSpecies[,-c(1:3,117:140)], distance = 'bray', k = 3, try = 1000)


#Make a prettier plot using instructions from
#https://chrischizinski.github.io/rstats/vegan-ggplot2/

#Create group variable
PatchTypeedgeSpecies<-edgeSpecies$patchType

#And plot variable
plotedgeSpecies<-edgeSpecies$PlotID

#Pull data from NMDS results
data.scoresedgeSpecies <- as.data.frame(scores(NMDSedgeSpecies))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scoresedgeSpecies$site <- plotedgeSpecies  # create a column of site names, from the rownames of data.scores
data.scoresedgeSpecies$PatchType <- PatchTypeedgeSpecies  #  add the grp variable created earlier
head(data.scoresedgeSpecies)  #look at the data

#Pull species data
species.scoresedgeSpecies <- as.data.frame(scores(NMDSedgeSpecies, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scoresedgeSpecies$species <- rownames(species.scoresedgeSpecies)  # create a column of species, from the rownames of species.scores
species.scoresedgeSpecies$abundance <- colSums(edgeSpecies[3:98])
order.abundance<-order(species.scoresedgeSpecies$abundance,species.scoresedgeSpecies$species)
species.scoresedgeSpecies$rank <- NA
species.scoresedgeSpecies$rank[order.abundance] <- 1:nrow(species.scoresedgeSpecies)
head(species.scoresedgeSpecies)  #look at the data
species.scoresedgeSpeciesTop <- species.scoresedgeSpecies %>% filter(rank>86) #Top 20 species


#Make the plot. I removed the plot names because I thought it was hard to read.

ggplot() +
  geom_text(data=species.scoresedgeSpeciesTop,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
  geom_point(data=data.scoresedgeSpecies,aes(x=NMDS1,y=NMDS2,shape=PatchType,colour=PatchType),size=3) + # add the point markers
  #geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
  scale_colour_manual(values=c("Remnant" = "#415c57", "Regrowth" = "#6baa35", "Novel" = '#fe941c')) +
  coord_equal() +
  stat_ellipse(data = data.scoresedgeSpecies, aes(x=NMDS1,y=NMDS2, colour=PatchType)) +
  theme(legend.background = element_rect(fill = '#FDF7F1')) +
  theme(plot.background = element_rect(fill = "#FDF7F1")) +
  guides(fill=guide_legend(title="Forest type")) + #TODO: This isn't working
  labs(title = 'Ordination of sum basal area of tree species in edge plots by patch types')

#Edges seem to be the same regardless of patch type.

#Edge species fit ------------

envEdgeSpecies <- edgeSpecies[,2]

enEdgeSpecies <- envfit(NMDSedgeSpecies, envEdgeSpecies, permutations = 999, na.rm = TRUE)

enEdgeSpecies

#Edge genus-----------------

edgeGenus<-read_csv('edgeGenusBA20.csv') %>% 
  select(-X48) %>% 
  filter(PlotID != 6125) %>% 
  filter(PlotID != 665)

#Run the NMDS. Two axes, try 250 times

set.seed(123) #Make it reproducible
NMDSedgeGenus<-metaMDS(comm = edgeGenus[,-c(1:2)], distance = 'bray', k = 2, try = 1000)

#Check results
NMDS

#Make a prettier plot using instructions from
#https://chrischizinski.github.io/rstats/vegan-ggplot2/

#Create group variable
PatchTypeedgeGenus<-edgeGenus$patchType

#And plot variable
plotedgeGenus<-edgeGenus$PlotID

#Pull data from NMDS results
data.scoresedgeGenus <- as.data.frame(scores(NMDSedgeGenus))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scoresedgeGenus$site <- plotedgeGenus  # create a column of site names, from the rownames of data.scores
data.scoresedgeGenus$PatchType <- PatchTypeedgeGenus  #  add the grp variable created earlier
head(data.scoresedgeGenus)  #look at the data

#Pull species data
species.scoresedgeGenus <- as.data.frame(scores(NMDSedgeGenus, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scoresedgeGenus$species <- rownames(species.scoresedgeGenus)  # create a column of species, from the rownames of species.scores
head(species.scoresedgeGenus)  #look at the data


#Make the plot. I removed the plot names because I thought it was hard to read.

ggplot() +
  geom_text(data=species.scoresedgeGenus,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
  geom_point(data=data.scoresedgeGenus,aes(x=NMDS1,y=NMDS2,shape=PatchType,colour=PatchType),size=3) + # add the point markers
  #geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
  scale_colour_manual(values=c("Remnant" = "#415c57", "Regrowth" = "#6baa35", "Novel" = '#fe941c')) +
  coord_equal() +
  stat_ellipse(data = data.scoresedgeGenus, aes(x=NMDS1,y=NMDS2, colour=PatchType)) +
  theme(legend.background = element_rect(fill = '#FDF7F1')) +
  theme(plot.background = element_rect(fill = "#FDF7F1")) +
  guides(fill=guide_legend(title="Forest type")) + #TODO: This isn't working
  labs(title = 'Ordination of sum basal area of tree genera in edge plots by patch types')

#Edge genus fit ------------

envEdgeGenus <- edgeGenus[,2]

enEdgeGenus <- envfit(NMDSedgeGenus, envEdgeGenus, permutations = 999, na.rm = TRUE)

enEdgeGenus
