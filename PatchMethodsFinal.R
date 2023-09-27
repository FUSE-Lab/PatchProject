#Final code for patch project
#Assembled 9/22/2022
#By Lindsay Darling

#This code will do all spatial analysis, statistics, and figure creation for 
#the paper "Ecological and developmental history impacts the equitable 
#distribution of services"
#The starred section headers can be run independently so long as the initial
#code was run previously to make the necessary layers.

#*Load libraries*---------------

library(plyr)             #For join flexibility
library(tidyverse)        
library(magrittr)         #%<>%
library(tidylog)          #More verbose tidyverse
library(sf)               #Spatial data

#Set path to files
path <- "C:/Users/ledarlin/"

setwd(paste0(path,'Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject'))

#*Identify patch types*-----------

#Pre-settlement veg from Halsey and McBride (2015)
veg <- st_read('AllVeg.shp')%>% 
  sf::st_transform(., crs = 5070) %>%  #Change to albers equal conic
  st_make_valid(.) #Fix topology

#1939 patches from Fahey and Casali (2017)
midCen <- st_read('1939_Oaks_CW.shp')%>% 
  sf::st_transform(., crs = 5070) %>% 
  st_make_valid(.) #Fix topology

#Current patch core and edge from Darling et al. (2023)
#Note, that prior to loading these data into R, the Darling et al. raster was
#converted to simplified polygons.
#This process failed in R, so it was done in ArcGIS Pro 9.8.
coreEdge <- st_read('CoreAndEdge.shp') %>%
  st_make_valid(.) %>% #Fix topology error
  sf::st_transform(., crs = 5070) %>% #Change to albers conic
  #Give better names, remove the connector polygons.
  mutate(coreEdge = if_else(gridcode == 2, 'edge',
                            if_else(gridcode == 3, 'core', 'none'))) %>% 
  dplyr::select(coreEdge)

#Isolate wooded areas in the PLS------------

woods <- veg %>% 
  filter(ECOSYSTEM %in% c('Thicket', 'Barrens', 'Woodland')) #Only wooded ecosystem types

#Create layers for each patch type----------

#Remnant cores

#Everything that was forested in 1939 and 2010 = remnant
remEdgeCore <- st_intersection(coreEdge, midCen) %>% 
  sf::st_transform(., crs = 5070) %>% 
  #Give names to ID
  mutate(Type = 'Remnant') %>% 
  dplyr::select(coreEdge, Type) #Drop extra columns

#Regrowth
#!!! This code should work, but the file is too big. It keeps crashing during 
#the difference step. I am running the exact same process (clip to presettlement 
#woodlands, erase remnant woodlands) in ArcGIS And saving the file to the same 
#name. I'll do the same for the edges.

# regrowthCoreEdge <- st_intersection(coreEdge, woods) %>% #Everything in preset woodlands
#   st_difference(.,remEdgeCore) #Erase things that are remnants (ie forested in 1939)   

regrowthEdgeCore <- st_read('RegrowthCoreEdge.shp') %>% 
  sf::st_transform(., crs = 5070) %>% 
  st_make_valid(.) %>% #Fix topology
  mutate(coreEdge = if_else(gridcode == 2, 'edge',
                            if_else(gridcode == 3, 'core', 'none')),
         Type = 'Regrowth') %>% 
  dplyr::select(coreEdge, Type)

#!!! Again, st_difference keeps failing. I am doing the same process using 
#'erase' in arcgis then loading those files into here.  

# #New cores
# newCore <- st_difference(currentcore, woodsBuff) %>% 
#   st_difference(., midCen) %>% #Sometimes 1939 patches are outside the preset 
#boundaries.
#   st_write(., 'NovelPatch.shp', driver = 'ESRI Shapefile')
# 
# #New edges
# newEdge <- st_difference(currentedge, woodsBuff) #%>%  
#   st_difference(., midCen) %>% 
#   st_write(., 'NovelEdge.shp', driver = 'ESRI Shapefile')

novelEdgeCore <- st_read('NovelCoreEdge.shp') %>% 
  sf::st_transform(., crs = 5070) %>% 
  st_make_valid(.) %>% #Fix topology
  mutate(coreEdge = if_else(gridcode == 2, 'edge',
                            if_else(gridcode == 3, 'core', 'none')),
         Type = 'Novel') %>% 
  dplyr::select(coreEdge, Type) 

#Create layer of all patch types

PatchType <- remEdgeCore %>% 
  rbind(novelEdgeCore, regrowthEdgeCore) %>%
  mutate(PatchType = paste0(Type, ' ', coreEdge)) %>% 
  st_write(., 'PatchType.shp', driver = 'ESRI Shapefile', append = FALSE)

#Note: I put this file in Arc and ran Dissolve with the options Unsplit lines
#and do not make multipart polygons and dissolve field = PatchType 
#to create PatchTypeDissolve. This layer is published in Zenodo

#Take a break, clear memory, and pull in these files fresh.

rm(list = ls())

path <- "C:/Users/ledarlin/"

#*Identify plot types*------------------

#From Kua at al. (2020)
plots2 <- st_read('PlotsAll2020.shp') %>% #iTree
  sf::st_transform(., crs = 5070)

#We sampled in 2021
plots3 <- st_read('PlotsToSample.shp') %>% #New data
  sf::st_transform(., crs = 5070) %>% 
  dplyr::rename(LATITUDE = LAT,
         LONGITUDE = LONG,
         PlotID = PLOTID) %>% 
  dplyr::mutate(OBJECTID = row_number()) %>% 
  dplyr::select(OBJECTID, PlotID, LATITUDE, LONGITUDE, geometry)

plots2020 <- rbind(plots2, plots3)

#load patch data
patch <- st_read('PatchTypeDissolve.shp')

plotPatch <- st_intersection(plots2020, patch) %>% 
  select(PlotID, PatchType) %>%  #Select what we need
  #Make patch type and core/edge separate columns
  separate(PatchType, into = c('Type', 'coreEdge'), sep = ' ') 

#Get tree data-------------

#Species list. Names from itree only have common. This lets us add latin 

species <- read_csv('itree_species_list.csv')

#Kua et al. 2020
trees20 <- read_csv('AllTrees2020.csv') %>% 
  dplyr::rename('PlotID' = 'Plot ID') %>% #clean names
  mutate(dbh_cm = `DBH (in)`*2.54) %>% #Because we're scientists
  left_join(., species, by = 'Common Name' ) %>% #Get latin
  select(PlotID, GenusSpecies, dbh_cm) #Grab the goods

#2021 plots, clean up, bind to 2020, join to plots

plotTree <- read.csv('PatchTreeData.csv') %>% 
  dplyr::rename(GenusSpecies = Species) %>% #Good names
  mutate(dbh_cm = DBH_in * 2.54) %>% #in to cm
  select(PlotID, GenusSpecies, dbh_cm) %>% #good columns
  rbind(trees20, .) %>% #Bind Kua et al. data
  left_join(plotPatch, ., by = 'PlotID') %>% #Get patch types
  na.omit() #A few plots don't have trees

#Create table with plots and each species BA and stem count

plotTypeBA <- plotTree %>% 
  mutate(ba_m = (dbh_cm^2)*0.00007854) %>% #Add BA
  dplyr::select(c(PlotID, Type, coreEdge, GenusSpecies, ba_m)) %>% #Only the necessary columns
  pivot_wider(id_cols = c('PlotID', 'Type', 'coreEdge'), #Summarize by PlotID and keep patch type
              names_from = 'GenusSpecies', #Makes species columns
              names_sort = TRUE, #Sort alphabetically
              values_from = 'ba_m', #Cells are BA
              values_fn = sum, #Add BA together
              values_fill = 0) %>% #Fill NA with 0 
  as_tibble() %>% 
  #Add sum BA column. Adding everything numeric then subtract PlotID
  dplyr::mutate(sumBA = rowSums(across(where(is.numeric)))- `PlotID`,
                BADen = sumBA/0.0404686) %>%  #Divide by plot size to get density (m2/ha)) %>% 
  #I looked at imagery from 1970 and 1990 and was able to confirm whether plots
  #were remnant or regrowth. These are some manual tweaks to reflect that work.
  mutate(Type = case_when(PlotID == 2112 ~"Regrowth", 
                          PlotID == 3027 ~"Regrowth",
                          PlotID == 5053 ~"Regrowth",
                          PlotID == 10011 ~"Regrowth",
                          PlotID == 10014 ~"Novel",
                          PlotID == 10020 ~"Regrowth",
                          TRUE ~ Type)) %>% 
  write_csv(., 'PlotTypeBA.csv') #Write this big boi so that we can get him later

#Make BA and stem plots---------

#Grab stem count per plot
stemCount <- plotTree %>% 
  dplyr::group_by(PlotID, coreEdge, Type) %>% #Group by plot
  dplyr::summarize(value = n()) %>% #Count how many trees in each
  mutate(metric = 'StemDen',
         value = value/0.0404686,
         metric = factor(metric)) %>% 
  st_drop_geometry()

longCore <- plotTypeBA %>% 
  select(c('PlotID', 'coreEdge', 'Type', 'BADen')) %>% 
  dplyr::rename(value = 'BADen') %>% 
  mutate(metric = factor("BA")) %>% 
  rbind(., stemCount) %>% 
  filter(coreEdge == 'core') 

#Create new labels for facet_grid
longCore$labels <- factor(longCore$metric, labels = c(
  'Basal~area~m^{2}/ha', 'Stems~per~ha'))

#The sizes of font and exports was originally written for a poster. However,
#When I changed them to be paper-sized the lines were super fat and everything
#looked yucky. This was the easiest way to fix it.
corePlot <- ggplot(longCore, aes(x = Type, y = value, fill = Type))+
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.2, fill = 'white') +
  scale_fill_manual(values=c("Remnant" = "#b2a594", "Regrowth" = "#d56639", "Novel" = '#59b6be')) +
  guides(fill = guide_legend(title = "Forest type")) +
  theme_bw() +
  theme(legend.position = 'none',
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 24),
        title = element_text(size = 24),
        strip.text = element_text(size=24),
        axis.ticks.x = element_blank()) +
  facet_wrap(~labels, ncol = 2, scales = 'free', labeller = label_parsed) 

corePlot
ggsave(paste0(path, "Dropbox/LindsayWorking/GradSchool/Dissertation/Figures/corePlotPoster.png"), 
       corePlot, width = 9.25, height = 2.825, unit = "in", dpi = 300)

longEdge <- plotTypeBA %>% 
  select(c('PlotID', 'coreEdge', 'Type', 'BADen')) %>% 
  dplyr::rename(value = 'BADen') %>% 
  mutate(metric = factor("BA")) %>% 
  rbind(., stemCount) %>% 
  filter(coreEdge == 'edge') 

longEdge$labels <- factor(longEdge$metric, labels = c(
  'Basal~area~m^{2}/ha', 'Stems~per~ha'))

edgePlot <- ggplot(longEdge, aes(x = Type, y = value, fill = Type))+
  geom_violin(trim = TRUE) +
  geom_boxplot(width = 0.2, fill = 'white') +
  scale_fill_manual(values=c("Remnant" = "#b2a594", "Regrowth" = "#d56639", "Novel" = '#59b6be')) +
  guides(fill = guide_legend(title = "Forest type")) +
  theme_bw() +
  theme(legend.position = 'none',
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 24),
        title = element_text(size = 24),
        strip.text = element_text(size=24),
        axis.ticks.x = element_blank()) +
  facet_wrap(~labels, ncol = 2, scales = 'free', labeller = label_parsed) 

edgePlot

ggsave(paste0(path, "Dropbox/LindsayWorking/GradSchool/Dissertation/Figures/edgePlotPoster.png"), 
       edgePlot, width = 9.25, height = 2.825, unit = "in", dpi = 300)

#Check significance------------

#Edge BA
edgeBA <- longEdge %>% 
  filter(metric == 'BA')

lmEdgeBA <- lm(value ~ Type, data = edgeBA)

anova(lmEdgeBA) #ns

#Edge stem

edgeStem <- longEdge %>% 
  filter(metric == 'StemDen')

lmCoreBA <- lm(value ~ Type, data = edgeBA)

anova(lmCoreBA) #ns

#Core BA

coreBA <- longCore %>% 
  filter(metric == 'BA')

lmCoreBA <- lm(value ~ Type, data = coreBA)

anova(lmCoreBA) #*

#Core stem

coreStem <- longCore %>% 
  filter(metric == 'StemDen')

lmCoreStem <- lm(value ~ Type, data = coreStem)

anova(lmCoreStem) #*

#Take a break, clear memory, and pull in these files fresh.

rm(list = ls())

path <- "C:/Users/ledarlin/"

#*NMDS*------------

#new libraries

library(vegan)       #For ordination
library(ggrepel)     #Clean up figure

coreSpecies<-read_csv('plotTypeBA.csv') %>% 
  filter(coreEdge == 'core') #Only core plots

#Run the NMDS. Three axes, try 10000 times

set.seed(123) #Make it reproducible
NMDScoreSpecies<-metaMDS(comm = coreSpecies[,c(4:104)], 
                         distance = "bray", k = 3, try = 10000, trymax = 10000)
#Check results
NMDScoreSpecies

#Make a pretty plot using instructions from
#https://chrischizinski.github.io/rstats/vegan-ggplot2/

#Create group variable
PatchCoreSpecies<-coreSpecies$Type

#And plot variable
plotCoreSpecies<-coreSpecies$PlotID

#Pull data from NMDS results
data.scoresCoreSpecies <- as.data.frame(scores(NMDScoreSpecies, display = "sites"))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scoresCoreSpecies$site <- plotCoreSpecies  # create a column of site names, from the rownames of data.scores
data.scoresCoreSpecies$PatchType <- PatchCoreSpecies  #  add the grp variable created earlier
head(data.scoresCoreSpecies)  #look at the data

#Pull species data
species.scoresCoreSpecies <- as.data.frame(scores(NMDScoreSpecies, display = "species"))   #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scoresCoreSpecies$species <- rownames(species.scoresCoreSpecies)  # create a column of species, from the rownames of species.scores
species.scoresCoreSpecies$abundance <- colSums(coreSpecies[4:104])
order.abundance<-order(species.scoresCoreSpecies$abundance,species.scoresCoreSpecies$species)
species.scoresCoreSpecies$rank <- NA
species.scoresCoreSpecies$rank[order.abundance] <- 1:nrow(species.scoresCoreSpecies)
species.scoresCoreSpecies %<>% separate(`species`, c("genus", "species"), sep = " ") %>% 
  mutate(genusAb = substr(genus, 1, 1),
         speciesAb = paste0(genusAb, ". ", species))
head(species.scoresCoreSpecies)  #look at the data
species.scoresCoreSpeciesTop <- species.scoresCoreSpecies %>% filter(rank>86) #Top 15 species


#Make the plot. I removed the plot names because I thought it was hard to read.

corePlot <- ggplot() +
  stat_ellipse(level = .75, size = 1, data = data.scoresCoreSpecies, aes(x=`NMDS1`,y=`NMDS2`, colour=`PatchType`)) +
  geom_point(data=data.scoresCoreSpecies,aes(x=NMDS1,y=NMDS2,shape=PatchType,colour=PatchType),size=2) + # add the point markers
  geom_text_repel(data=species.scoresCoreSpeciesTop,aes(x=NMDS1,y=NMDS2,label=speciesAb),alpha=0.8, size = 5, fontface = "italic") +  # add the species labels
  #geom_text(data=data.scoresCoreSpecies,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
  scale_colour_manual(values=c("Remnant" = "#b2a594", "Regrowth" = "#d56639", "Novel" = '#59b6be')) +
  coord_equal() +
  annotate("text", x = -0.7, y = -0.5, label = 'R^2 == 0.2802', parse = TRUE, size = 5) +
  annotate("text", x = -0.7, y = -0.6, label = 'p < 0.001', size = 5) +
  annotate("text", x = -0.4, y = 0.55, label = 'Novel', size = 6, color = "#59b6be", fontface = 'bold') +
  annotate("text", x = -0.75, y = -0.0, label = 'Regrowth', size = 6, color = "#d56639", fontface = 'bold') +
  annotate("text", x = 0.35, y = -0.5, label = 'Remnant', size = 6, color = "#b2a594", fontface = 'bold') +
  theme_bw() +
  xlim(c(-0.86, .55)) +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) 
#theme(legend.background = element_rect(fill = '#FDF7F1')) +
#theme(plot.background = element_rect(fill = "#FDF7F1")) +
#guides(fill=guide_legend(title="Forest type")) + #TODO: This isn't working
#labs(title = 'Ordination of sum basal area of tree species in core plots by patch types') 

corePlot

ggsave('C:/Users/ledarlin/Dropbox/LindsayWorking/GradSchool/Dissertation/Figures/coreNMDSPoster_bw.png', 
       corePlot, width = 6.5, height = 6, unit = "in", dpi = 300)

#Core species fit------------

envCoreSpecies <- coreSpecies[,c(2)]
# , 117, 118,122,123,128,129,131,139,140)] #If you want to add the other variables

envCoreSpecies <- envfit(NMDScoreSpecies, envCoreSpecies, perm = 999, na.rm = TRUE)

envCoreSpecies

#Edge plots-----------------
#Let's look at the composition of plots that are on the edge of patches

edgeSpecies<-read_csv('plotTypeBA.csv') %>% 
  filter(coreEdge == 'edge', #Only core plot
         PlotID != 6125)#,  #This plot is all white pine and is ruining everything.
        # PlotID != 665)  #These are not patches. Only 1 or 2 trees
 

#Run the NMDS. Two axes, try 10000 times

set.seed(123) #Make it reproducible
NMDSedgeSpecies<-metaMDS(comm = edgeSpecies[,c(4:106)], 
                         distance = "bray", k = 3, try = 1000, trymax = 1000)

#Create group variable
PatchTypeedgeSpecies<-edgeSpecies$Type

#And plot variable
plotedgeSpecies<-edgeSpecies$PlotID

#Pull data from NMDS results
data.scoresedgeSpecies <- as.data.frame(scores(NMDSedgeSpecies, display = "sites"))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scoresedgeSpecies$site <- plotedgeSpecies  # create a column of site names, from the rownames of data.scores
data.scoresedgeSpecies$PatchType <- PatchTypeedgeSpecies  #  add the grp variable created earlier
head(data.scoresedgeSpecies)  #look at the data

#Pull species data
species.scoresedgeSpecies <- as.data.frame(scores(NMDSedgeSpecies, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scoresedgeSpecies$species <- rownames(species.scoresedgeSpecies)  # create a column of species, from the rownames of species.scores
species.scoresedgeSpecies$abundance <- colSums(edgeSpecies[4:110])
order.abundance<-order(species.scoresedgeSpecies$abundance,species.scoresedgeSpecies$species)
species.scoresedgeSpecies$rank <- NA
species.scoresedgeSpecies$rank[order.abundance] <- 1:nrow(species.scoresedgeSpecies)
species.scoresedgeSpecies %<>% separate(`species`, c("genus", "species"), sep = " ") %>% 
  mutate(genusAb = substr(genus, 1, 1),
         speciesAb = paste0(genusAb, ". ", species))
head(species.scoresedgeSpecies)  #look at the data
species.scoresedgeSpeciesTop <- species.scoresedgeSpecies %>% filter(rank>87) #Top 15 species

#Make the plot. 

edgePlot <- ggplot() +
  stat_ellipse(level = .75, size = 1, data = data.scoresedgeSpecies, aes(x=`NMDS1`,y=`NMDS2`, colour=`PatchType`)) +
  geom_point(data=data.scoresedgeSpecies,aes(x=NMDS1,y=NMDS2,shape=PatchType,colour=PatchType),size=2) + # add the point markers
  geom_text_repel(data=species.scoresedgeSpeciesTop,aes(x=NMDS1,y=NMDS2,label=speciesAb),alpha=0.8, size = 5, fontface = 'italic') +  # add the species labels
  #geom_text(data=data.scoresedgeSpecies,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
  scale_colour_manual(values=c("Remnant" = "#b2a594", "Regrowth" = "#d56639", "Novel" = '#59b6be')) +
  coord_equal() +
  annotate("text", x = -0.85, y = -0.65, label = 'R^2 == 0.1880', parse = TRUE, size = 5) +
  annotate("text", x = -0.85, y = -0.75, label = 'p < 0.052', size = 5) +
  annotate("text", x = .4, y = 0.65, label = 'Novel', size = 6, color = "#59b6be", fontface = 'bold') +
  annotate("text", x = -.75, y = 0.55, label = 'Regrowth', size = 6, color = "#d56639", fontface = 'bold') +
  annotate("text", .87, y = 0, label = 'Remnant', size = 6, color = "#b2a594", fontface = 'bold') +
  theme_bw() +
  xlim(c(-1,1)) +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

edgePlot

ggsave('C:/Users/ledarlin/Dropbox/LindsayWorking/GradSchool/Dissertation/Figures/edgeNMDSPoster_bw.png', 
       edgePlot, width = 6.5, height = 6, unit = "in", dpi = 300)

#Edges seem to be the same regardless of patch type.

#Edge species fit ------------

envEdgeSpecies <- edgeSpecies[,2]

enEdgeSpecies <- envfit(NMDSedgeSpecies, envEdgeSpecies, permutations = 999, na.rm = TRUE)

enEdgeSpecies

#Clean up

rm(list = ls())

path <- "C:/Users/ledarlin/"

#*Grid patch and race data*----------

#Census data from tidycensus

library(tidycensus)
library(units)

#You need to add your own api key.
census_api_key(key = "YOUR KEY HERE", install=T, overwrite = TRUE)

#Set variables
geom <- 'block group'
YR <- 2015
ST <- c('IL')
CNTY <- c('Cook', 'Dupage', 'Kendall', 'Lake', 'McHenry', 'Kane', 'Will County')
vars <- c('B02001_002', 'B01001_001')

#Use tidycensus to get data
census<- get_acs(geography = geom,
                 variables = vars,
                 state = ST,
                 county = CNTY,
                 year = YR,
                 output = 'wide',
                 geometry = TRUE) %>% #That pulls in tigerlines
  sf::st_transform(., crs = 5070) %>% #Change to albers conic
  #Add area and convert to ha
  mutate(AreaHa = units::set_units(st_area(.), value=ha))

#Rename, select, and modify variables

census %<>%
  rename(Wht_pop = `B02001_002E`,
         Tot_pop = `B01001_001E`) %>% 
  dplyr::select(GEOID, NAME,
                Wht_pop,
                Tot_pop) %>% 
  mutate(WhtPopP = (Wht_pop/(Tot_pop +0.000001))) #Change some variables to percent


#Aggregate census to grid---------

grid<-st_read('Poly2km.shp') %>%  
  dplyr::select(ID) %>% 
  st_transform(crs = 5070)

gridCensus <- #area = extensive means it sums the values
  st_interpolate_aw(census[,c('Wht_pop', 'Tot_pop')], 
                    grid, extensive = TRUE) %>% 
  cbind(grid,.) %>% #Bind IDs back in
  mutate(WhtPopP = Wht_pop/Tot_pop)

#Patch to grid-------------
patch <- st_read("PatchTypeDissolve.shp") %>% 
  st_make_valid(.) %>% #Fix topology error
  separate(., col = `PatchType`, into = c('Type', 'coreEdge'), sep = " ", remove = TRUE)

#Get grid IDs on patches
int <- st_intersection(patch, grid) %>% #Intersect patches with census data
  mutate(AreaHa = units::set_units(st_area(.), value=ha)) %>% 
  st_drop_geometry(.)

#This works like a pivot table in Excel. 
#Calculate sum of patch history in each grid
patchType <- int %>% 
  dplyr::select('ID', 'Type', 'AreaHa') %>%  #Select columns that I need
  pivot_wider(names_from = Type, #Column names
              values_from = AreaHa, #Values to put in cells
              values_fn = sum) #Add all of the values within a block/patchType

#Now patch edges
patchSums <- int %>% 
  dplyr::select('ID', 'coreEdge', 'AreaHa') %>%  #Select columns that I need
  pivot_wider(names_from = coreEdge, #Column names
              values_from = AreaHa, #Values to put in cells
              values_fn = sum) %>% #Add all of the values within a block/edge
  left_join(., patchType, by = 'ID')

#Join back to grids
patchGrid <- left_join(gridCensus, patchSums, by = 'ID') %>%
  mutate(Novel0 = drop_units(`Novel`),
         Regrowth0 = drop_units(`Regrowth`),
         Remnant0 = drop_units(`Remnant`),
         Edge0 = drop_units(`edge`),
         Core0 = drop_units(`core`)) %>% 
  replace(is.na(.), 0) %>% 
  st_drop_geometry(.) %>% 
  write_csv(., 'gridData.csv')

#Breathe
rm(list = ls())

path <- "C:/Users/ledarlin/"

#*Race and patches*---------------

#Running through zero gamma in brms from here: 
#https://www.andrewheiss.com/blog/2022/05/09/hurdle-lognormal-gaussian-brms/

#First need to install cmdstanr. Getting these to run on your machine takes 
#some work. I've commented out many of the commands that are needed on the 
#first time. If you haven't done bayesian things, you may need to use those lines.

#install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
#install_cmdstan(cores = 2, overwrite = TRUE)

library(cmdstanr)
library(brms)            # Bayesian modeling through Stan
library(broom)           # Convert model objects to data frames
library(tidybayes)       # Manipulate Stan objects in a tidy way
library(gt)              # Make handsome tables
library(gtExtras)        # Even handomer tables

options(mc.cores = 4,
        brms.backend = "cmdstanr")

# Set some global Stan options
CHAINS <- 4
ITER <- 5000 
WARMUP <- 500
BAYES_SEED <- 1234

set.seed(1234)

#Read data-------
df <- read_csv('gridData.csv') %>% 
  mutate(All0 = Novel0 + Regrowth0 + Remnant0)

#Testing models---------
#Look at linear regression

ggplot(df, aes(x = WhtPopP, y = All0)) +
  geom_smooth(method = "lm", color = "#0074D9") +
  guides(color = guide_legend(override.aes = list(size = 1.5, alpha = 1))) +
  theme(legend.position = "bottom")

hist(df$Novel0)

#Positive correlation, distribution is VERY non-normal. 
#Need to do something different.

#No zero bayasian
model_mass_basic_no_zero <- brm(
  bf(Novel0 ~ WhtPopP),
  data = filter(df, Novel0 != 0), #Filter out zero values
  family = gaussian(),
  chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED,
  silent = 2
)

summary(model_mass_basic_no_zero) #Rhat is important. 1 is great. Chains converged
tidy(model_mass_basic_no_zero)
pp_check(model_mass_basic) #Really bad fit
pp_check(model_mass_basic_no_zero) #Yuck

#Hurdle with gamma distribution
model_mass_hurdle_Novel <- brm(
  bf(Novel0 ~ WhtPopP,
     hu ~ WhtPopP),
  data = df,
  family = hurdle_gamma(),
  chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED,
  silent = 2
)

summary(model_mass_hurdle_Novel) #How to interpret?
#In table show the estimate and confidence intervals
pp_check(model_mass_hurdle_Novel, ndraws = 200) #Looks pretty good?
pp_check(model_mass_hurdle_Novel, ndraws = 200, type = 'stat_2d') 

#Plot each model
conditional_effects(model_mass_hurdle_Novel, effects = "WhtPopP")
#More white folk = less novel
conditional_effects(model_mass_hurdle_Novel, effects = "WhtPopP", dpar = "hu")
#!!! Likelihood of no novel increase with whiteness!!!

#Looks pretty good. Let's run this for everything.

#Patch history types-------

#Prep data
df2 <- pivot_longer(data = df,
                    cols = c(Remnant0, Regrowth0, Novel0),
                    names_to = 'Type')

#Run model with novel, regrowth, remnant acreage.
#Clear your calendar.

model_gamma_hurdle_hist <- brm(
  bf(value ~ WhtPopP * Type,
     hu ~ WhtPopP * Type),
  data = df2,
  family = hurdle_gamma(),
  chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED,
  silent = 2
)

#Check it
summary(model_gamma_hurdle_hist) #Model results (Use these for summary table)
View(coef(model_gamma_hurdle_hist))
pp_check(model_gamma_hurdle_hist)

#Prep for figures
df3 <- df2 %>%
  group_by(type) %>%
  add_epred_draws(model_gamma_hurdle_hist, dpar = 'hu') #%>%

regress_hist <- ggplot(data = df3, aes(x = WhtPopP, y = type, color = type, fill = type)) +
  stat_lineribbon(aes(y = .epred), .width = 0.95, alpha = 0.5) +
  scale_colour_manual(values=c("Remnant0" = "#b2a594", 
                               "Regrowth0" = "#d56639", "Novel0" = '#59B6BE')) +
  scale_fill_manual(values=alpha(c("Remnant0" = "#b2a594", 
                                   "Regrowth0" = "#d56639", "Novel0" = '#59B6BE'), 0.2)) +
  xlab("Percent white population") +
  ylab("Forest cover (ha)") +
  scale_x_continuous(labels = scales::percent, limits = c(0,1)) +
  theme_bw() +
  theme(legend.position = 'none',
        text = element_text(size = 11))

regress_hist

ggsave(paste0(path,'Dropbox/LindsayWorking/GradSchool/Dissertation/Figures/PatchSocialRegressCoreOnly.png'), 
       regress_hist, width = 3.5, height = 2.25, unit = "in", dpi = 300)


hu_hist <- ggplot(data = df3, aes(x = WhtPopP, y = type, color = type, fill = type)) +
  stat_lineribbon(aes(y = hu), .width = 0.95) +
  scale_colour_manual(values=c("Remnant0" = "#b2a594", 
                               "Regrowth0" = "#d56639", "Novel0" = '#59B6BE')) +
  scale_fill_manual(values=alpha(c("Remnant0" = "#b2a594", 
                                   "Regrowth0" = "#d56639", "Novel0" = '#59B6BE'), 0.2)) +
  xlab("Percent white population") +
  ylab("Likelihood of zero forests") +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.8)) +
  theme_bw() +
  theme(legend.position = 'none',
        text = element_text(size = 11))

hu_hist

ggsave(paste0(path,'Dropbox/LindsayWorking/GradSchool/Dissertation/Figures/PatchSocialHuCoreOnly.png'), 
       hu_hist, width = 3.5, height = 2.25, unit = "in", dpi = 300)

#Edge/core-------

df4 <- pivot_longer(data = df,
                    cols = c(Edge0, Core0),
                    names_to = 'type')

model_gamma_hurdle_edge <- brm(
  bf(value ~ WhtPopP * type,
     hu ~ WhtPopP * type),
  data = df4,
  family = hurdle_gamma(),
  chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED,
  silent = 2
)

summary(model_gamma_hurdle_edge)
View(coef(model_gamma_hurdle_edge))
pp_check(model_gamma_hurdle_edge)

df5 <- df4 %>%
  group_by(type) %>%
  add_epred_draws(model_gamma_hurdle_edge, dpar = 'hu') #%>%

regress_edge <- ggplot(data = df5, aes(x = WhtPopP, y = type, color = type, fill = type)) +
  stat_lineribbon(aes(y = .epred), .width = 0.95, alpha = 0.5) +
  scale_colour_manual(values=c("Core0" = "#53813d", 
                               "Edge0" = "#c9a224")) +
  scale_fill_manual(values=alpha(c("Core0" = "#53813d", 
                                   "Edge0" = "#c9a224"), 0.2)) +
  xlab("Percent white population") +
  ylab("Forest cover (ha)") +
  scale_x_continuous(labels = scales::percent, limits = c(0,1)) +
  theme_bw() +
  theme(legend.position = 'none',
        text = element_text(size = 11))

regress_edge

ggsave(paste0(path,'Dropbox/LindsayWorking/GradSchool/Dissertation/Figures/EdgeCoreRegress.png'), 
       regres_edges, width = 3.5, height = 2.25, unit = "in", dpi = 300)


hu_edge <- ggplot(data = df5, aes(x = WhtPopP, y = type, color = type, fill = type)) +
  stat_lineribbon(aes(y = hu), .width = 0.95) +
  scale_colour_manual(values=c("Core0" = "#53813d", 
                               "Edge0" = "#c9a224")) +
  scale_fill_manual(values=alpha(c("Core0" = "#53813d", 
                                   "Edge0" = "#c9a224"), 0.2)) +
  xlab("Percent white population") +
  ylab("Likelihood of zero forests") +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.2)) +
  theme_bw() +
  theme(legend.position = 'none',
        text = element_text(size = 11))

hu_edge

ggsave(paste0(path,'Dropbox/LindsayWorking/GradSchool/Dissertation/Figures/EdgeCoreHu.png'), 
       hu_edge, width = 3.5, height = 2.25, unit = "in", dpi = 300)


#All patches--------------

model_gamma_hurdle_all <- brm(
  bf(All0 ~ WhtPopP,
     hu ~ WhtPopP),
  data = df,
  family = hurdle_gamma(),
  chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED,
  silent = 2
)

summary(model_gamma_hurdle_all)
View(coef(model_gamma_hurdle_all))
pp_check(model_gamma_hurdle_all)

df6 <- df %>%
  add_epred_draws(model_gamma_hurdle_all, dpar = 'hu') #%>%

regress <- ggplot(data = df6, aes(x = WhtPopP, y = All0)) +
  stat_lineribbon(aes(y = .epred), .width = 0.95, alpha = 0.5) +
  scale_colour_manual(values = 'Blue') +
  scale_fill_manual(values=alpha('Blue'), 0.2) +
  xlab("Percent white population") +
  ylab("Forest cover (ha)") +
  scale_x_continuous(labels = scales::percent, limits = c(0,1)) +
  theme_bw() +
  theme(legend.position = 'none',
        text = element_text(size = 11))

regress

ggsave(paste0(path,'Dropbox/LindsayWorking/GradSchool/Dissertation/Figures/AllRegress.png'), 
       regress, width = 3.5, height = 2.25, unit = "in", dpi = 300)


hu <- ggplot(data = df5, aes(x = WhtPopP, y = type, color = type, fill = type)) +
  stat_lineribbon(aes(y = hu), .width = 0.95) +
  scale_colour_manual(values=c("Core0" = "#53813d", 
                               "Edge0" = "#c9a224")) +
  scale_fill_manual(values=alpha(c("Core0" = "#53813d", 
                                   "Edge0" = "#c9a224"), 0.2)) +
  xlab("Percent white population") +
  ylab("Likelihood of zero forests") +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.2)) +
  theme_bw() +
  theme(legend.position = 'none',
        text = element_text(size = 11))

hu

ggsave(paste0(path,'Dropbox/LindsayWorking/GradSchool/Dissertation/Figures/EdgeCoreHu.png'), 
       hu, width = 3.5, height = 2.25, unit = "in", dpi = 300)

#Model results table-------

#Model results from above.
tb <- read_csv('BrmsModelResults.csv')

#Use gt to make a pretty table
gt(tb) %>% 
  gt_theme_guardian() %>% 
  fmt_number(decimals = 2)

#*LiDAR!*------------

#This code is a bit of a bear. The data are stored on multiple
#hard drives, so I cannot run a loop. Therefore, the code
#repeats one time for each county. Sorry.

library(lidR)
library(leafR)

#Load and clip---------

#Each of these layers are on separate hard drives. This makes it impossible to 
#loop this process. Hence the very long, very repetative script.

#Kendall--------

lasKendall <- readLAScatalog("F:/KendallCounty_LAZ")

## Read plots. Specific to each county.

plotKendall <- st_read('AllPlotClippedToPatchCounty.shp') %>% 
  sf::st_transform(., crs = 8733)  %>% #Change to state plane east
  filter(`NAME` == 'Kendall')


# Specify where you want your files to be output and pick name (PlotID)
opt_output_files(lasKendall) <- paste0("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/", "{PlotID}")

#Clip (automatically writes to specified folder)
plotKendalllas <- clip_roi(lasKendall, plotKendall)

chm_groundpts_metrics <- function(plotKendalllas) {
  #ground points are not filtered out here so can get actual plot area 
  
  #set any points < 0.5 m (0503_2022 Dennis Changed the value from 3 m to 0.5) as zero because to get rid of ground points (count as < 0.5 m here, and 
  #very short understory vegetation)
  # I changed it again to 3m 06/23/2022
  
  plotKendalllas@data$Z[plotKendalllas@data$Z <= 3] <- 0
  
  #if then statment to filter out any bad plots (no points or no points > 3)
  if(sum(plotKendalllas@data$Z) > 0) {
    chm <- grid_canopy(plotKendalllas, res = 1, pitfree(c(0,2,5,10,15), c(0, 1.5))) # this uses all the points in the chm
    
    #metrics from a chm (grid_canopy)
    rumple <- rumple_index(chm)
    
    #deep gap fraction (fraction of total rasterized area (1 m2 cells) w/points that are 
    #zero height (> 0.5 m))
    cells <- length(chm@data@values) 
    chm.0 <- chm
    chm.0[is.na(chm.0)] <- 0 
    zeros <- which(chm.0@data@values == 0) 
    deepgaps <- length(zeros) 
    deepgap.fraction <- deepgaps/cells 
    
    out.chm <- data.frame(matrix(c(rumple, deepgap.fraction), ncol = 2))
    
  } else {
    #set rumple to 0, gap fraction to 1 if there were not points > .5 m height or no points
    out.chm <- data.frame(matrix(c(0, 1), ncol = 2))
  }
  
  colnames(out.chm) <- c("rumple", "deepgap.fraction")
  return(out.chm)
}

############################################################
############################################################

##################Dennis corrected the function related to cloud_metrics, because of errors ###
structural_diversity_metrics <- function(plotKendalllas) {
  #ground points are filtered (< .5 m) out of the point cloud in the loop first
  #don't use without filtering ground points
  
  #metrics from a chm (grid_canopy)
  chm <- grid_canopy(plotKendalllas, res = 1, pitfree(c(0,2,5,10,15), c(0, 1.5))) 
  mean.max.canopy.ht <- mean(chm@data@values, na.rm = TRUE) 
  max.canopy.ht <- max(chm@data@values, na.rm=TRUE) 
  top.rugosity <- sd(chm@data@values, na.rm = TRUE) 
  
  #metrics from cloud_metrics (Dennnis changed the cloud metrics since erros occur)
  vert.sd <- sd(plotKendalllas$Z, na.rm = TRUE)  # <- corrected
  meanH <- mean(plotKendalllas$Z, na.rm = TRUE)   # <- corrected
  vertCV <- vert.sd / meanH 
  vertq <- quantile(plotKendalllas$Z, na.rm = TRUE)  # <- corrected
  
  #metrics from grid_metrics
  sd.9m2 <- grid_metrics(plotKendalllas, sd(Z), 3) #9 m2 grid for sd.sd
  sd.sd <- sd(sd.9m2@data@values, na.rm = TRUE) 
  
  #metrics from a Z vector
  Zs <- plotKendalllas@data$Z
  Zs <- Zs[!is.na(Zs)]
  
  
  ### Dennis Changed the height threshold from 3m to 0.5m since there are shrub plots
  # changed it to 3m again 06/23/2022
  gap_frac <- gap_fraction_profile(Zs, dz = 1, z0 = 3) #ignore points < 0.5 m
  GFP <- mean(gap_frac$gf) 
  LADen<-LAD(Zs, dz = 1, k=.5, z0 = 3) #ignore points < 0.5 m   
  VAI <- sum(LADen$lad, na.rm=TRUE) 
  VCI <- VCI(Zs, by = 1, zmax=100) 
  out.plot <- data.frame(matrix(c(mean.max.canopy.ht, max.canopy.ht,
                                  top.rugosity, vert.sd, sd.sd, GFP, VAI,
                                  VCI,  vertCV, vertq), ncol = 14)) 
  colnames(out.plot) <- c("mean.max.canopy.ht",
                          "max.canopy.ht", 
                          "top.rugosity",
                          "vert.sd","sd.sd", "GFP",
                          "VAI", "VCI", "vertCV", 
                          "q0", "q25", "q50", "q75", "q100")
  return(out.plot)
}

#####################################################################################
#STEP 8 FSD metric table 
#################################################################################
#loop through plots that were previously cut to plot area and corrected for elevation

las.names <- list.files("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/", pattern = "*.las") #set location where all plot .las were added

OUT <- NULL
# skip_to_next <- F
#i=1
for(i in 1:length(las.names)) tryCatch({
  #reading in individual las plots without filtering to get area and density
  plotKendalllas <- readLAS(file.path(paste0("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/", las.names[i])))
  plotKendalllas <- filter_poi(plotKendalllas, Classification != LASNOISE)
  print(las.names[i])
  
  #get area so can filter out cropped plots
  plot_area_ground <- area(plotKendalllas)
  
  #total number of points divided by plot area
  den <- length(plotKendalllas@data$Z)/plot_area_ground
  
  #max height to catch if outliers are being removed or something is off with outlier filter
  maxZ <- max(plotKendalllas@data$Z)
  
  #chm area metrics function w/ground points kept 
  chm_metrics_i <- chm_groundpts_metrics(plotKendalllas)
  
  #------------------------------------------------------------------
  #reading in individual las plots again and filtering points < 0.5m
  plotKendalllas <- readLAS(file.path(paste0("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/", las.names[i])), filter = "-drop_z_below .5")
  # filter = "-drop_z_below .5")
  plotKendalllas <- filter_poi(plotKendalllas, Classification != LASNOISE)
  
  #remove any duplicated points ("deprecated points") for heterogeneity metrics
  plotKendalllas <- filter_duplicates(plotKendalllas)
  
  #get area of veg points, might be less than actual area with ground points
  veg_plot_area <- area(plotKendalllas)
  
  #Second FSD metrics function
  #if there are no points b/c all veg is < 0.5 m keep loop running
  if(veg_plot_area > 0) { #if veg > 0.5 v
    #run the FSD metric function
    FSD.i <- structural_diversity_metrics(plotKendalllas)
    
  } else { #if no veg points < 0.5 m
    FSD.i <- data.frame(matrix(rep(0, 14), ncol = 14))
    colnames(FSD.i) <- c("mean.max.canopy.ht",
                         "max.canopy.ht", 
                         "top.rugosity",
                         "vert.sd","sd.sd", "GFP",
                         "VAI", "VCI", "vertCV", 
                         "q0", "q25", "q50", "q75", "q100")
  }
  
  #------------------------------------------------------------------
  #Metrics from leafR
  #reads in las files different than lidR
  normlas.file <- paste0("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/", las.names[i])
  
  #lad profile doesn't use points below 1 m
  #k = 1 effective LAI
  #grain.size = 3 m horizontal grid resolution
  VOXELS_LAD <- lad.voxels(normlas.file, grain.size = 3, k = 1) 
  
  lad_profile <- lad.profile(VOXELS_LAD, relative = T) #uses relative total LAI
  fhd <- FHD(lad_profile, evenness = FALSE, LAD.threshold = -1) #default
  
  lad_profile_lai <- lad.profile(VOXELS_LAD, relative = F) #doesn't use relative LAI
  LAI <- lai(lad_profile_lai, min = 1, max = 75)
  LAI_subcanopy <- lai(lad_profile_lai, min = 1, max = 5)
  
  gini <- GC(normlas.file, threshold = .5)
  
  #######dennis added_12/15/2021
  # plot.volume <- voxel_volumes(plotKendalllas, res= c(5,1))
  #-------------------------------------------------------------------
  out.i <- cbind.data.frame(las.names[i], plot_area_ground, 
                            den, maxZ, chm_metrics_i, FSD.i, 
                            fhd, gini, LAI, LAI_subcanopy)
  OUT <- rbind(OUT, out.i)}, 
  error = function(e) {skip_to_next <<- TRUE})


OUT
colnames(OUT)[1] <- "filename"
FSD <- OUT


#save the results
write.csv(FSD, file = "D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/KendallMetrics.csv")

rm(list=ls())

#Cook----------------

lasCook <- readLAScatalog("F:/CookCounty_LAZ/LAZ")

## Read plots. Specific to each county.

plotCook <- st_read('AllPlotClippedToPatchCounty.shp') %>% 
  sf::st_transform(., crs = 8733) %>%  #Change to state plane east
  filter(`NAME` == 'Cook')

# Specify where you want your files to be output and pick name (PlotID)
opt_output_files(lasCook) <- paste0("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/Cook/", "{PlotID}")

#Clip (automatically writes to specified folder)
plotCooklas <- clip_roi(lasCook, plotCook)

chm_groundpts_metrics <- function(plotCooklas) {
  #ground points are not filtered out here so can get actual plot area 
  
  #set any points < 0.5 m (0503_2022 Dennis Changed the value from 3 m to 0.5) as zero because to get rid of ground points (count as < 0.5 m here, and 
  #very short understory vegetation)
  # I changed it again to 3m 06/23/2022
  
  plotCooklas@data$Z[plotCooklas@data$Z <= 3] <- 0
  
  #if then statment to filter out any bad plots (no points or no points > 3)
  if(sum(plotCooklas@data$Z) > 0) {
    chm <- grid_canopy(plotCooklas, res = 1, pitfree(c(0,2,5,10,15), c(0, 1.5))) # this uses all the points in the chm
    
    #metrics from a chm (grid_canopy)
    rumple <- rumple_index(chm)
    
    #deep gap fraction (fraction of total rasterized area (1 m2 cells) w/points that are 
    #zero height (> 0.5 m))
    cells <- length(chm@data@values) 
    chm.0 <- chm
    chm.0[is.na(chm.0)] <- 0 
    zeros <- which(chm.0@data@values == 0) 
    deepgaps <- length(zeros) 
    deepgap.fraction <- deepgaps/cells 
    
    out.chm <- data.frame(matrix(c(rumple, deepgap.fraction), ncol = 2))
    
  } else {
    #set rumple to 0, gap fraction to 1 if there were not points > .5 m height or no points
    out.chm <- data.frame(matrix(c(0, 1), ncol = 2))
  }
  
  colnames(out.chm) <- c("rumple", "deepgap.fraction")
  return(out.chm)
}

############################################################
############################################################

##################Dennis corrected the function related to cloud_metrics, because of errors ###
structural_diversity_metrics <- function(plotCooklas) {
  #ground points are filtered (< .5 m) out of the point cloud in the loop first
  #don't use without filtering ground points
  
  #metrics from a chm (grid_canopy)
  chm <- grid_canopy(plotCooklas, res = 1, pitfree(c(0,2,5,10,15), c(0, 1.5))) 
  mean.max.canopy.ht <- mean(chm@data@values, na.rm = TRUE) 
  max.canopy.ht <- max(chm@data@values, na.rm=TRUE) 
  top.rugosity <- sd(chm@data@values, na.rm = TRUE) 
  
  #metrics from cloud_metrics (Dennnis changed the cloud metrics since erros occur)
  vert.sd <- sd(plotCooklas$Z, na.rm = TRUE)  # <- corrected
  meanH <- mean(plotCooklas$Z, na.rm = TRUE)   # <- corrected
  vertCV <- vert.sd / meanH 
  vertq <- quantile(plotCooklas$Z, na.rm = TRUE)  # <- corrected
  
  #metrics from grid_metrics
  sd.9m2 <- grid_metrics(plotCooklas, sd(Z), 3) #9 m2 grid for sd.sd
  sd.sd <- sd(sd.9m2@data@values, na.rm = TRUE) 
  
  #metrics from a Z vector
  Zs <- plotCooklas@data$Z
  Zs <- Zs[!is.na(Zs)]
  
  
  ### Dennis Changed the height threshold from 3m to 0.5m since there are shrub plots
  # changed it to 3m again 06/23/2022
  gap_frac <- gap_fraction_profile(Zs, dz = 1, z0 = 3) #ignore points < 0.5 m
  GFP <- mean(gap_frac$gf) 
  LADen<-LAD(Zs, dz = 1, k=.5, z0 = 3) #ignore points < 0.5 m   
  VAI <- sum(LADen$lad, na.rm=TRUE) 
  VCI <- VCI(Zs, by = 1, zmax=100) 
  out.plot <- data.frame(matrix(c(mean.max.canopy.ht, max.canopy.ht,
                                  top.rugosity, vert.sd, sd.sd, GFP, VAI,
                                  VCI,  vertCV, vertq), ncol = 14)) 
  colnames(out.plot) <- c("mean.max.canopy.ht",
                          "max.canopy.ht", 
                          "top.rugosity",
                          "vert.sd","sd.sd", "GFP",
                          "VAI", "VCI", "vertCV", 
                          "q0", "q25", "q50", "q75", "q100")
  return(out.plot)
}

#####################################################################################
#STEP 8 FSD metric table 
#################################################################################
#loop through plots that were previously cut to plot area and corrected for elevation

las.names <- list.files("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/Cook/", pattern = "*.las") #set location where all plot .las were added

OUT <- NULL
# skip_to_next <- F
#i=1
for(i in 1:length(las.names)) tryCatch({
  #reading in individual las plots without filtering to get area and density
  plotCooklas <- readLAS(file.path(paste0("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/Cook/", las.names[i])))
  plotCooklas <- filter_poi(plotCooklas, Classification != LASNOISE)
  print(las.names[i])
  
  #get area so can filter out cropped plots
  plot_area_ground <- area(plotCooklas)
  
  #total number of points divided by plot area
  den <- length(plotCooklas@data$Z)/plot_area_ground
  
  #max height to catch if outliers are being removed or something is off with outlier filter
  maxZ <- max(plotCooklas@data$Z)
  
  #chm area metrics function w/ground points kept 
  chm_metrics_i <- chm_groundpts_metrics(plotCooklas)
  
  #------------------------------------------------------------------
  #reading in individual las plots again and filtering points < 0.5m
  plotCooklas <- readLAS(file.path(paste0("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/Cook/", las.names[i])), filter = "-drop_z_below .5")
  # filter = "-drop_z_below .5")
  plotCooklas <- filter_poi(plotCooklas, Classification != LASNOISE)
  
  #remove any duplicated points ("deprecated points") for heterogeneity metrics
  plotCooklas <- filter_duplicates(plotCooklas)
  
  #get area of veg points, might be less than actual area with ground points
  veg_plot_area <- area(plotCooklas)
  
  #Second FSD metrics function
  #if there are no points b/c all veg is < 0.5 m keep loop running
  if(veg_plot_area > 0) { #if veg > 0.5 v
    #run the FSD metric function
    FSD.i <- structural_diversity_metrics(plotCooklas)
    
  } else { #if no veg points < 0.5 m
    FSD.i <- data.frame(matrix(rep(0, 14), ncol = 14))
    colnames(FSD.i) <- c("mean.max.canopy.ht",
                         "max.canopy.ht", 
                         "top.rugosity",
                         "vert.sd","sd.sd", "GFP",
                         "VAI", "VCI", "vertCV", 
                         "q0", "q25", "q50", "q75", "q100")
  }
  
  #------------------------------------------------------------------
  #Metrics from leafR
  #reads in las files different than lidR
  normlas.file <- paste0("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/Cook/", las.names[i])
  
  #lad profile doesn't use points below 1 m
  #k = 1 effective LAI
  #grain.size = 3 m horizontal grid resolution
  VOXELS_LAD <- lad.voxels(normlas.file, grain.size = 3, k = 1) 
  
  lad_profile <- lad.profile(VOXELS_LAD, relative = T) #uses relative total LAI
  fhd <- FHD(lad_profile, evenness = FALSE, LAD.threshold = -1) #default
  
  lad_profile_lai <- lad.profile(VOXELS_LAD, relative = F) #doesn't use relative LAI
  LAI <- lai(lad_profile_lai, min = 1, max = 75)
  LAI_subcanopy <- lai(lad_profile_lai, min = 1, max = 5)
  
  gini <- GC(normlas.file, threshold = .5)
  
  #######dennis added_12/15/2021
  # plot.volume <- voxel_volumes(plotKendalllas, res= c(5,1))
  #-------------------------------------------------------------------
  out.i <- cbind.data.frame(las.names[i], plot_area_ground, 
                            den, maxZ, chm_metrics_i, FSD.i, 
                            fhd, gini, LAI, LAI_subcanopy)
  OUT <- rbind(OUT, out.i)}, 
  error = function(e) {skip_to_next <<- TRUE})


OUT
colnames(OUT)[1] <- "filename"
FSD <- OUT


#save the results
write.csv(FSD, file = "D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/Cook/CookMetrics.csv")

rm(list=ls())

#McHenry----------------

lasMcHenry <- readLAScatalog("F:/McHenryCounty_LAZ")

## Read plots. Specific to each county.

plotMcHenry <- st_read('AllPlotClippedToPatchCounty.shp') %>% 
  sf::st_transform(., crs = 8733) %>%  #Change to state plane east
  filter(`NAME` == 'McHenry')


# Specify where you want your files to be output and pick name (PlotID)
opt_output_files(lasMcHenry) <- paste0("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/McHenry/", "{PlotID}")

#Clip (automatically writes to specified folder)
plotMcHenrylas <- clip_roi(lasMcHenry, plotMcHenry)

chm_groundpts_metrics <- function(plotMcHenrylas) {
  #ground points are not filtered out here so can get actual plot area 
  
  #set any points < 0.5 m (0503_2022 Dennis Changed the value from 3 m to 0.5) as zero because to get rid of ground points (count as < 0.5 m here, and 
  #very short understory vegetation)
  # I changed it again to 3m 06/23/2022
  
  plotMcHenrylas@data$Z[plotMcHenrylas@data$Z <= 3] <- 0
  
  #if then statment to filter out any bad plots (no points or no points > 3)
  if(sum(plotMcHenrylas@data$Z) > 0) {
    chm <- grid_canopy(plotMcHenrylas, res = 1, pitfree(c(0,2,5,10,15), c(0, 1.5))) # this uses all the points in the chm
    
    #metrics from a chm (grid_canopy)
    rumple <- rumple_index(chm)
    
    #deep gap fraction (fraction of total rasterized area (1 m2 cells) w/points that are 
    #zero height (> 0.5 m))
    cells <- length(chm@data@values) 
    chm.0 <- chm
    chm.0[is.na(chm.0)] <- 0 
    zeros <- which(chm.0@data@values == 0) 
    deepgaps <- length(zeros) 
    deepgap.fraction <- deepgaps/cells 
    
    out.chm <- data.frame(matrix(c(rumple, deepgap.fraction), ncol = 2))
    
  } else {
    #set rumple to 0, gap fraction to 1 if there were not points > .5 m height or no points
    out.chm <- data.frame(matrix(c(0, 1), ncol = 2))
  }
  
  colnames(out.chm) <- c("rumple", "deepgap.fraction")
  return(out.chm)
}

############################################################
############################################################

##################Dennis corrected the function related to cloud_metrics, because of errors ###
structural_diversity_metrics <- function(plotMcHenrylas) {
  #ground points are filtered (< .5 m) out of the point cloud in the loop first
  #don't use without filtering ground points
  
  #metrics from a chm (grid_canopy)
  chm <- grid_canopy(plotMcHenrylas, res = 1, pitfree(c(0,2,5,10,15), c(0, 1.5))) 
  mean.max.canopy.ht <- mean(chm@data@values, na.rm = TRUE) 
  max.canopy.ht <- max(chm@data@values, na.rm=TRUE) 
  top.rugosity <- sd(chm@data@values, na.rm = TRUE) 
  
  #metrics from cloud_metrics (Dennnis changed the cloud metrics since erros occur)
  vert.sd <- sd(plotMcHenrylas$Z, na.rm = TRUE)  # <- corrected
  meanH <- mean(plotMcHenrylas$Z, na.rm = TRUE)   # <- corrected
  vertCV <- vert.sd / meanH 
  vertq <- quantile(plotMcHenrylas$Z, na.rm = TRUE)  # <- corrected
  
  #metrics from grid_metrics
  sd.9m2 <- grid_metrics(plotMcHenrylas, sd(Z), 3) #9 m2 grid for sd.sd
  sd.sd <- sd(sd.9m2@data@values, na.rm = TRUE) 
  
  #metrics from a Z vector
  Zs <- plotMcHenrylas@data$Z
  Zs <- Zs[!is.na(Zs)]
  
  
  ### Dennis Changed the height threshold from 3m to 0.5m since there are shrub plots
  # changed it to 3m again 06/23/2022
  gap_frac <- gap_fraction_profile(Zs, dz = 1, z0 = 3) #ignore points < 0.5 m
  GFP <- mean(gap_frac$gf) 
  LADen<-LAD(Zs, dz = 1, k=.5, z0 = 3) #ignore points < 0.5 m   
  VAI <- sum(LADen$lad, na.rm=TRUE) 
  VCI <- VCI(Zs, by = 1, zmax=100) 
  out.plot <- data.frame(matrix(c(mean.max.canopy.ht, max.canopy.ht,
                                  top.rugosity, vert.sd, sd.sd, GFP, VAI,
                                  VCI,  vertCV, vertq), ncol = 14)) 
  colnames(out.plot) <- c("mean.max.canopy.ht",
                          "max.canopy.ht", 
                          "top.rugosity",
                          "vert.sd","sd.sd", "GFP",
                          "VAI", "VCI", "vertCV", 
                          "q0", "q25", "q50", "q75", "q100")
  return(out.plot)
}

#####################################################################################
#STEP 8 FSD metric table 
#################################################################################
#loop through plots that were previously cut to plot area and corrected for elevation

las.names <- list.files("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/McHenry/", pattern = "*.las") #set location where all plot .las were added

OUT <- NULL
# skip_to_next <- F
#i=1
for(i in 1:length(las.names)) tryCatch({
  #reading in individual las plots without filtering to get area and density
  plotMcHenrylas <- readLAS(file.path(paste0("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/McHenry/", las.names[i])))
  plotMcHenrylas <- filter_poi(plotMcHenrylas, Classification != LASNOISE)
  print(las.names[i])
  
  #get area so can filter out cropped plots
  plot_area_ground <- area(plotMcHenrylas)
  
  #total number of points divided by plot area
  den <- length(plotMcHenrylas@data$Z)/plot_area_ground
  
  #max height to catch if outliers are being removed or something is off with outlier filter
  maxZ <- max(plotMcHenrylas@data$Z)
  
  #chm area metrics function w/ground points kept 
  chm_metrics_i <- chm_groundpts_metrics(plotMcHenrylas)
  
  #------------------------------------------------------------------
  #reading in individual las plots again and filtering points < 0.5m
  plotMcHenrylas <- readLAS(file.path(paste0("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/McHenry/", las.names[i])), filter = "-drop_z_below .5")
  # filter = "-drop_z_below .5")
  plotMcHenrylas <- filter_poi(plotMcHenrylas, Classification != LASNOISE)
  
  #remove any duplicated points ("deprecated points") for heterogeneity metrics
  plotMcHenrylas <- filter_duplicates(plotMcHenrylas)
  
  #get area of veg points, might be less than actual area with ground points
  veg_plot_area <- area(plotMcHenrylas)
  
  #Second FSD metrics function
  #if there are no points b/c all veg is < 0.5 m keep loop running
  if(veg_plot_area > 0) { #if veg > 0.5 v
    #run the FSD metric function
    FSD.i <- structural_diversity_metrics(plotMcHenrylas)
    
  } else { #if no veg points < 0.5 m
    FSD.i <- data.frame(matrix(rep(0, 14), ncol = 14))
    colnames(FSD.i) <- c("mean.max.canopy.ht",
                         "max.canopy.ht", 
                         "top.rugosity",
                         "vert.sd","sd.sd", "GFP",
                         "VAI", "VCI", "vertCV", 
                         "q0", "q25", "q50", "q75", "q100")
  }
  
  #------------------------------------------------------------------
  #Metrics from leafR
  #reads in las files different than lidR
  normlas.file <- paste0("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/McHenry/", las.names[i])
  
  #lad profile doesn't use points below 1 m
  #k = 1 effective LAI
  #grain.size = 3 m horizontal grid resolution
  VOXELS_LAD <- lad.voxels(normlas.file, grain.size = 3, k = 1) 
  
  lad_profile <- lad.profile(VOXELS_LAD, relative = T) #uses relative total LAI
  fhd <- FHD(lad_profile, evenness = FALSE, LAD.threshold = -1) #default
  
  lad_profile_lai <- lad.profile(VOXELS_LAD, relative = F) #doesn't use relative LAI
  LAI <- lai(lad_profile_lai, min = 1, max = 75)
  LAI_subcanopy <- lai(lad_profile_lai, min = 1, max = 5)
  
  gini <- GC(normlas.file, threshold = .5)
  
  #######dennis added_12/15/2021
  # plot.volume <- voxel_volumes(plotMcHenrylas, res= c(5,1))
  #-------------------------------------------------------------------
  out.i <- cbind.data.frame(las.names[i], plot_area_ground, 
                            den, maxZ, chm_metrics_i, FSD.i, 
                            fhd, gini, LAI, LAI_subcanopy)
  OUT <- rbind(OUT, out.i)}, 
  error = function(e) {skip_to_next <<- TRUE})


OUT
colnames(OUT)[1] <- "filename"
FSD <- OUT

#save the results
write.csv(FSD, file = "D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/McHenry/McHenryMetrics.csv")

rm(list=ls())

#Kane----------------

lasKane <- readLAScatalog("F:/KaneCounty/las_rgb_20ppsm")

## Read plots. Specific to each county.

plotKane <- st_read('AllPlotClippedToPatchCounty.shp') %>% 
  sf::st_transform(., crs = 8733) %>%  #Change to state plane east
  filter(`NAME` == 'Kane')


# Specify where you want your files to be output and pick name (PlotID)
opt_output_files(lasKane) <- paste0("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/Kane/", "{PlotID}")

#Clip (automatically writes to specified folder)
plotKanelas <- clip_roi(lasKane, plotKane)

chm_groundpts_metrics <- function(plotKanelas) {
  #ground points are not filtered out here so can get actual plot area 
  
  #set any points < 0.5 m (0503_2022 Dennis Changed the value from 3 m to 0.5) as zero because to get rid of ground points (count as < 0.5 m here, and 
  #very short understory vegetation)
  # I changed it again to 3m 06/23/2022
  
  plotKanelas@data$Z[plotKanelas@data$Z <= 3] <- 0
  
  #if then statment to filter out any bad plots (no points or no points > 3)
  if(sum(plotKanelas@data$Z) > 0) {
    chm <- grid_canopy(plotKanelas, res = 1, pitfree(c(0,2,5,10,15), c(0, 1.5))) # this uses all the points in the chm
    
    #metrics from a chm (grid_canopy)
    rumple <- rumple_index(chm)
    
    #deep gap fraction (fraction of total rasterized area (1 m2 cells) w/points that are 
    #zero height (> 0.5 m))
    cells <- length(chm@data@values) 
    chm.0 <- chm
    chm.0[is.na(chm.0)] <- 0 
    zeros <- which(chm.0@data@values == 0) 
    deepgaps <- length(zeros) 
    deepgap.fraction <- deepgaps/cells 
    
    out.chm <- data.frame(matrix(c(rumple, deepgap.fraction), ncol = 2))
    
  } else {
    #set rumple to 0, gap fraction to 1 if there were not points > .5 m height or no points
    out.chm <- data.frame(matrix(c(0, 1), ncol = 2))
  }
  
  colnames(out.chm) <- c("rumple", "deepgap.fraction")
  return(out.chm)
}

############################################################
############################################################

##################Dennis corrected the function related to cloud_metrics, because of errors ###
structural_diversity_metrics <- function(plotKanelas) {
  #ground points are filtered (< .5 m) out of the point cloud in the loop first
  #don't use without filtering ground points
  
  #metrics from a chm (grid_canopy)
  chm <- grid_canopy(plotKanelas, res = 1, pitfree(c(0,2,5,10,15), c(0, 1.5))) 
  mean.max.canopy.ht <- mean(chm@data@values, na.rm = TRUE) 
  max.canopy.ht <- max(chm@data@values, na.rm=TRUE) 
  top.rugosity <- sd(chm@data@values, na.rm = TRUE) 
  
  #metrics from cloud_metrics (Dennnis changed the cloud metrics since erros occur)
  vert.sd <- sd(plotKanelas$Z, na.rm = TRUE)  # <- corrected
  meanH <- mean(plotKanelas$Z, na.rm = TRUE)   # <- corrected
  vertCV <- vert.sd / meanH 
  vertq <- quantile(plotKanelas$Z, na.rm = TRUE)  # <- corrected
  
  #metrics from grid_metrics
  sd.9m2 <- grid_metrics(plotKanelas, sd(Z), 3) #9 m2 grid for sd.sd
  sd.sd <- sd(sd.9m2@data@values, na.rm = TRUE) 
  
  #metrics from a Z vector
  Zs <- plotKanelas@data$Z
  Zs <- Zs[!is.na(Zs)]
  
  
  ### Dennis Changed the height threshold from 3m to 0.5m since there are shrub plots
  # changed it to 3m again 06/23/2022
  gap_frac <- gap_fraction_profile(Zs, dz = 1, z0 = 3) #ignore points < 0.5 m
  GFP <- mean(gap_frac$gf) 
  LADen<-LAD(Zs, dz = 1, k=.5, z0 = 3) #ignore points < 0.5 m   
  VAI <- sum(LADen$lad, na.rm=TRUE) 
  VCI <- VCI(Zs, by = 1, zmax=100) 
  out.plot <- data.frame(matrix(c(mean.max.canopy.ht, max.canopy.ht,
                                  top.rugosity, vert.sd, sd.sd, GFP, VAI,
                                  VCI,  vertCV, vertq), ncol = 14)) 
  colnames(out.plot) <- c("mean.max.canopy.ht",
                          "max.canopy.ht", 
                          "top.rugosity",
                          "vert.sd","sd.sd", "GFP",
                          "VAI", "VCI", "vertCV", 
                          "q0", "q25", "q50", "q75", "q100")
  return(out.plot)
}

#####################################################################################
#STEP 8 FSD metric table 
#################################################################################
#loop through plots that were previously cut to plot area and corrected for elevation

las.names <- list.files("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/Kane/", pattern = "*.las") #set location where all plot .las were added

OUT <- NULL
# skip_to_next <- F
#i=1
for(i in 1:length(las.names)) tryCatch({
  #reading in individual las plots without filtering to get area and density
  plotKanelas <- readLAS(file.path(paste0("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/Kane/", las.names[i])))
  plotKanelas <- filter_poi(plotKanelas, Classification != LASNOISE)
  print(las.names[i])
  
  #get area so can filter out cropped plots
  plot_area_ground <- area(plotKanelas)
  
  #total number of points divided by plot area
  den <- length(plotKanelas@data$Z)/plot_area_ground
  
  #max height to catch if outliers are being removed or something is off with outlier filter
  maxZ <- max(plotKanelas@data$Z)
  
  #chm area metrics function w/ground points kept 
  chm_metrics_i <- chm_groundpts_metrics(plotKanelas)
  
  #------------------------------------------------------------------
  #reading in individual las plots again and filtering points < 0.5m
  plotKanelas <- readLAS(file.path(paste0("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/Kane/", las.names[i])), filter = "-drop_z_below .5")
  # filter = "-drop_z_below .5")
  plotKanelas <- filter_poi(plotKanelas, Classification != LASNOISE)
  
  #remove any duplicated points ("deprecated points") for heterogeneity metrics
  plotKanelas <- filter_duplicates(plotKanelas)
  
  #get area of veg points, might be less than actual area with ground points
  veg_plot_area <- area(plotKanelas)
  
  #Second FSD metrics function
  #if there are no points b/c all veg is < 0.5 m keep loop running
  if(veg_plot_area > 0) { #if veg > 0.5 v
    #run the FSD metric function
    FSD.i <- structural_diversity_metrics(plotKanelas)
    
  } else { #if no veg points < 0.5 m
    FSD.i <- data.frame(matrix(rep(0, 14), ncol = 14))
    colnames(FSD.i) <- c("mean.max.canopy.ht",
                         "max.canopy.ht", 
                         "top.rugosity",
                         "vert.sd","sd.sd", "GFP",
                         "VAI", "VCI", "vertCV", 
                         "q0", "q25", "q50", "q75", "q100")
  }
  
  #------------------------------------------------------------------
  #Metrics from leafR
  #reads in las files different than lidR
  normlas.file <- paste0("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/Kane/", las.names[i])
  
  #lad profile doesn't use points below 1 m
  #k = 1 effective LAI
  #grain.size = 3 m horizontal grid resolution
  VOXELS_LAD <- lad.voxels(normlas.file, grain.size = 3, k = 1) 
  
  lad_profile <- lad.profile(VOXELS_LAD, relative = T) #uses relative total LAI
  fhd <- FHD(lad_profile, evenness = FALSE, LAD.threshold = -1) #default
  
  lad_profile_lai <- lad.profile(VOXELS_LAD, relative = F) #doesn't use relative LAI
  LAI <- lai(lad_profile_lai, min = 1, max = 75)
  LAI_subcanopy <- lai(lad_profile_lai, min = 1, max = 5)
  
  gini <- GC(normlas.file, threshold = .5)
  
  #######dennis added_12/15/2021
  # plot.volume <- voxel_volumes(plotKanelas, res= c(5,1))
  #-------------------------------------------------------------------
  out.i <- cbind.data.frame(las.names[i], plot_area_ground, 
                            den, maxZ, chm_metrics_i, FSD.i, 
                            fhd, gini, LAI, LAI_subcanopy)
  OUT <- rbind(OUT, out.i)}, 
  error = function(e) {skip_to_next <<- TRUE})


OUT
colnames(OUT)[1] <- "filename"
FSD <- OUT

#save the results
write.csv(FSD, file = "D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/Kane/KaneMetrics.csv")

rm(list=ls())

#DuPage----------------

lasDuPage <- readLAScatalog("F:/DuPageCounty_LAS_Files/LAS")

## Read plots. Specific to each county.

plotDuPage <- st_read('AllPlotClippedToPatchCounty.shp') %>% 
  sf::st_transform(., crs = 8733) %>%  #Change to state plane east
  filter(`NAME` == 'DuPage')


# Specify where you want your files to be output and pick name (PlotID)
opt_output_files(lasDuPage) <- paste0("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/DuPage/", "{PlotID}")

#Clip (automatically writes to specified folder)
plotDuPagelas <- clip_roi(lasDuPage, plotDuPage)

chm_groundpts_metrics <- function(plotDuPagelas) {
  #ground points are not filtered out here so can get actual plot area 
  
  #set any points < 0.5 m (0503_2022 Dennis Changed the value from 3 m to 0.5) as zero because to get rid of ground points (count as < 0.5 m here, and 
  #very short understory vegetation)
  # I changed it again to 3m 06/23/2022
  
  plotDuPagelas@data$Z[plotDuPagelas@data$Z <= 3] <- 0
  
  #if then statment to filter out any bad plots (no points or no points > 3)
  if(sum(plotDuPagelas@data$Z) > 0) {
    chm <- grid_canopy(plotDuPagelas, res = 1, pitfree(c(0,2,5,10,15), c(0, 1.5))) # this uses all the points in the chm
    
    #metrics from a chm (grid_canopy)
    rumple <- rumple_index(chm)
    
    #deep gap fraction (fraction of total rasterized area (1 m2 cells) w/points that are 
    #zero height (> 0.5 m))
    cells <- length(chm@data@values) 
    chm.0 <- chm
    chm.0[is.na(chm.0)] <- 0 
    zeros <- which(chm.0@data@values == 0) 
    deepgaps <- length(zeros) 
    deepgap.fraction <- deepgaps/cells 
    
    out.chm <- data.frame(matrix(c(rumple, deepgap.fraction), ncol = 2))
    
  } else {
    #set rumple to 0, gap fraction to 1 if there were not points > .5 m height or no points
    out.chm <- data.frame(matrix(c(0, 1), ncol = 2))
  }
  
  colnames(out.chm) <- c("rumple", "deepgap.fraction")
  return(out.chm)
}

############################################################
############################################################

##################Dennis corrected the function related to cloud_metrics, because of errors ###
structural_diversity_metrics <- function(plotDuPagelas) {
  #ground points are filtered (< .5 m) out of the point cloud in the loop first
  #don't use without filtering ground points
  
  #metrics from a chm (grid_canopy)
  chm <- grid_canopy(plotDuPagelas, res = 1, pitfree(c(0,2,5,10,15), c(0, 1.5))) 
  mean.max.canopy.ht <- mean(chm@data@values, na.rm = TRUE) 
  max.canopy.ht <- max(chm@data@values, na.rm=TRUE) 
  top.rugosity <- sd(chm@data@values, na.rm = TRUE) 
  
  #metrics from cloud_metrics (Dennnis changed the cloud metrics since erros occur)
  vert.sd <- sd(plotDuPagelas$Z, na.rm = TRUE)  # <- corrected
  meanH <- mean(plotDuPagelas$Z, na.rm = TRUE)   # <- corrected
  vertCV <- vert.sd / meanH 
  vertq <- quantile(plotDuPagelas$Z, na.rm = TRUE)  # <- corrected
  
  #metrics from grid_metrics
  sd.9m2 <- grid_metrics(plotDuPagelas, sd(Z), 3) #9 m2 grid for sd.sd
  sd.sd <- sd(sd.9m2@data@values, na.rm = TRUE) 
  
  #metrics from a Z vector
  Zs <- plotDuPagelas@data$Z
  Zs <- Zs[!is.na(Zs)]
  
  
  ### Dennis Changed the height threshold from 3m to 0.5m since there are shrub plots
  # changed it to 3m again 06/23/2022
  gap_frac <- gap_fraction_profile(Zs, dz = 1, z0 = 3) #ignore points < 0.5 m
  GFP <- mean(gap_frac$gf) 
  LADen<-LAD(Zs, dz = 1, k=.5, z0 = 3) #ignore points < 0.5 m   
  VAI <- sum(LADen$lad, na.rm=TRUE) 
  VCI <- VCI(Zs, by = 1, zmax=100) 
  out.plot <- data.frame(matrix(c(mean.max.canopy.ht, max.canopy.ht,
                                  top.rugosity, vert.sd, sd.sd, GFP, VAI,
                                  VCI,  vertCV, vertq), ncol = 14)) 
  colnames(out.plot) <- c("mean.max.canopy.ht",
                          "max.canopy.ht", 
                          "top.rugosity",
                          "vert.sd","sd.sd", "GFP",
                          "VAI", "VCI", "vertCV", 
                          "q0", "q25", "q50", "q75", "q100")
  return(out.plot)
}

#####################################################################################
#STEP 8 FSD metric table 
#################################################################################
#loop through plots that were previously cut to plot area and corrected for elevation

las.names <- list.files("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/DuPage/", pattern = "*.las") #set location where all plot .las were added

OUT <- NULL
# skip_to_next <- F
#i=1
for(i in 1:length(las.names)) tryCatch({
  #reading in individual las plots without filtering to get area and density
  plotDuPagelas <- readLAS(file.path(paste0("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/DuPage/", las.names[i])))
  plotDuPagelas <- filter_poi(plotDuPagelas, Classification != LASNOISE)
  print(las.names[i])
  
  #get area so can filter out cropped plots
  plot_area_ground <- area(plotDuPagelas)
  
  #total number of points divided by plot area
  den <- length(plotDuPagelas@data$Z)/plot_area_ground
  
  #max height to catch if outliers are being removed or something is off with outlier filter
  maxZ <- max(plotDuPagelas@data$Z)
  
  #chm area metrics function w/ground points kept 
  chm_metrics_i <- chm_groundpts_metrics(plotDuPagelas)
  
  #------------------------------------------------------------------
  #reading in individual las plots again and filtering points < 0.5m
  plotDuPagelas <- readLAS(file.path(paste0("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/DuPage/", las.names[i])), filter = "-drop_z_below .5")
  # filter = "-drop_z_below .5")
  plotDuPagelas <- filter_poi(plotDuPagelas, Classification != LASNOISE)
  
  #remove any duplicated points ("deprecated points") for heterogeneity metrics
  plotDuPagelas <- filter_duplicates(plotDuPagelas)
  
  #get area of veg points, might be less than actual area with ground points
  veg_plot_area <- area(plotDuPagelas)
  
  #Second FSD metrics function
  #if there are no points b/c all veg is < 0.5 m keep loop running
  if(veg_plot_area > 0) { #if veg > 0.5 v
    #run the FSD metric function
    FSD.i <- structural_diversity_metrics(plotDuPagelas)
    
  } else { #if no veg points < 0.5 m
    FSD.i <- data.frame(matrix(rep(0, 14), ncol = 14))
    colnames(FSD.i) <- c("mean.max.canopy.ht",
                         "max.canopy.ht", 
                         "top.rugosity",
                         "vert.sd","sd.sd", "GFP",
                         "VAI", "VCI", "vertCV", 
                         "q0", "q25", "q50", "q75", "q100")
  }
  
  #------------------------------------------------------------------
  #Metrics from leafR
  #reads in las files different than lidR
  normlas.file <- paste0("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/DuPage/", las.names[i])
  
  #lad profile doesn't use points below 1 m
  #k = 1 effective LAI
  #grain.size = 3 m horizontal grid resolution
  VOXELS_LAD <- lad.voxels(normlas.file, grain.size = 3, k = 1) 
  
  lad_profile <- lad.profile(VOXELS_LAD, relative = T) #uses relative total LAI
  fhd <- FHD(lad_profile, evenness = FALSE, LAD.threshold = -1) #default
  
  lad_profile_lai <- lad.profile(VOXELS_LAD, relative = F) #doesn't use relative LAI
  LAI <- lai(lad_profile_lai, min = 1, max = 75)
  LAI_subcanopy <- lai(lad_profile_lai, min = 1, max = 5)
  
  gini <- GC(normlas.file, threshold = .5)
  
  #######dennis added_12/15/2021
  # plot.volume <- voxel_volumes(plotDuPagelas, res= c(5,1))
  #-------------------------------------------------------------------
  out.i <- cbind.data.frame(las.names[i], plot_area_ground, 
                            den, maxZ, chm_metrics_i, FSD.i, 
                            fhd, gini, LAI, LAI_subcanopy)
  OUT <- rbind(OUT, out.i)}, 
  error = function(e) {skip_to_next <<- TRUE})


OUT
colnames(OUT)[1] <- "filename"
FSD <- OUT

#save the results
write.csv(FSD, file = "D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/DuPage/DuPageMetrics.csv")

rm(list=ls())

#Lake----------------

lasLake <- readLAScatalog("F:/LakeCounty/LAZ")

## Read plots. Specific to each county.

plotLake <- st_read('AllPlotClippedToPatchCounty.shp') %>% 
  sf::st_transform(., crs = 8733) %>%  #Change to state plane east
  filter(`NAME` == 'Lake')


# Specify where you want your files to be output and pick name (PlotID)
opt_output_files(lasLake) <- paste0("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/Lake/", "{PlotID}")

#Clip (automatically writes to specified folder)
plotLakelas <- clip_roi(lasLake, plotLake)

chm_groundpts_metrics <- function(plotLakelas) {
  #ground points are not filtered out here so can get actual plot area 
  
  #set any points < 0.5 m (0503_2022 Dennis Changed the value from 3 m to 0.5) as zero because to get rid of ground points (count as < 0.5 m here, and 
  #very short understory vegetation)
  # I changed it again to 3m 06/23/2022
  
  plotLakelas@data$Z[plotLakelas@data$Z <= 3] <- 0
  
  #if then statment to filter out any bad plots (no points or no points > 3)
  if(sum(plotLakelas@data$Z) > 0) {
    chm <- grid_canopy(plotLakelas, res = 1, pitfree(c(0,2,5,10,15), c(0, 1.5))) # this uses all the points in the chm
    
    #metrics from a chm (grid_canopy)
    rumple <- rumple_index(chm)
    
    #deep gap fraction (fraction of total rasterized area (1 m2 cells) w/points that are 
    #zero height (> 0.5 m))
    cells <- length(chm@data@values) 
    chm.0 <- chm
    chm.0[is.na(chm.0)] <- 0 
    zeros <- which(chm.0@data@values == 0) 
    deepgaps <- length(zeros) 
    deepgap.fraction <- deepgaps/cells 
    
    out.chm <- data.frame(matrix(c(rumple, deepgap.fraction), ncol = 2))
    
  } else {
    #set rumple to 0, gap fraction to 1 if there were not points > .5 m height or no points
    out.chm <- data.frame(matrix(c(0, 1), ncol = 2))
  }
  
  colnames(out.chm) <- c("rumple", "deepgap.fraction")
  return(out.chm)
}

############################################################
############################################################

##################Dennis corrected the function related to cloud_metrics, because of errors ###
structural_diversity_metrics <- function(plotLakelas) {
  #ground points are filtered (< .5 m) out of the point cloud in the loop first
  #don't use without filtering ground points
  
  #metrics from a chm (grid_canopy)
  chm <- grid_canopy(plotLakelas, res = 1, pitfree(c(0,2,5,10,15), c(0, 1.5))) 
  mean.max.canopy.ht <- mean(chm@data@values, na.rm = TRUE) 
  max.canopy.ht <- max(chm@data@values, na.rm=TRUE) 
  top.rugosity <- sd(chm@data@values, na.rm = TRUE) 
  
  #metrics from cloud_metrics (Dennnis changed the cloud metrics since erros occur)
  vert.sd <- sd(plotLakelas$Z, na.rm = TRUE)  # <- corrected
  meanH <- mean(plotLakelas$Z, na.rm = TRUE)   # <- corrected
  vertCV <- vert.sd / meanH 
  vertq <- quantile(plotLakelas$Z, na.rm = TRUE)  # <- corrected
  
  #metrics from grid_metrics
  sd.9m2 <- grid_metrics(plotLakelas, sd(Z), 3) #9 m2 grid for sd.sd
  sd.sd <- sd(sd.9m2@data@values, na.rm = TRUE) 
  
  #metrics from a Z vector
  Zs <- plotLakelas@data$Z
  Zs <- Zs[!is.na(Zs)]
  
  
  ### Dennis Changed the height threshold from 3m to 0.5m since there are shrub plots
  # changed it to 3m again 06/23/2022
  gap_frac <- gap_fraction_profile(Zs, dz = 1, z0 = 3) #ignore points < 0.5 m
  GFP <- mean(gap_frac$gf) 
  LADen<-LAD(Zs, dz = 1, k=.5, z0 = 3) #ignore points < 0.5 m   
  VAI <- sum(LADen$lad, na.rm=TRUE) 
  VCI <- VCI(Zs, by = 1, zmax=100) 
  out.plot <- data.frame(matrix(c(mean.max.canopy.ht, max.canopy.ht,
                                  top.rugosity, vert.sd, sd.sd, GFP, VAI,
                                  VCI,  vertCV, vertq), ncol = 14)) 
  colnames(out.plot) <- c("mean.max.canopy.ht",
                          "max.canopy.ht", 
                          "top.rugosity",
                          "vert.sd","sd.sd", "GFP",
                          "VAI", "VCI", "vertCV", 
                          "q0", "q25", "q50", "q75", "q100")
  return(out.plot)
}

#####################################################################################
#STEP 8 FSD metric table 
#################################################################################
#loop through plots that were previously cut to plot area and corrected for elevation

las.names <- list.files("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/Lake/", pattern = "*.las") #set location where all plot .las were added

OUT <- NULL
# skip_to_next <- F
#i=1
for(i in 1:length(las.names)) tryCatch({
  #reading in individual las plots without filtering to get area and density
  plotLakelas <- readLAS(file.path(paste0("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/Lake/", las.names[i])))
  plotLakelas <- filter_poi(plotLakelas, Classification != LASNOISE)
  print(las.names[i])
  
  #get area so can filter out cropped plots
  plot_area_ground <- area(plotLakelas)
  
  #total number of points divided by plot area
  den <- length(plotLakelas@data$Z)/plot_area_ground
  
  #max height to catch if outliers are being removed or something is off with outlier filter
  maxZ <- max(plotLakelas@data$Z)
  
  #chm area metrics function w/ground points kept 
  chm_metrics_i <- chm_groundpts_metrics(plotLakelas)
  
  #------------------------------------------------------------------
  #reading in individual las plots again and filtering points < 0.5m
  plotLakelas <- readLAS(file.path(paste0("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/Lake/", las.names[i])), filter = "-drop_z_below .5")
  # filter = "-drop_z_below .5")
  plotLakelas <- filter_poi(plotLakelas, Classification != LASNOISE)
  
  #remove any duplicated points ("deprecated points") for heterogeneity metrics
  plotLakelas <- filter_duplicates(plotLakelas)
  
  #get area of veg points, might be less than actual area with ground points
  veg_plot_area <- area(plotLakelas)
  
  #Second FSD metrics function
  #if there are no points b/c all veg is < 0.5 m keep loop running
  if(veg_plot_area > 0) { #if veg > 0.5 v
    #run the FSD metric function
    FSD.i <- structural_diversity_metrics(plotLakelas)
    
  } else { #if no veg points < 0.5 m
    FSD.i <- data.frame(matrix(rep(0, 14), ncol = 14))
    colnames(FSD.i) <- c("mean.max.canopy.ht",
                         "max.canopy.ht", 
                         "top.rugosity",
                         "vert.sd","sd.sd", "GFP",
                         "VAI", "VCI", "vertCV", 
                         "q0", "q25", "q50", "q75", "q100")
  }
  
  #------------------------------------------------------------------
  #Metrics from leafR
  #reads in las files different than lidR
  normlas.file <- paste0("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/Lake/", las.names[i])
  
  #lad profile doesn't use points below 1 m
  #k = 1 effective LAI
  #grain.size = 3 m horizontal grid resolution
  VOXELS_LAD <- lad.voxels(normlas.file, grain.size = 3, k = 1) 
  
  lad_profile <- lad.profile(VOXELS_LAD, relative = T) #uses relative total LAI
  fhd <- FHD(lad_profile, evenness = FALSE, LAD.threshold = -1) #default
  
  lad_profile_lai <- lad.profile(VOXELS_LAD, relative = F) #doesn't use relative LAI
  LAI <- lai(lad_profile_lai, min = 1, max = 75)
  LAI_subcanopy <- lai(lad_profile_lai, min = 1, max = 5)
  
  gini <- GC(normlas.file, threshold = .5)
  
  #######dennis added_12/15/2021
  # plot.volume <- voxel_volumes(plotLakelas, res= c(5,1))
  #-------------------------------------------------------------------
  out.i <- cbind.data.frame(las.names[i], plot_area_ground, 
                            den, maxZ, chm_metrics_i, FSD.i, 
                            fhd, gini, LAI, LAI_subcanopy)
  OUT <- rbind(OUT, out.i)}, 
  error = function(e) {skip_to_next <<- TRUE})


OUT
colnames(OUT)[1] <- "filename"
FSD <- OUT

#save the results
write.csv(FSD, file = "D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/Lake/LakeMetrics.csv")

rm(list=ls())

#Will----------------

lasWill <- readLAScatalog("F:/las")

## Read plots. Specific to each county.

plotWill <- st_read('AllPlotClippedToPatchCounty.shp') %>% 
  sf::st_transform(., crs = 8733) %>%  #Change to state plane east
  filter(`NAME` == 'Will')

# Specify where you want your files to be output and pick name (PlotID)
opt_output_files(lasWill) <- paste0("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/Will/", "{PlotID}")

#Clip (automatically writes to specified folder)
plotWilllas <- clip_roi(lasWill, plotWill)

chm_groundpts_metrics <- function(plotWilllas) {
  #ground points are not filtered out here so can get actual plot area 
  
  #set any points < 0.5 m 
  
  plotWilllas@data$Z[plotWilllas@data$Z <= 3] <- 0
  
  #if then statment to filter out any bad plots (no points or no points > 3)
  if(sum(plotWilllas@data$Z) > 0) {
    chm <- grid_canopy(plotWilllas, res = 1, pitfree(c(0,2,5,10,15), c(0, 1.5))) # this uses all the points in the chm
    
    #metrics from a chm (grid_canopy)
    rumple <- rumple_index(chm)
    
    #deep gap fraction (fraction of total rasterized area (1 m2 cells) w/points that are 
    #zero height (> 0.5 m))
    cells <- length(chm@data@values) 
    chm.0 <- chm
    chm.0[is.na(chm.0)] <- 0 
    zeros <- which(chm.0@data@values == 0) 
    deepgaps <- length(zeros) 
    deepgap.fraction <- deepgaps/cells 
    
    out.chm <- data.frame(matrix(c(rumple, deepgap.fraction), ncol = 2))
    
  } else {
    #set rumple to 0, gap fraction to 1 if there were not points > .5 m height or no points
    out.chm <- data.frame(matrix(c(0, 1), ncol = 2))
  }
  
  colnames(out.chm) <- c("rumple", "deepgap.fraction")
  return(out.chm)
}

############################################################
############################################################

##################Dennis corrected the function related to cloud_metrics, because of errors ###
structural_diversity_metrics <- function(plotWilllas) {
  #ground points are filtered (< .5 m) out of the point cloud in the loop first
  #don't use without filtering ground points
  
  #metrics from a chm (grid_canopy)
  chm <- grid_canopy(plotWilllas, res = 1, pitfree(c(0,2,5,10,15), c(0, 1.5))) 
  mean.max.canopy.ht <- mean(chm@data@values, na.rm = TRUE) 
  max.canopy.ht <- max(chm@data@values, na.rm=TRUE) 
  top.rugosity <- sd(chm@data@values, na.rm = TRUE) 
  
  #metrics from cloud_metrics (Dennnis changed the cloud metrics since erros occur)
  vert.sd <- sd(plotWilllas$Z, na.rm = TRUE)  # <- corrected
  meanH <- mean(plotWilllas$Z, na.rm = TRUE)   # <- corrected
  vertCV <- vert.sd / meanH 
  vertq <- quantile(plotWilllas$Z, na.rm = TRUE)  # <- corrected
  
  #metrics from grid_metrics
  sd.9m2 <- grid_metrics(plotWilllas, sd(Z), 3) #9 m2 grid for sd.sd
  sd.sd <- sd(sd.9m2@data@values, na.rm = TRUE) 
  
  #metrics from a Z vector
  Zs <- plotWilllas@data$Z
  Zs <- Zs[!is.na(Zs)]
  
  
  ### Dennis Changed the height threshold from 3m to 0.5m since there are shrub plots
  # changed it to 3m again 06/23/2022
  gap_frac <- gap_fraction_profile(Zs, dz = 1, z0 = 3) #ignore points < 0.5 m
  GFP <- mean(gap_frac$gf) 
  LADen<-LAD(Zs, dz = 1, k=.5, z0 = 3) #ignore points < 0.5 m   
  VAI <- sum(LADen$lad, na.rm=TRUE) 
  VCI <- VCI(Zs, by = 1, zmax=100) 
  out.plot <- data.frame(matrix(c(mean.max.canopy.ht, max.canopy.ht,
                                  top.rugosity, vert.sd, sd.sd, GFP, VAI,
                                  VCI,  vertCV, vertq), ncol = 14)) 
  colnames(out.plot) <- c("mean.max.canopy.ht",
                          "max.canopy.ht", 
                          "top.rugosity",
                          "vert.sd","sd.sd", "GFP",
                          "VAI", "VCI", "vertCV", 
                          "q0", "q25", "q50", "q75", "q100")
  return(out.plot)
}

#####################################################################################
#STEP 8 FSD metric table 
#################################################################################
#loop through plots that were previously cut to plot area and corrected for elevation

las.names <- list.files("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/Will/", pattern = "*.las") #set location where all plot .las were added

OUT <- NULL
# skip_to_next <- F
#i=1
for(i in 1:length(las.names)) tryCatch({
  #reading in individual las plots without filtering to get area and density
  plotWilllas <- readLAS(file.path(paste0("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/Will/", las.names[i])))
  plotWilllas <- filter_poi(plotWilllas, Classification != LASNOISE)
  print(las.names[i])
  
  #get area so can filter out cropped plots
  plot_area_ground <- area(plotWilllas)
  
  #total number of points divided by plot area
  den <- length(plotWilllas@data$Z)/plot_area_ground
  
  #max height to catch if outliers are being removed or something is off with outlier filter
  maxZ <- max(plotWilllas@data$Z)
  
  #chm area metrics function w/ground points kept 
  chm_metrics_i <- chm_groundpts_metrics(plotWilllas)
  
  #------------------------------------------------------------------
  #reading in individual las plots again and filtering points < 0.5m
  plotWilllas <- readLAS(file.path(paste0("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/Will/", las.names[i])), filter = "-drop_z_below .5")
  # filter = "-drop_z_below .5")
  plotWilllas <- filter_poi(plotWilllas, Classification != LASNOISE)
  
  #remove any duplicated points ("deprecated points") for heterogeneity metrics
  plotWilllas <- filter_duplicates(plotWilllas)
  
  #get area of veg points, might be less than actual area with ground points
  veg_plot_area <- area(plotWilllas)
  
  #Second FSD metrics function
  #if there are no points b/c all veg is < 0.5 m keep loop running
  if(veg_plot_area > 0) { #if veg > 0.5 v
    #run the FSD metric function
    FSD.i <- structural_diversity_metrics(plotWilllas)
    
  } else { #if no veg points < 0.5 m
    FSD.i <- data.frame(matrix(rep(0, 14), ncol = 14))
    colnames(FSD.i) <- c("mean.max.canopy.ht",
                         "max.canopy.ht", 
                         "top.rugosity",
                         "vert.sd","sd.sd", "GFP",
                         "VAI", "VCI", "vertCV", 
                         "q0", "q25", "q50", "q75", "q100")
  }
  
  #------------------------------------------------------------------
  #Metrics from leafR
  #reads in las files different than lidR
  normlas.file <- paste0("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/Will/", las.names[i])
  
  #lad profile doesn't use points below 1 m
  #k = 1 effective LAI
  #grain.size = 3 m horizontal grid resolution
  VOXELS_LAD <- lad.voxels(normlas.file, grain.size = 3, k = 1) 
  
  lad_profile <- lad.profile(VOXELS_LAD, relative = T) #uses relative total LAI
  fhd <- FHD(lad_profile, evenness = FALSE, LAD.threshold = -1) #default
  
  lad_profile_lai <- lad.profile(VOXELS_LAD, relative = F) #doesn't use relative LAI
  LAI <- lai(lad_profile_lai, min = 1, max = 75)
  LAI_subcanopy <- lai(lad_profile_lai, min = 1, max = 5)
  
  gini <- GC(normlas.file, threshold = .5)
  
  #######dennis added_12/15/2021
  # plot.volume <- voxel_volumes(plotWilllas, res= c(5,1))
  #-------------------------------------------------------------------
  out.i <- cbind.data.frame(las.names[i], plot_area_ground, 
                            den, maxZ, chm_metrics_i, FSD.i, 
                            fhd, gini, LAI, LAI_subcanopy)
  OUT <- rbind(OUT, out.i)}, 
  error = function(e) {skip_to_next <<- TRUE})


OUT
colnames(OUT)[1] <- "filename"
FSD <- OUT

#save the results
write.csv(FSD, file = "D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/Will/WillMetrics.csv")

rm(list=ls())

#Combine and join data------------

Cook <- read_csv(paste0(path, "Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/Cook/CookMetrics.csv"))
DuPage <- read_csv(paste0(path, "Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/DuPage/DuPageMetrics.csv"))
Kane <- read_csv(paste0(path, "Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/Kane/KaneMetrics.csv"))
Kendall <- read_csv(paste0(path, "Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/KendallMetrics.csv"))
Lake <- read_csv(paste0(path, "Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/Lake/LakeMetrics.csv"))
McHenry <- read_csv(paste0(path, "Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/McHenry/McHenryMetrics.csv"))
Will <- read_csv(paste0(path, "Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LiDARProcessing/OutFolder/Will/WillMetrics.csv"))

data <- read_csv(paste0(path, "Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/allPlotTreeBA.csv"))

combine <- rbind(Cook, DuPage, Kane, Kendall, Lake, McHenry, Will) %>% 
  dplyr::rename(PlotID = `filename`) %>% 
  separate(`PlotID`, c('PlotID'), extra = 'drop') %>% 
  mutate(PlotID = as.numeric(PlotID)) %>% 
  left_join(., data, by = 'PlotID') %>% 
  mutate(typeCoreEdge = paste0(patchType, edgeCore)) %>% 
  filter(PlotID != 5173, #Plots with no trees
         PlotID != 10010,
         PlotID != 10020,
         PlotID != 4006,
         PlotID != 3056) 

lid <- read_csv('C:/Users/ledarlin/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject/LargerLandsatJoined.csv') %>% 
  left_join(combine, ., by = 'PlotID')

#Look across all edges. Rescaling to no units so that facet_wrap works.
#This is not ideal for final analysis.
edge <- combine %>% filter(edgeCore == 'Edge') %>% 
  mutate(DeepGapFraction = rescale(deepgap.fraction, to = c(-1, 1)),
         Density = rescale(den, to = c(-1, 1)),
         GFP = rescale(GFP, to = c(-1, 1)),
         Gini = rescale(gini, to = c(-1, 1)),
         Rumple = rescale(rumple, to = c(-1, 1)),
         sd.sd = rescale(sd.sd, to = c(-1, 1)),
         TopRugosity= rescale(top.rugosity, to = c(-1, 1)),
         vert.sd = rescale(vert.sd, to = c(-1, 1)),
         vertCV = rescale(vertCV, to = c(-1, 1)),
         VAI = rescale(VAI, to = c(-1, 1)),
         mean.max.canopy.ht = rescale(mean.max.canopy.ht, to = c(-1, 1)),
         q100 = rescale(q100, to = c(-1, 1)),
         q25 = rescale(q25, to = c(-1, 1)),
         q50 = rescale(q50, to = c(-1, 1)),
         q75 = rescale(q75, to = c(-1, 1)),
         max.canopy.ht = rescale(max.canopy.ht, to = c(-1, 1))) %>%
  select(c(2, 8, 9, 11:14, 16, 18:21, 142:146, 26:27)) %>% 
  pivot_longer(cols = c(2:17),
               names_to = 'Metric',
               values_to = 'Value') 

ggplot(edge, aes(x = patchType, y = Value, fill = patchType))+
  geom_violin(trim = TRUE) +
  #stat_summary(fun.data = "mean_cl_boot", geom = "crossbar", color="#550135",
  #             fill = '#FDF7F1', width = 0.2) +
  scale_fill_manual(values=c("Remnant" = "#b2a594", 
                             "Regrowth" = "#d56639", "Novel" = '#59B6BE')) +
  labs(title = 'LiDAR metrics in edge plots') +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) +
  facet_wrap(~Metric, ncol = 4) +
  theme(legend.position = 'blank')

#Select fewer metrics

edgeSelect <- lid %>% filter(edgeCore == 'Edge',
                             ht<25) %>% 
  mutate(`Point density (1/m)` = den.x,
         `Exterior complexity (m)` = top.rugosity* .3048*.3048,
         `Interior complexity (m)` = vert.sd* .3048*.3048,
         `Mean canopy height (m)` = ht) %>%
  select(c(`Point density (1/m)`, `Exterior complexity (m)`, `Interior complexity (m)`, `Mean canopy height (m)`, patchType)) %>% 
  pivot_longer(cols = c(1:4),
               names_to = 'Metric',
               values_to = 'Value') 

edgeFig <- ggplot(edgeSelect, aes(x = patchType, y = Value, fill = patchType))+
  geom_violin(trim = TRUE) +
  geom_boxplot(fill = 'white', width = .2) +
  scale_fill_manual(values=c("Remnant" = "#b2a594", 
                             "Regrowth" = "#d56639", "Novel" = '#59b6be')) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(size = 28),
        axis.text.x = element_blank()) +
  facet_wrap(~Metric, ncol = 2, scales = 'free') +
  theme(legend.position = 'blank')

edgeFig

ggsave(paste0(path, 'Dropbox/LindsayWorking/GradSchool/Dissertation/Figures/edgeFigPaper.png'),
       edgeFig, width = 9.75, height = 7, unit = "in", dpi = 300)

edgeWide <- lid %>% filter(edgeCore == 'Edge')

lm <- lm(mean.max.canopy.ht~patchType, data = edgeWide, family = 'poisson') #
anova(lm)
lm <- lm(vert.sd~patchType, data = edgeWide) # ***
anova(lm)
lm <- lm(top.rugosity~patchType, data = edgeWide) # **
anova(lm)
lm <- lm(den.x~patchType, data = edgeWide) # ns
anova(lm)

core <- combine %>% filter(edgeCore == 'Core') %>% 
  filter(PlotID != '10006') %>% 
  mutate(DeepGapFraction = rescale(deepgap.fraction, to = c(-1, 1)),
         Density = rescale(den, to = c(-1, 1)),
         GFP = rescale(GFP, to = c(-1, 1)),
         Gini = rescale(gini, to = c(-1, 1)),
         Rumple = rescale(rumple, to = c(-1, 1)),
         sd.sd = rescale(sd.sd, to = c(-1, 1)),
         TopRugosity= rescale(top.rugosity, to = c(-1, 1)),
         vert.sd = rescale(vert.sd, to = c(-1, 1)),
         vertCV = rescale(vertCV, to = c(-1, 1)),
         VAI = rescale(VAI, to = c(-1, 1)),
         mean.max.canopy.ht = rescale(mean.max.canopy.ht, to = c(-1, 1)),
         q100 = rescale(q100, to = c(-1, 1)),
         q25 = rescale(q25, to = c(-1, 1)),
         q50 = rescale(q50, to = c(-1, 1)),
         q75 = rescale(q75, to = c(-1, 1)),
         max.canopy.ht = rescale(max.canopy.ht, to = c(-1, 1))) %>%
  select(c(2, 8, 9, 11:14, 16, 18:21, 142:146, 26:27)) %>%
  pivot_longer(cols = c(2:17),
               names_to = 'Metric',
               values_to = 'Value') 

ggplot(core, aes(x = patchType, y = Value, fill = patchType))+
  #geom_boxplot() +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.2, fill = 'white') +
  #stat_summary(fun.data = "mean_cl_boot", geom = "crossbar", color="#550135",
  #             fill = '#FDF7F1', width = 0.2) +
  scale_fill_manual(values=c("Remnant" = "#b2a594", 
                             "Regrowth" = "#d56639", "Novel" = '#59b6be')) +
  labs(title = 'LiDAR metrics in core plots') +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  facet_wrap(~Metric, ncol = 4) +
  theme(legend.position = 'blank')

#Select fewer metrics

coreSelect <- lid %>% filter(edgeCore == 'Core') %>% 
  filter(PlotID != '10006',
         ht < 50) %>% 
  mutate(`Point density (1/m)` = den.x,
         `Exterior complexity (m)` = top.rugosity* .3048*.3048,
         `Interior complexity (m)` = vert.sd* .3048*.3048,
         `Mean canopy height (m)` = ht) %>%
  select(c(`Point density (1/m)`, `Exterior complexity (m)`, `Interior complexity (m)`, `Mean canopy height (m)`, patchType)) %>% 
  pivot_longer(cols = c(1:4),
               names_to = 'Metric',
               values_to = 'Value') 

coreFig <- ggplot(coreSelect, aes(x = patchType, y = Value, fill = patchType))+
  geom_violin(trim = TRUE) +
  geom_boxplot(fill = 'white', width = .2) +
  scale_fill_manual(values=c("Remnant" = "#b2a594", 
                             "Regrowth" = "#d56639", "Novel" = '#59b6be')) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        text = element_text(size = 28)) + 
  facet_wrap(~Metric, ncol = 2, scales = 'free') +
  theme(legend.position = 'blank')

coreFig

ggsave(paste0(path, 'Dropbox/LindsayWorking/GradSchool/Dissertation/Figures/coreFigPaper.png'),
       coreFig, width = 9.75, height = 7, unit = "in", dpi = 300)

coreWide <- lid %>% filter(edgeCore == 'Core')

f <- coreWide %>% filter(patchType == 'Regrowth')
lm <- lm(vert.sd~patchType, data = coreWide) #***
lm <- lm(ht~patchType, data = coreWide) # **
lm <- lm(top.rugosity~patchType, data = coreWide) # ***
lm <- lm(den.x~patchType, data = coreWide) # *

anova(lm)

