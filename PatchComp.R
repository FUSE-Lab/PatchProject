#Look at i-Tree comp data
#Lindsay Darling
#5/10/2021

#Load libraries and data------------------

library(plyr)
library(tidyverse)
library(magrittr)
library(tidylog)          #More verbose tidyverse
library(sf)               #Spatial data
#library(janitor)
#library(santoku)          #Good for cutting data up

setwd("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject")

#Load core plots from 2010 -----------------

novelCore<-st_read('novelCorePlot10.shp') #9 plots
regrowthCore<-st_read('regrowthCorePlot10.shp') #13 plots
remnantCore<-st_read('remCorePlot10.shp') #31 plots

#Load tree data

trees10 <- read_csv('iTree2010Trees.csv')%>% 
  mutate(dbh_cm = DBH_IN*2.54) #Change to cm

#Look at each group of trees

#Novel
novelCoreTree10<-left_join(novelCore,trees10, by = 'PlotID') %>% 
  mutate(patchType = 'Novel') %>% #Add column with patch type
  select(PlotID, TreeID, Genus, Species, GenusSpecies, dbh_cm, patchType) #441 trees

#Remnant
remnantCoreTree10<-left_join(remnantCore,trees10, by = 'PlotID')%>% 
  mutate(patchType = 'Remnant') %>% #Add column with patch type
  select(PlotID, TreeID, Genus, Species, GenusSpecies, dbh_cm, patchType) #1325 trees

#Regrowth
regrowthCoreTree10<-left_join(regrowthCore,trees10, by = 'PlotID')%>% 
  mutate(patchType = 'Regrowth') %>% #Add column with patch type
  select(PlotID, TreeID, Genus, Species, GenusSpecies, dbh_cm, patchType) #775 trees

#Merge them together

mergedCore10<-rbind(remnantCoreTree10, regrowthCoreTree10, novelCoreTree10) 

#write
write_csv(mergedCore10,'corePlotSpecies10.csv')

#Not using this binning code for now, but may need later.

# #Create DBH bins ----------------
# #This binning script was written by Dexter Locke for the SESYNC urban
# #woodlands pursuit. 
# 
# #create a column with dbh classes
# 
# plots_trees <-  df %>% mutate(dbh_class = santoku::chop(dbh_cm,  
#                                                         breaks = seq(from = 2.54, #min DBH in dataset
#                                                                      to = 136, #Max DBH in dataset
#                                                                      by = 10)),         # 10 cm intervals
#                               dbh_class = factor(dbh_class, ordered = TRUE), #Put them in order
#                               ba_m = (dbh_cm^2)*0.00007854) #Add BA
# View(plots_trees)
# # how did the dbh class work out?
# table(plots_trees$dbh_class)
# barplot(table(plots_trees$dbh_class), las=2) #DBH distribution by class. Boom.
# 
# #Now, create a table with 
# 
# binned<-plots_trees %>% 
#   mutate(dbh_class = factor(dbh_class, ordered = TRUE,
#                             levels = c(
#                               '[2.54, 12.54)',
#                               '[12.54, 22.54)',
#                               '[22.54, 32.54)',
#                               '[32.54, 42.54)',
#                               '[42.54, 52.54)',
#                               '[52.54, 62.54)',
#                               '[62.54, 72.54)',
#                               '[72.54, 82.54)', 
#                               '[82.54, 92.54)',
#                               '[92.54, 102.5)', 
#                               '[102.54, 112.5)', 
#                               '[112.54, 122.5)', 
#                               '[122.54, 132.5)',
#                               '[132.54, 134.1'))) %>% 
#   drop_na(dbh_class) 
# 
# View(binned)

#Create a table with plots as rows, species as columns, and sum of BA in cells

mergedCore10 %>% 
  mutate(ba_m = (dbh_cm^2)*0.00007854) %>% #Add BA
  st_drop_geometry(.) %>% #pivot_wider doesn't like spatial data
  select(PlotID, patchType, GenusSpecies, ba_m) %>% #Only the necessary columns
  pivot_wider(id_cols = c(PlotID, patchType), #Summarize by PlotID and keep patch type
              names_from = GenusSpecies, #Makes species columns
              names_sort = TRUE, #Sort alphabetically
              values_from = ba_m, #Cells are BA
              values_fn = sum, #Add BA together
              values_fill = 0) %>% #Fill NA with 0 
  #There are some plots with no trees. Find them by adding all of the BA and
  #filtering for plots with sum BA of 0
  mutate(sumBA = rowSums(dplyr::across(where(is.numeric)))) %>% #Add column with total BA
  filter(sumBA > 0) %>% #Filter rows with no trees
  select(-sumBA)-> coreSpeciesBA10 #Remove sumBA column and save out results

#View(coreSpeciesBA10) #Looks great!

write_csv(coreSpeciesBA10, 'coreSpeciesBA10.csv')

#Edge 2010-----------------

#Load plot types

novelEdge10<-st_read('novelEdgePlot10.shp') #18 plots
regrowthEdge10<-st_read('regrowthEdgePlot10.shp') #16 plots
remnantEdge10<-st_read('remEdgePlot10.shp') #18 plots

#Look at each group of trees

#Novel
novelEdgeTree10<-left_join(novelEdge10,trees10, by = 'PlotID') %>% 
  mutate(patchType = 'Novel') %>% 
  select(PlotID, TreeID, Genus, Species, GenusSpecies, dbh_cm, patchType) #951 trees

#Remnant
remnantEdgeTree10<-left_join(remnantEdge10,trees10, by = 'PlotID')%>% 
  mutate(patchType = 'Remnant') %>% 
  select(PlotID, TreeID, Genus, Species, GenusSpecies, dbh_cm, patchType) #734 trees

#Regrowth
regrowthEdgeTree10<-left_join(regrowthEdge10,trees10, by = 'PlotID')%>% 
  mutate(patchType = 'Regrowth')%>% 
  select(PlotID, TreeID, Genus, Species, GenusSpecies, dbh_cm, patchType) # 642 trees

#Merge them together

mergedEdge10<-rbind(remnantEdgeTree10, regrowthEdgeTree10, novelEdgeTree10) 

#Write results

write_csv(mergedEdge10,'edgePlotSpecies10.csv')

#Now calculate BA of each species

mergedEdge10 %>% 
  mutate(ba_m = (dbh_cm^2)*0.00007854) %>% #Add BA
  st_drop_geometry(.) %>% #pivot_wider doesn't like spatial data
  select(PlotID, patchType, GenusSpecies, ba_m) %>% #Only the necessary columns
  pivot_wider(id_cols = c(PlotID, patchType), #Summarize by PlotID and keep patch type
              names_from = GenusSpecies, #Makes species columns
              names_sort = TRUE, #Sort alphabetically
              values_from = ba_m, #Cells are BA
              values_fn = sum, #Add BA together
              values_fill = 0) %>%  #Fill NA with 0
  #There are some plots with no trees. Find them by adding all of the BA and
  #filtering for plots with sum BA of 0
  mutate(sumBA = rowSums(across(where(is.numeric)))) %>% 
  filter(sumBA > 0) %>% 
  select(-sumBA)-> edgeSpeciesBA10 #Remove sumBA and write


#View(edgeSpeciesBA10)

write_csv(edgeSpeciesBA10, 'edgeSpeciesBA10.csv')

#Do it again with 2020 data---------------------

#Species list

species <- read_csv('itree_species_list.csv')

trees20 <- read_csv('AllTrees2020.csv') %>% 
  dplyr::rename('PlotID' = 'Plot ID',
         'Native' = 'Native to State') %>% 
  mutate(TreeID = paste(`PlotID`, `Tree ID`, sep = '')) %>% 
  mutate(dbh_cm = `DBH (in)`*2.54) %>% 
  left_join(., species, by = 'Common Name' )

novelCore20<-st_read('novelCorePlot20.shp') 
regrowthCore20<-st_read('regrowthCorePlot20.shp') 
remnantCore20<-st_read('remCorePlot20.shp') 

#Look at each group of trees

#Novel
novelCoreTree20<-left_join(novelCore20, trees20, by = 'PlotID') %>% 
  mutate(patchType = 'Novel') %>% 
  dplyr::select(PlotID, TreeID, Genus, Species, GenusSpecies, dbh_cm, patchType) #502

#Regrowth
regrowthCoreTree20<-left_join(regrowthCore20, trees20, by = 'PlotID')%>% 
  mutate(patchType = 'Regrowth') %>% 
  dplyr::select(PlotID, TreeID, Genus, Species, GenusSpecies, dbh_cm, patchType) #681

#Remnant
remnantCoreTree20<-left_join(remnantCore20, trees20, by = 'PlotID')%>% 
  mutate(patchType = 'Remnant') %>% 
  dplyr::select(PlotID, TreeID, Genus, Species, GenusSpecies, dbh_cm, patchType) #918


#New samples

sampled <- read.csv('PatchTreeData.csv')
plotData <- read.csv('PlotsToSample.csv')

#Clean up data for join
coreClean20 <- sampled %>% 
  left_join(., plotData, by = 'PlotID') %>% #join plot data
  dplyr::rename(patchType = 'Type') %>% 
  separate(., `Species`, c('Genus', 'Species'), sep = " ") %>% #Make genus and species columns
  unite(GenusSpecies, c('Genus', 'Species'), sep = ' ', remove = FALSE) %>% #Pull back together
  mutate(dbh_cm = DBH_in*2.54) %>% #cm
  filter(CoreEdge == 'Core') %>% #Only want core
  dplyr::select(PlotID, UniqueID, Genus, Species, GenusSpecies, dbh_cm, patchType) %>% 
  dplyr::rename('TreeID' = 'UniqueID')

#Merge them together

mergedCore20<-rbind(remnantCoreTree20, regrowthCoreTree20, novelCoreTree20) %>% 
  st_drop_geometry(.) %>% #Move to df
  rbind(., coreClean20) %>%  #Bring in new data
  mutate(edgeCore = "Core") %>% 
  drop_na(.) #There are some weird added rows with all NA. Destroy them

head(mergedCore20)

write_csv(mergedCore20,'corePlotSpecies20.csv')

#Create table with plots and BA

mergedCore20 %>% 
  mutate(ba_m = (dbh_cm^2)*0.00007854) %>% #Add BA
  dplyr::select(PlotID, patchType, edgeCore, GenusSpecies, ba_m) %>% #Only the necessary columns
  pivot_wider(id_cols = c(PlotID, patchType, edgeCore), #Summarize by PlotID and keep patch type
              names_from = GenusSpecies, #Makes species columns
              names_sort = TRUE, #Sort alphabetically
              values_from = ba_m, #Cells are BA
              values_fn = sum, #Add BA together
              values_fill = 0) %>% #Fill NA with 0 
  #There are some plots with no trees. Find them by adding all of the BA and
  #filtering for plots with sum BA of 0
  dplyr::mutate(sumBA = rowSums(dplyr::across(where(is.numeric)))) %>% 
  filter(sumBA > 0) %>% 
  dplyr::select(-sumBA) -> plotCoreSpeciesBA20 #Remove sumBA

#I looked at imagery from 1970 and 1990 and was able to confirm whether plots
#were remnant, regrowth or novel. These are some manual tweaks to reflect that work.

plotCoreSpeciesBA20[2,2] <- 'Regrowth'

View(plotCoreSpeciesBA20) #Looks great!

write_csv(plotCoreSpeciesBA20, 'coreSpeciesBA20.csv')

#Do the same thing for plots that are on the edge

#Load plot types

novelEdge20<-st_read('novelEdgePlot20.shp') 
regrowthEdge20<-st_read('regrowthEdgePlot20.shp') 
remnantEdge20<-st_read('remEdgePlot20.shp') 

#Look at each group of trees

#Novel
novelEdgeTree20<-left_join(novelEdge20,trees20, by = 'PlotID') %>% #751 trees
  mutate(patchType = 'Novel') %>% 
  dplyr::select(PlotID, TreeID, Genus, Species, GenusSpecies, dbh_cm, patchType)

#Remnant
remnantEdgeTree20<-left_join(remnantEdge20,trees20, by = 'PlotID')%>% #702 trees
  mutate(patchType = 'Remnant') %>% 
  dplyr::select(PlotID, TreeID, Genus, Species, GenusSpecies, dbh_cm, patchType)

#Regrowth
regrowthEdgeTree20<-left_join(regrowthEdge20,trees20, by = 'PlotID')%>% #746 trees
  mutate(patchType = 'Regrowth')%>% 
  dplyr::select(PlotID, TreeID, Genus, Species, GenusSpecies, dbh_cm, patchType)

#New samples

edgeClean20 <- sampled %>% 
  left_join(., plotData, by = 'PlotID') %>% 
  dplyr::rename(patchType = 'Type') %>% 
  separate(., `Species`, c('Genus', 'Species'), sep = " ") %>% 
  unite(GenusSpecies, c('Genus', 'Species'), sep = ' ', remove = FALSE) %>% 
  mutate(dbh_cm = DBH_in*2.54) %>% 
  filter(CoreEdge == 'Edge') %>% #Filter for edge
  dplyr::select(PlotID, UniqueID, Genus, Species, GenusSpecies, dbh_cm, patchType) %>% 
  dplyr::rename('TreeID' = 'UniqueID')


#Merge them together

mergedEdge20<-rbind(remnantEdgeTree20, regrowthEdgeTree20, novelEdgeTree20)%>% 
  st_drop_geometry(.) %>% 
  rbind(., edgeClean20) %>% 
  mutate(edgeCore = 'Edge') %>% 
  drop_na(.)

write_csv(mergedEdge20,'edgePlotSpecies20.csv')

mergedEdge20 %>% 
  mutate(ba_m = (dbh_cm^2)*0.00007854) %>% #Add BA
  dplyr::select(PlotID, patchType, edgeCore, GenusSpecies, ba_m) %>% #Only the necessary columns
  pivot_wider(id_cols = c(PlotID, patchType, edgeCore), #Summarize by PlotID and keep patch type
              names_from = GenusSpecies, #Makes species columns
              names_sort = TRUE, #Sort alphabetically
              values_from = ba_m, #Cells are BA
              values_fn = sum, #Add BA together
              values_fill = 0) %>%  #Fill NA with 0
  #There are some plots with no trees. Find them by adding all of the BA and
  #filtering for plots with sum BA of 0
  dplyr::mutate(sumBA = rowSums(across(where(is.numeric)))) %>% 
  filter(sumBA > 0) %>% 
  dplyr::select(-sumBA)-> edgeSpeciesBA20 #Remove sumBA and write

#Fix identities that were checked with more imagery

edgeSpeciesBA20[c(7,15),2] <- 'Regrowth'

View(edgeSpeciesBA20)

write_csv(edgeSpeciesBA20, 'edgeSpeciesBA20.csv')

#Combine tree data----------

#Bring all of the tree and plot data into one dataframe

novelEdge <- st_read('novelEdgePlot20.shp') %>% 
  dplyr::select(PlotID)%>% #Get rid of extra columns
  mutate(edgeCore = 'Edge') %>% 
  sf::st_transform(., crs = 5070) #Change to albers = conic
novelCore <- st_read('novelCorePlot20.shp') %>% 
  dplyr::select(PlotID)%>%
  mutate(edgeCore = 'Core') %>% 
  sf::st_transform(., crs = 5070)
regrowthEdge <- st_read('regrowthEdgePlot20.shp') %>% 
  dplyr::select(PlotID)%>%
  mutate(edgeCore = 'Edge') %>% 
  sf::st_transform(., crs = 5070)
regrowthCore <- st_read('regrowthCorePlot20.shp') %>% 
  dplyr::select(PlotID)%>% 
  mutate(edgeCore = 'Core') %>%
  sf::st_transform(., crs = 5070)
remEdge <- st_read('remEdgePlot20.shp') %>% 
  dplyr::select(PlotID)%>% 
  mutate(edgeCore = 'Edge') %>% 
  sf::st_transform(., crs = 5070)
remCore <- st_read('RemCorePlot20.shp') %>% 
  dplyr::select(PlotID)%>% 
  mutate(edgeCore = 'Core') %>%
  sf::st_transform(., crs = 5070)
newPlot <- st_read('PlotsToSample.shp') %>% 
  dplyr::rename('PlotID' = 'PLOTID',
                'edgeCore' = 'COREEDGE') %>% 
  dplyr::select(c(PlotID, edgeCore))%>% 
  sf::st_transform(., crs = 5070)

#Merge together
plot <- rbind(novelEdge, novelCore, regrowthEdge, regrowthCore, 
              remEdge, remCore, newPlot)

#Add tree data
core<-read_csv('coreSpeciesBA20.csv')

edge<-read_csv('edgeSpeciesBA20.csv') 

#Use plyr type bind to combine tree data

trees <- plyr::rbind.fill(core, edge) %>% 
  replace(is.na(.), 0) #Replace na with 0

#Join plots and trees and write out file

scanned <- read_csv('plotScanned.csv') %>% 
  rename(PlotID = "PlotID,N,24,15",
         scanned = "scanned,C,80") %>% 
  select(PlotID,scanned,scanComplete)

plotTree <- left_join(plot, trees, by = 'PlotID') %>% 
  distinct(PlotID, .keep_all = TRUE) %>% #There are a few duplicate rows.
  #st_drop_geometry(.) %>% 
  drop_na(.) %>%  #A few of the plots don't have trees
  #Add sum BA column. Adding everything numeric then subtract PlotID
  dplyr::mutate(sumBA = rowSums(across(where(is.numeric)))- `PlotID`) %>% 
  rename(edgeCore = edgeCore.x) %>% 
  select(-edgeCore.y) %>% 
  left_join(., scanned)

st_write(plotTree, 'plotTypeBa.shp', driver = 'ESRI Shapefile', append = FALSE)


 #Calculate trees per plot

edge20 <- read_csv('edgePlotSpecies20.csv') 
core20 <- read_csv('corePlotSpecies20.csv') 

merged20 <- rbind(edge20, core20) %>% 
  dplyr::mutate(typeCoreEdge = paste(`patchType`, `coreEdge`))

stemsPlot <- merged20 %>% group_by(PlotID, coreEdge, typeCoreEdge, patchType) %>% 
  dplyr::summarize(plotStems = n())

#Summary stats-----------------

#Violin plot of BA-------------

ggplot(plotTree, aes(x = typeCoreEdge, y = sumBA, fill = patchType))+
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.2, fill = '#FDF7F1') +
  #stat_summary(fun.data = "mean_cl_boot", geom = "crossbar", color="#550135",
  #             fill = '#FDF7F1', width = 0.2) +
  scale_fill_manual(values=c("Remnant" = "#415c57", 
                             "Regrowth" = "#6baa35", "Novel" = '#fe941c')) +
  labs(title = 'Sum of basal area across patch types', 
       x = 'Patch type', y = 'Sum of basal area (meters squared)') +
  guides(fill = guide_legend(title = "Forest type")) #+
  #theme(legend.background = element_rect(fill = '#FDF7F1')) +
  #theme(plot.background = element_rect(fill = "#FDF7F1"))

#Violin plot number of stems--------------

ggplot(stemsPlot, aes(x = typeCoreEdge, y = plotStems, fill = patchType))+
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.2, fill = '#FDF7F1') +
  #stat_summary(fun.data = "mean_cl_boot", geom = "crossbar", color="#550135",
  #             fill = '#FDF7F1', width = 0.2) +
  scale_fill_manual(values=c("Remnant" = "#415c57", 
                             "Regrowth" = "#6baa35", "Novel" = '#fe941c')) +
  labs(title = 'Number of stems across patch types', 
       x = 'Patch type', y = 'Stem count (n)') +
  guides(fill = guide_legend(title = "Forest type")) #+
  #theme(legend.background = element_rect(fill = '#FDF7F1')) +
  #theme(plot.background = element_rect(fill = "#FDF7F1"))


#Species richness-------------

core20 <- read_csv('corePlotSpecies20.csv') %>% 
  mutate(edgeCore = 'Core')

all20 <- read_csv('edgePlotSpecies20.csv') %>% 
  mutate(edgeCore = 'Edge') %>% 
  rbind(., core20) %>% 
  dplyr::mutate(typeCoreEdge = paste(`patchType`, `edgeCore`))

stemsType <- all20 %>% group_by(typeCoreEdge) %>% 
  dplyr::summarize(count_stems = n())

plotCount <- all20 %>% group_by(typeCoreEdge) %>% 
  summarise(plotCount = n_distinct(PlotID))

speciesType <- all20 %>% group_by(typeCoreEdge) %>% 
  summarise(speciesCount = n_distinct(GenusSpecies)) 

#There is a problem here.

table <- left_join(stemsType, plotCount, by = 'typeCoreEdge') %>% 
  left_join(., speciesType, by = 'typeCoreEdge')
View(table)

#Test if groups have significantly different BA-----------

#First core
core<-plotTree %>% 
  filter(edgeCore == 'Core') #%>% The below script allows for z score format 
#dplyr::select(edgeCore, sumBA, PlotID) %>% 
#pivot_wider(., id_cols = PlotID, names_from = edgeCore, values_from = sumBA)

testCore<-lm(data = core, sumBA ~ patchType)
anova(testCore) #Nope
par(mfcol=c(2,2))
  plot(testCore)

#Edges
edge<-plotTree %>% 
  filter(edgeCore == 'Edge')

testEdge<-lm(data = edge, sumBA ~ patchType)
anova(testEdge) #Nope
plot(testEdge)


#There are no significant difference in any group nor overall in the BA
#in edges and core

#Test if groups have significantly different stem count-----------

core<-stemsPlot %>% 
  filter(edgeCore == 'Core') #%>% The below script allows for z score format 
#dplyr::select(edgeCore, sumBA, PlotID) %>% 
#pivot_wider(., id_cols = PlotID, names_from = edgeCore, values_from = sumBA)

testCore<-lm(data = core, plotStems ~ patchType)
anova(testCore) #!!! Highly significant
plot(testCore) #Looks awesome

#Edges
edge<-stemsPlot %>% 
  filter(edgeCore == 'Edge')

testEdge<-lm(data = edge, plotStems ~ patchType)
anova(testEdge) #Not even a little bit
plot(testEdge)

#Differences in stem core/edge in patch types---------------

remCore <- stemsPlot %>% 
  filter(typeCoreEdge == 'Remnant Core')
remEdge <- stemsPlot %>% 
  filter(typeCoreEdge == 'Remnant Edge')

t.test(remCore$plotStems,remEdge$plotStems) #Not different

novCore <- stemsPlot %>% 
  filter(typeCoreEdge == 'Novel Core')
novEdge <- stemsPlot %>% 
  filter(typeCoreEdge == 'Novel Edge')

t.test(novCore$plotStems,novEdge$plotStems) # slightly different

regCore <- stemsPlot %>% 
  filter(typeCoreEdge == 'Regrowth Core')
regEdge <- stemsPlot %>% 
  filter(typeCoreEdge == 'Regrowth Edge')

t.test(regCore$plotStems,regEdge$plotStems) #Not different

#Differences in BA core/edge in patch types---------------

remCore <- plotTree %>% 
  filter(typeCoreEdge == 'Remnant Core')
remEdge <- plotTree %>% 
  filter(typeCoreEdge == 'Remnant Edge')

t.test(remCore$sumBA,remEdge$sumBA) #Not different

novCore <- plotTree %>% 
  filter(typeCoreEdge == 'Novel Core')
novEdge <- plotTree %>% 
  filter(typeCoreEdge == 'Novel Edge')

t.test(novCore$sumBA,novEdge$sumBA) # Very Not different

regCore <- plotTree %>% 
  filter(typeCoreEdge == 'Regrowth Core')
regEdge <- plotTree %>% 
  filter(typeCoreEdge == 'Regrowth Edge')

t.test(regCore$sumBA,regEdge$sumBA) #Not different


#Species histogram---------------

#Not oak

noOak<- mergedCore20 %>% 
  filter(Genus != 'Quercus') %>% 
  filter(patchType == 'Remnant') %>% 
  mutate(oak = 'N')


#Oaks
Oak<- mergedCore20 %>% 
  filter(Genus == 'Quercus') %>% 
  filter(patchType == 'Remnant') %>% 
  mutate(oak = 'Y')

#Bind together
oakYN<-rbind(noOak, Oak)

mu <- plyr::ddply(oakYN, "oak", summarise, grp.mean=mean(dbh_cm))

ggplot(oakYN, aes(x=dbh_cm, fill=oak)) +
  geom_histogram(aes(color = oak, fill=oak), 
                 alpha=0.6, position = 'identity') +
  scale_color_manual(values = c("#6baa35", "#415c57"),
                     name = 'Distribution of Oak and non Oak DBH',
                     labels = c('Not oak', 
                                'Oak')) +
  scale_fill_manual(values = c("#6baa35", "#415c57"),
                    name = 'Distribution of Oak and non Oak DBH',
                    labels = c('Not oak', 
                               'Oak'))  +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=oak),
             linetype="dashed") +
  xlab('DBH in cm') +
  ylab('Number of individuals') +
  theme(legend.position = c(.8,.8))
