#Look at i-Tree comp data
#Lindsay Darling
#5/10/2021

#Load libraries and data------------------

library(plyr)
library(tidyverse)
library(tidylog)          #More verbose tidyverse
library(magrittr)         #Piping %>% 
library(ggplot2)          #Pretty!
library(sf)               #Spatial data
library(janitor)
library(santoku)          #Good for cutting data up

setwd("D:/Dropbox/Forest Composition/composition/Maps/shapefiles/PatchProject")

#Load plot types

novel<-st_read('NovelPlot.shp') #9
regrowth<-st_read('RegrowthPlot.shp') #10
remnant<-st_read('RemPlots.shp') #26

#Load tree data

trees <- read_csv('RegionAllTrees2020.csv') %>% #Throws a warning but looks okay
  rename('PlotID' = 'Plot ID',
         'Species' = 'Species Name',
         'Native' = 'Native to State',
         'TreeID' = 'Tree ID') %>% 
  mutate(dbh_cm = DBH_in*2.54)

#Look at each group of trees

#Novel
novelTree<-left_join(novel,trees, by = 'PlotID') %>% 
  mutate(patchType = 'Novel')

#Remnant
remnantTree<-left_join(remnant,trees, by = 'PlotID')%>% 
  mutate(patchType = 'Remnant')

#Regrowth
regrowthTree<-left_join(regrowth,trees, by = 'PlotID')%>% 
  mutate(patchType = 'Regrowth')

#Merge them together

merged<-rbind(remnantTree, regrowthTree, novelTree) 

#Drop extra columns
df<-merged[,-c(1:4,6:29,31:41)] 

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
#TODO: this is not working.

df %>% 
  mutate(ba_m = (dbh_cm^2)*0.00007854) %>% #Add BA
  st_drop_geometry(.) %>% #pivot_wider doesn't like spatial data
  select(PlotID, patchType, Species, ba_m) %>% #Only the necessary columns
  pivot_wider(id_cols = c(PlotID, patchType), #Summarize by PlotID and keep patch type
              names_from = Species, #Makes species columns
              names_sort = TRUE, #Sort alphabetically
              values_from = ba_m, #Cells are BA
              values_fn = sum, #Add BA together
              values_fill = 0) -> plotSpeciesBA #Fill NA with 0 and write it

View(plotSpeciesBA) #Looks great!

write_csv(plotSpeciesBA, 'coreSpeciesBA.csv')

#Do the same thing for plots that are on the edge

#Load plot types

novelEdge<-st_read('NovelEdgePlot.shp') #9
regrowthEdge<-st_read('RegrowthEdgePlot.shp') #10
remnantEdge<-st_read('RemnantEdgePlot.shp') #26

#Look at each group of trees

#Novel
novelEdgeTree<-left_join(novelEdge,trees, by = 'PlotID') %>% 
  mutate(patchType = 'Novel')

#Remnant
remnantEdgeTree<-left_join(remnantEdge,trees, by = 'PlotID')%>% 
  mutate(patchType = 'Remnant')

#Regrowth
regrowthEdgeTree<-left_join(regrowthEdge,trees, by = 'PlotID')%>% 
  mutate(patchType = 'Regrowth')

#Merge them together

merged<-rbind(remnantEdgeTree, regrowthEdgeTree, novelEdgeTree) 

df<-merged[,-c(1,3:5,7:17)] 

summary(df)

df %>% 
  mutate(ba_m = (dbh_cm^2)*0.00007854) %>% #Add BA
  st_drop_geometry(.) %>% #pivot_wider doesn't like spatial data
  select(PlotID, patchType, Species, ba_m) %>% #Only the necessary columns
  pivot_wider(id_cols = c(PlotID, patchType), #Summarize by PlotID and keep patch type
              names_from = Species, #Makes species columns
              names_sort = TRUE, #Sort alphabetically
              values_from = ba_m, #Cells are BA
              values_fn = sum, #Add BA together
              values_fill = 0) -> edgeSpeciesBA #Fill NA with 0 and write it

View(edgeSpeciesBA)

write_csv(edgeSpeciesBA, 'edgeSpeciesBA.csv')
