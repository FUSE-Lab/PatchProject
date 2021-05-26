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

df<-merged[,-c(1:4,6:25,27:28,31)] 
colnames(df)
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

df<-merged[,-c(1,7:10,27:28,31)] 

summary(df)


summary(merged)

#Let's play ----------------

#This binning script was written by Dexter Locke for the SESYNC urban
#woodlands pursuit. 

#create a column with dbh classes

plots_trees <-  df %>% mutate(dbh_class = santoku::chop(dbh_cm,  
                                                breaks = seq(from = 2.54, #min DBH in dataset
                                                to = 136, #Max DBH in dataset
                                                by = 10)),         # 5 cm intervals
         dbh_class = factor(dbh_class, ordered = TRUE), #Put them in order
         ba_m = (dbh_cm^2)*0.00007854) #Add BA

# how did the dbh class work out?
table(plots_trees$dbh_class)
round(100*prop.table(table(plots_trees$dbh_class)), 2)
barplot(table(plots_trees$dbh_class), las=2) #DBH distribution by class. Boom.

View(plots_trees)


binned<-plots_trees %>% 
  mutate(dbh_class = factor(dbh_class, ordered = TRUE,
                            levels = c(
                              '[2.54, 12.54)',
                              '[12.54, 22.54)',
                              '[22.54, 32.54)',
                              '[32.54, 42.54)',
                              '[42.54, 52.54)',
                              '[52.54, 62.54)',
                              '[62.54, 72.54)',
                              '[72.54, 82.54)', 
                              '[82.54, 92.54)',
                              '[92.54, 102.5)', 
                              '[102.54, 112.5)', 
                              '[112.54, 122.5)', 
                              '[122.54, 132.5)',
                              '[132.54, 134.1'))) %>% 
  drop_na(dbh_class) 

View(binned)

binned %>% 
  select(PlotID,genus_species, trees_per_hectare) %>% 
  pivot_wider(id_cols = c(data_source_plot_ID, year),
              names_from = genus_species,
              names_sort = TRUE,
              values_from = trees_per_hectare,
              values_fill = 0) %>% 
  mutate(data_source_plot_ID_year = paste(data_source_plot_ID, year, sep = '_')) %>% 
  select(data_source_plot_ID_year, everything(), -data_source_plot_ID, -year) -> trees_stem_density_ha_species_matrix

st_write(binned, 'PlotTree.csv')
