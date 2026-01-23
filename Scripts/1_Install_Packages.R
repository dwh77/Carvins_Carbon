#### Install needed packages 

install.packages('tidyverse')
install.packages('plyr') #rbind.fill
install.packages('dataRetrieval') #read in USGS data
install.packages('ggpmisc') #stat_poly_line
install.packages('mgcv') # for USGS to stage GAMs

library(segmented) #for breakpoints

#kruskal wallis boxplot function
library(FSA)
library(rcompanion)
library(ggpubr)
library(rlang)

#arranging figures
library(gridExtra)
library(cowplot)
library(patchwork)



#from heatmaps
# pacman::p_load(tidyverse, lubridate, akima, reshape2, 
#                gridExtra, grid, colorRamps, RColorBrewer, cowplot)
#for thermocline calcs; rLakeAnalyzer