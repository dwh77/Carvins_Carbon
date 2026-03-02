#### Install needed packages 

install.packages('tidyverse') #this also brings in dplyr and lubridate
install.packages('plyr') #rbind.fill
install.packages('ggpmisc') #stat_poly_line
install.packages('dataRetrieval') #read in USGS data
install.packages('mgcv') # for USGS to stage GAMs

install.packages('segmented') #for breakpoints

install.packages('rLakeAnalyzer') #water.density

#kruskal wallis boxplot function
install.packages('FSA')
install.packages('rcompanion')
install.packages('ggpubr') # for ggdensity
install.packages('rlang')

#arranging figures
install.packages('gridExtra')
install.packages('cowplot')
install.packages('patchwork')



#from heatmaps
# pacman::p_load(tidyverse, lubridate, akima, reshape2, 
#                gridExtra, grid, colorRamps, RColorBrewer, cowplot)
#for thermocline calcs; rLakeAnalyzer