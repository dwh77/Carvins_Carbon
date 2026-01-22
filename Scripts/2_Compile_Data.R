#### Compile needed datasets for analysis 


## Load packages
library(tidyverse)




#### Read in data sets for discrete chemistry grab samples ####

## EEMs
eems_EDI <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/841/2/45207449d82365ab02caa4f7f3b05cc1" )

## Chemistry
chem_EDI <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/199/13/3f09a3d23b7b5dd32ed7d28e9bc1b081")


## Isotopes
iso_EDI <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/1745/1/71e54498052b76e103898e0b07426a7c")



#### Sensor data sets ####

ctd_EDI <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/200/16/d8de9befde67007072850897d5dd2e06")
  
ysi_EDI <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/198/14/514d382eccf21428985abbd40e1d7d50")




#### CCR Dam water level ####

ccr_catwalk_EDI <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/1069/4/42e6d8bb3d379d40a4a4fb566d4ff36e")

ccr_met_EDI <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/1105/4/8ebf27393ccafe518328468a260d2e18")

hpb_stage_STAGED <- read_csv()



#### CCR Flowmate ####

flowmate_edi <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/454/9/0e7fe16623a1ad2a67774c23ce8a29d8")












