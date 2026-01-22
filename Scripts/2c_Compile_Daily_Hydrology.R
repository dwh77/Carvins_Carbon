#### Compile daily hydrology data sets 


####packages 
library(tidyverse)


########  CCR Dam water level ####
ccr_catwalk_EDI <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/1069/4/42e6d8bb3d379d40a4a4fb566d4ff36e")


#check out water level options 
ccr_catwalk_EDI |> 
  filter(as.Date(DateTime) > ymd("2024-05-01")) |> 
  select(DateTime, LvlDepth_m_13, Modeled_Depth_m) |> 
  pivot_longer(-1) |> 
  ggplot(aes(x = DateTime, y = value, color = name))+
  geom_point()


#Calculate daily mean water level
dam_daily_waterlevel <- ccr_catwalk_EDI |> 
  select(DateTime, Modeled_Depth_m) |> 
  mutate(Date = as.Date(DateTime)) |> 
  group_by(Date) |> 
  summarise(Dam_WaterLevel_m_daily = mean(Modeled_Depth_m, na.rm = T))

#look at data
dam_daily_waterlevel |> 
  ggplot(aes(x = Date, y = Dam_WaterLevel_m_daily))+geom_point()


########  CCR dam daily rain ####
ccr_met_EDI <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/1105/4/8ebf27393ccafe518328468a260d2e18")

#Get daily summed rain
daily_rain <- ccr_met_EDI |> 
  select(DateTime, Rain_Total_mm) |> 
  mutate(Date = as.Date(DateTime)) |> 
  group_by(Date) |> 
  summarise(Daily_rain_mm = sum(Rain_Total_mm, na.rm = T))

#look at data
daily_rain |> ggplot(aes(x= Date, y = Daily_rain_mm))+geom_point()



########  HPB daily stage ####
hpb_stage_STAGED <- read_csv("https://pasta-s.lternet.edu/package/data/eml/edi/1781/1/630f42ffb3560c3a6afd592511756c1e")

#get daily mean stage
daily_stage <- hpb_stage_STAGED |> 
  mutate(Date = as.Date(DateTime)) |> 
  group_by(Date) |> 
  summarise(Daily_Stage_cm = mean(Stage_cm, na.rm = T))

#look at data
daily_stage |> ggplot(aes(x= Date, y = Daily_Stage_cm))+geom_point()


########daily USGS stage and Q data from nearby stations ####
library(dataRetrieval)

#define USGS site numbers for sites near CCR
sitenos <- c("02055100" , "02018500")

#get site info
siteinfo <- readNWISsite(sitenos)
data_available <- whatNWISdata(siteNumber = sitenos, service = "dv") #can remove dv to get other variables

#set parameters to read in data
startDate <- "2021-03-29" # "1987-01-01" to line up w/ WVWA records; "1940-01-01" gets all possible data from the sites
endDate <- "2025-12-31" # OR could use Sys.Date() #to today
parameter <- "00060" # 00060 is Q, works for daily (readNWISdv) and 15min (readNWISuv) USGS parameter 'Discharge, cubic feet per second'
stage <- "00065" # 00065 is USGS parameter code for 'Gage height, feet' is continous data

#Get Daily Q data
Qdat <- readNWISdv(sitenos, parameter, startDate, endDate) %>% 
  renameNWISColumns()

Qdata <- left_join(Qdat, siteinfo, by = c("site_no"))

Qdata_forjoin <- Qdata |> 
  select(Date, site_no, Flow) |> 
  pivot_wider(names_from = site_no, values_from = Flow, names_prefix = "Flow_cfs_")

## 15 min Q
# Q_15min_dat <- readNWISuv(sitenos, parameter, startDate, endDate) %>% 
#   renameNWISColumns()

## Get 15 min stage data
stage_15min_dat <- readNWISuv(sitenos, stage, startDate, endDate) %>% 
  renameNWISColumns()

#look at 15 min stage data
stage_15min_dat |> 
  ggplot(aes(x = dateTime, y = GH_Inst, col = site_no))+
  geom_point()+
  labs(title = "USGS gauges height: 15 min")

## Get daily stage data
stagedata_forjoin <- stage_15min_dat |> 
  mutate(Date = as.Date(dateTime)) |> 
  group_by(Date, site_no) |> 
  summarise(Stage = mean(GH_Inst, na.rm = T)) |> 
  pivot_wider(names_from = site_no, values_from = Stage, names_prefix = "Stage_ft_")







#### Bind daily data sets together ####
#look at dfs to bind
head(daily_rain)
head(dam_daily_waterlevel)
head(daily_stage)
head(Qdata_forjoin)
head(stagedata_forjoin)

#bind data and clean up names
Daily_Hydro_data <- left_join(daily_rain, dam_daily_waterlevel, by = "Date") |> 
  left_join(daily_stage, by = "Date") |> 
  left_join(Qdata_forjoin, by = "Date") |> 
  left_join(stagedata_forjoin, by = "Date") |> 
  rename(HPB_daily_Stage_cm = Daily_Stage_cm,
         Dam_daily_WaterLevel_m = Dam_WaterLevel_m_daily,
         Flow_Catawba_cfs = Flow_cfs_02018500,
         Flow_Tinker_cfs = Flow_cfs_02055100,
         Stage_Catawba_ft = Stage_ft_02018500,
         Stage_Tinker_ft = Stage_ft_02055100
         )


###write csv
# write.csv(Daily_Hydro_data, "./Data/Hydrology_Daily_Observed.csv", row.names = F)




