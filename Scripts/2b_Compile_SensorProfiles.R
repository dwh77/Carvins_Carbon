#### Sensors data compilation

##packages
library(tidyverse)

#Define sampling dates for filtering
sampling_dates <- c(ymd("2024-05-22"), ymd("2024-06-19"), ymd("2024-07-11"), ymd("2024-08-14"), ymd("2024-09-16"),
                    ymd("2024-09-30"), ymd("2024-10-28"), ymd("2024-12-17"), ymd("2025-02-26"), ymd("2025-04-16"))


#### Read in and filter YSI data ####

ysi_EDI <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/198/14/514d382eccf21428985abbd40e1d7d50")

#filter to CCR and dates
ccr_ysi <- ysi_EDI |> 
  mutate(Date = as.Date(DateTime)) |> 
  filter(Reservoir == "CCR",
         Site %in% c(92,96,98,100,101), 
         Date %in% sampling_dates #get just sampling dates
  ) |> 
  select(Reservoir, Site, Date, Depth_m, Temp_C, DO_mgL, DOsat_percent, SpCond_uScm)

#plot to check right sites and dates are filtered out
ccr_ysi |> 
  ggplot(aes(x = Site, y = Temp_C, color = Depth_m))+
  geom_point()+
  facet_wrap(~Date)

  

#### Read in and filter CTD data ####
ctd_EDI <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/200/16/d8de9befde67007072850897d5dd2e06")

### CTD clean function
###function to clean ctd casts
ccr_ctd_clean <- function(data){
  
  #slice by depth for each reservoir
  depths = seq(0.1, 21, by = 0.1)
  df.final<-data.frame()
  
  for (i in 1:length(depths)){
    
    fp_layer <- data %>% 
      mutate(Date = as.Date(DateTime)) |> 
      group_by(Reservoir, Date, Site) |>  
      slice(which.min(abs(as.numeric(Depth_m) - depths[i])))
    
    # Bind each of the data layers together.
    df.final = bind_rows(df.final, fp_layer)%>%
      mutate(Depth_m = round(Depth_m, digits = 1)) |> 
      dplyr::distinct() 
  }
  return(df.final)
}

## Clean up CCR CTD data
ctd_EDI_ccr_trim <- ctd_EDI |> 
  mutate(Date = as.Date(DateTime)) |> 
  select(Reservoir, Site, Date, everything()) |> 
  filter(Reservoir == "CCR") |> 
  filter(Date %in% sampling_dates) |> 
  filter(SpCond_uScm < 150) #reomvoe values where sensors was in sediments


ccr_ctd <- ccr_ctd_clean(ctd_EDI_ccr_trim)


#plot to check filtering to right dates and site 
ccr_ctd |> 
  ggplot(aes(x = Temp_C, y = Depth_m))+
  geom_line(orientation = "y")+
  geom_point()+
  scale_y_reverse()+
  facet_grid(Site~Date, scales = "free_y")+
  theme_bw()

ccr_ctd |> 
  ggplot(aes(x = SpCond_uScm, y = Depth_m))+
  geom_line(orientation = "y")+
  geom_point()+
  scale_y_reverse()+
  facet_grid(Site~Date, scales = "free_y")+
  theme_bw()


#### bind YSI to CTD and write csv ####

#check no site overlap
unique(ccr_ctd$Site)
unique(ccr_ysi$Site)

sensors <- plyr::rbind.fill(ccr_ysi, ccr_ctd) |> 
  select(1:8)


# write.csv(sensors, "./Data/sensors_joined.csv", row.names = F)


