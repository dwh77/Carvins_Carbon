#### Bring sensor and hydro observations to chemistry synoptic sampling


## packages 
library(tidyverse)

#Define sampling dates for filtering
sampling_dates <- c(ymd("2024-05-22"), ymd("2024-06-19"), ymd("2024-07-11"), ymd("2024-08-14"), ymd("2024-09-16"),
                    ymd("2024-09-30"), ymd("2024-10-28"), ymd("2024-12-17"), ymd("2025-02-26"), ymd("2025-04-16"))



## Read in data sets from 2a-2c
chem_df <- read_csv("./Data/chemistry_joined.csv")

hydro_df <- read_csv("./Data/Hydro_daily.csv")

sensor_df <- read_csv("./Data/sensors_joined.csv") |> 
  select(-c(Reservoir, Site_code, Dry_start, Dry_end, Distance))



#### Bind chem to sensors ----

chem_sens1 <- chem_df %>%
  left_join(
    sensor_df,
    by = c("Site", "Date"),
    suffix = c("_chem", "_sens")
  ) %>%
  mutate(depth_diff = abs(Depth_m_chem - Depth_m_sens)) %>%
  group_by(Site, Date, Distance, Depth_m_chem) %>%
  slice_min(depth_diff, n = 1, with_ties = FALSE) %>%
  ungroup() 

## check depth_diff column to see if any joins are unreasonable
chem_sensors <- chem_sens1 |> 
  rename(Depth_m = Depth_m_chem) |> #set name back for function to work
  #get rid of sensor values for site P2 cast on 19 June that didn't reach the bottom (difference in depth was 7.5 meters)
  mutate(SpCond_uScm = ifelse(Date == ymd("2024-06-19") & Site_code == "P2" & Depth_m == 20, NA, SpCond_uScm),
         Temp_C = ifelse(Date == ymd("2024-06-19") & Site_code == "P2" & Depth_m == 20, NA, Temp_C),
         DO_mgL = ifelse(Date == ymd("2024-06-19") & Site_code == "P2" & Depth_m == 20, NA, DO_mgL),
         DOsat_percent = ifelse(Date == ymd("2024-06-19") & Site_code == "P2" & Depth_m == 20, NA, DOsat_percent)
  ) |> 
  #fix column names and remove unneeded columns
  select(-c(Depth_m_sens, depth_diff, Max_Depth_Site_m))


#### Bind in daily Hydro data ----

chem_sensors_hydro <- chem_sensors |> 
  left_join(hydro_df, by = "Date") |> 
  select(-Daily_rain_mm)

#### Write final csv ####

# write.csv(chem_sensors_hydro, "./Data/chemistry_joined_sensor_hydro.csv", row.names = F)



