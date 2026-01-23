#### Compile discrete chemistry samples data



## packages 
library(tidyverse)

#Define sampling dates for filtering
sampling_dates <- c(ymd("2024-05-22"), ymd("2024-06-19"), ymd("2024-07-11"), ymd("2024-08-14"), ymd("2024-09-16"),
                    ymd("2024-09-30"), ymd("2024-10-28"), ymd("2024-12-17"), ymd("2025-02-26"), ymd("2025-04-16"))

#### EEMs 
eems_EDI <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/841/2/45207449d82365ab02caa4f7f3b05cc1")

eems_ccr <- eems_EDI |> 
  mutate(Date = as.Date(DateTime)) |> 
  filter(Reservoir == "CCR",
         Site != 94, #remove extra sampling site
         Date %in% sampling_dates #get just sampling dates
         ) |> 
  rename(Peak_A = A,
         Peak_T = 'T') |> 
  select(Reservoir, Site, Date, Depth_m, Rep, HIX, BIX, FI, Peak_A, Peak_T, A_T, a254_m) |> 
  group_by(Reservoir, Site, Date, Depth_m) |> 
  summarise(across(c(HIX, BIX, FI, Peak_A, Peak_T, A_T, a254_m), mean, na.rm = TRUE))

  




#### DOC and nutrients 
chem_EDI <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/199/13/3f09a3d23b7b5dd32ed7d28e9bc1b081")

chem_ccr <- chem_EDI |> 
  mutate(Date = as.Date(DateTime)) |> 
  filter(Reservoir == "CCR",
         Site != 94, #remove extra sampling site
         Date %in% sampling_dates #get just sampling dates
  ) |>
  select(Reservoir, Site, Date, Depth_m, Rep, DOC_mgL, DN_mgL, NO3NO2_ugL, NH4_ugL, SRP_ugL) |> 
  group_by(Reservoir, Site, Date, Depth_m) |> 
  summarise(across(c(DOC_mgL, DN_mgL, NO3NO2_ugL, NH4_ugL, SRP_ugL), mean, na.rm = TRUE))


#### Isotopes
iso_EDI <- read_csv("https://pasta.lternet.edu/package/data/eml/edi/1745/1/71e54498052b76e103898e0b07426a7c")

#filter to sites of interest
iso_ccr <- iso_EDI |> 
  filter(Site != 94) |> 
  select(Reservoir, Site, Date, Depth_m, d18O_VSMOW, d2H_VSMOW)



### Join dataframes 
joined_ccr_chem <- full_join(eems_ccr, iso_ccr, by = c("Reservoir", "Site","Date", "Depth_m")) |> 
  full_join(chem_ccr, by = c("Reservoir", "Site","Date", "Depth_m"))


#Site 90 (C2) had extra depth samples collected at 1.5 meters on each day, confirm this and remove those so the two Cove sites are just surface and max depth samples
site92 <- joined_ccr_chem |> filter(Site == 92) 
unique(site92$Depth_m)

site90 <- joined_ccr_chem |> filter(Site == 90) 
unique(site90$Depth_m)

joined_ccr_chem2 <- joined_ccr_chem |> 
  filter(!(Site == 90 & Depth_m == 1.5))

#write.csv(joined_ccr_chem2, "./Data/chemistry_joined.csv", row.names = F)




