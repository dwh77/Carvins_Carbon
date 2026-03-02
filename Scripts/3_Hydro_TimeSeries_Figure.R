#### Make Hydro timeseries plot 


## packages 
library(tidyverse)
library(ggpmisc) #stat poly line
library(mgcv) #for GAMs



## read in data
hydro_daily <- read.csv("./Data/Hydrology_Daily_Observed.csv") |> 
  mutate(Date = as.Date(Date)) |> 
  filter(Date >= ymd("2024-05-01"),
         Date <= ymd("2025-04-30")) |> 
  mutate(HPB_daily_Stage_cm = ifelse(Date > ymd("2024-12-17"), NA, HPB_daily_Stage_cm))
  
  
  
######## Gap fill missing HPB stage data ####


#### Look at linear regressions between USGS stage and HPB stage
#Catawba
hydro_daily |> 
  ggplot(aes(x = Stage_Catawba_ft , y = HPB_daily_Stage_cm ))+
  geom_point()+
  stat_poly_line(method = "lm", linewidth = 2)+
  stat_poly_eq(formula=y~x, label.x = "left", label.y="top", parse=TRUE, inherit.aes = F,
               aes(x = Stage_Catawba_ft , y = HPB_daily_Stage_cm , label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))+
  theme_bw()

#Tinker
hydro_daily |> 
  ggplot(aes(x = Stage_Tinker_ft , y = HPB_daily_Stage_cm ))+
  geom_point()+
  stat_poly_line(method = "lm", linewidth = 2)+
  stat_poly_eq(formula=y~x, label.x = "left", label.y="top", parse=TRUE, inherit.aes = F,
               aes(x = Stage_Tinker_ft , y = HPB_daily_Stage_cm , label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))+
  theme_bw()



#### Try GAMs 

hydro_daily |> 
  ggplot(aes(x = Stage_Tinker_ft, y = HPB_daily_Stage_cm))+
  geom_point()+
  geom_smooth(method = "gam", formula = y ~ s(x))

##Tinker gam 
model <- gam(HPB_daily_Stage_cm ~ s(Stage_Tinker_ft), data = hydro_daily) 
summary(model)

hydro_daily$predicted_hpb_T <- predict(model, newdata = hydro_daily)

##Catawba gam
model2 <- gam(HPB_daily_Stage_cm ~ s(Stage_Catawba_ft), data = hydro_daily) 
summary(model2)

hydro_daily$predicted_hpb_C <- predict(model2, newdata = hydro_daily)

#Both USGS gam
model3 <- gam(HPB_daily_Stage_cm ~ s(Stage_Catawba_ft) + s(Stage_Tinker_ft), data = hydro_daily) 
summary(model3)

hydro_daily$predicted_hpb_CT <- predict(model3, newdata = hydro_daily)


#plot fits vs observed
hydro_daily |> 
  mutate(Date = as.Date(Date)) |> 
  #select(Date, catawba, tinker, hpb, predicted_hpb_C, predicted_hpb_T, predicted_hpb_CT) |> 
  select(Date, HPB_daily_Stage_cm, predicted_hpb_C, predicted_hpb_T, predicted_hpb_CT) |> 
  pivot_longer(-1) |> 
  ggplot(aes(x= Date, y= value, color = name)) +
  geom_point() +
  #scale_y_log10()+
  theme_bw()


#look at residuals 
resid <- hydro_daily |> 
  mutate(resid_hpb_C = HPB_daily_Stage_cm - predicted_hpb_C,
         resid_hpb_T = HPB_daily_Stage_cm - predicted_hpb_T,
         resid_hpb_CT = HPB_daily_Stage_cm - predicted_hpb_CT)

#look ar residual summary
summary(resid)

##RMSE Calcs
sqrt(mean((hydro_daily$predicted_hpb_C - hydro_daily$HPB_daily_Stage_cm)^2, na.rm = T))
sqrt(mean((hydro_daily$predicted_hpb_T - hydro_daily$HPB_daily_Stage_cm)^2, na.rm = T))
sqrt(mean((hydro_daily$predicted_hpb_CT - hydro_daily$HPB_daily_Stage_cm)^2, na.rm = T))

#plot predicted vs observed
hydro_daily |> 
  select(Date, HPB_daily_Stage_cm, Stage_Catawba_ft, Stage_Tinker_ft, predicted_hpb_T, predicted_hpb_C, predicted_hpb_CT) |> 
  pivot_longer(-1) |> 
  ggplot(aes(x = Date, y = value))+
  geom_point()+
  scale_y_log10()+
  facet_wrap(~name, scales = "free_y", ncol = 1)


## write csv that has interpolated stage HPB
hydro_daily_export <- hydro_daily |> 
  mutate(HPB_stage_cm = ifelse(is.na(HPB_daily_Stage_cm), predicted_hpb_T, HPB_daily_Stage_cm )) |> 
  select(Date, Daily_rain_mm, Dam_daily_WaterLevel_m, HPB_stage_cm) 

# write.csv(hydro_daily_export, "./Data/Hydro_daily.csv", row.names = F)





#### Hydrology MS timeseries figure ####
sampling_dates <- data.frame(Date = c(ymd("2024-05-22"), ymd("2024-06-19"), ymd("2024-07-11"), ymd("2024-08-14"), ymd("2024-09-16"),
                    ymd("2024-09-30"), ymd("2024-10-28"), ymd("2024-12-17"), ymd("2025-02-26"), ymd("2025-04-16")))


precip_fig <- hydro_daily |> 
  ggplot(aes(x = as.Date(Date), y = Daily_rain_mm))+
  #shade Helene storm period
  geom_rect(aes(xmin = ymd("2024-09-23"), xmax = ymd("2024-10-02"), ymin = -Inf, ymax = Inf), 
            alpha = 1, fill = "gray", color = NA )+ 
  #shade Feb storm period
  geom_rect(aes(xmin = ymd("2025-02-12"), xmax = ymd("2025-02-16"), ymin = -Inf, ymax = Inf), 
            alpha = 1, fill = "gray", color = NA )+ 
  geom_point()+
  geom_point(data = sampling_dates, aes(x = Date, y = Inf), shape = 25, size = 3, fill = "black") +
  scale_x_date(breaks = "1 months", date_labels = "%b")+
  labs(x= element_blank(), y = "Daily Rain (mm)")+
  theme_bw()+ theme(text = element_text(size = 12),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank())



stage_fig <- hydro_daily |> 
  mutate(interp = ifelse(is.na(HPB_daily_Stage_cm), "Interp", "Observed")) |> 
  mutate(HPB_stage_cm = ifelse(is.na(HPB_daily_Stage_cm), predicted_hpb_T, HPB_daily_Stage_cm )) |> # use predicted from T, based on plot above where modeled best represents increases around storms
  ggplot(aes(x = as.Date(Date), y = HPB_stage_cm, shape = interp))+
  #shade Helene storm period
  geom_rect(aes(xmin = ymd("2024-09-23"), xmax = ymd("2024-10-02"), ymin = -Inf, ymax = Inf), 
            alpha = 1, fill = "gray", color = NA )+ 
  #shade Feb storm period
  geom_rect(aes(xmin = ymd("2025-02-12"), xmax = ymd("2025-02-16"), ymin = -Inf, ymax = Inf), 
            alpha = 1, fill = "gray", color = NA )+ 
  geom_point()+
  geom_point(data = sampling_dates, aes(x = Date, y = Inf), shape = 25, size = 3, fill = "black") +
  scale_shape_manual(values = c("Observed" = 16, "Interp" = 5), guide = "none")+
  scale_x_date(breaks = "1 months", date_labels = "%b")+
  labs(x= element_blank(), y = "Stream Stage (cm)")+
  theme_bw()+ theme(text = element_text(size = 12),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank())


waterlevel_fig <- hydro_daily |> 
  ggplot(aes(x = as.Date(Date), y = Dam_daily_WaterLevel_m))+
  #shade Helene storm period
  geom_rect(aes(xmin = ymd("2024-09-23"), xmax = ymd("2024-10-02"), ymin = -Inf, ymax = Inf), 
            alpha = 1, fill = "gray", color = NA )+ 
  #shade Feb storm period
  geom_rect(aes(xmin = ymd("2025-02-12"), xmax = ymd("2025-02-16"), ymin = -Inf, ymax = Inf), 
            alpha = 1, fill = "gray", color = NA )+ 
  geom_point()+
  geom_point(data = sampling_dates, aes(x = Date, y = Inf), shape = 25, size = 3, fill = "black") +
  scale_x_date(breaks = "1 months", date_labels = "%b")+
  labs(x= element_blank(), y = "Reservoir Depth (m)")+
  theme_bw()+ theme(text = element_text(size = 12),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank())


hydro_TS_fig <- cowplot::plot_grid(precip_fig, stage_fig, waterlevel_fig, 
                                   labels = c("a", "b", "c"), nrow = 3)

hydro_TS_fig
# ggsave("./Figures/hydro_TS_figure_v2.png", hydro_TS_fig, width = 6, height = 7, units = "in")

#interactive plots
# plotly::ggplotly(waterlevel_fig) #For interactive plots
# plotly::ggplotly(precip_fig) #For interactive plots


#### SI figure for HPB~USGS
hpb_regress_timeseries <- hydro_daily |> 
  select(Date, HPB_daily_Stage_cm, Stage_Tinker_ft, predicted_hpb_T) |> 
  rename(HPB_GAM_modeled_cm = predicted_hpb_T) |> 
  pivot_longer(-1) |> 
  ggplot(aes(x = Date, y = value))+
  geom_point()+
  facet_wrap(~name, scales = "free_y", ncol = 1)

## check out predicted vs observed HPB values
hpb_GAM_vs_observed <-  hydro_daily |> 
  rename(HPB_GAM_modeled_cm = predicted_hpb_T) |> 
  filter(!is.na(HPB_daily_Stage_cm)) |> 
  ggplot(aes(x = HPB_GAM_modeled_cm, y = HPB_daily_Stage_cm))+
  geom_point()+
  stat_poly_line(method = "lm", linewidth = 2)+
  stat_poly_eq(formula=y~x, label.x = "left", label.y="top", parse=TRUE, inherit.aes = F,
               aes(x = predicted_hpb_CT , y = HPB_daily_Stage_cm , label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))+
  theme_bw()

library(patchwork)
hpb_regress_timeseries | hpb_GAM_vs_observed

HPB_regress_SI_fig <- cowplot::plot_grid(hpb_regress_timeseries, hpb_GAM_vs_observed, labels = c("a", "b"), ncol = 2)

 # ggsave("./Figures/hpb_gam_SI_figure.png", HPB_regress_SI_fig, width = 6, height = 4, units = "in")



####### some stats for paper #####

#Helene Rain
helene_rain <- hydro_daily |> 
  filter(Date >= ymd("2024-09-23"),
         Date <= ymd("2024-10-02")) 

sum(helene_rain$Daily_rain_mm)

#Feb ice and rain
# This website has some nice info to confirm these rain dates: https://weatherspark.com/h/m/146957/2025/2/Historical-Weather-in-February-2025-at-Roanoke-Regional-Airport-Woodrum-Field-Virginia-United-States
feb_rain_snow_storm <- hydro_daily |> 
  filter(Date >= ymd("2025-02-12"),
         Date <= ymd("2025-02-16")) 

sum(feb_rain_snow_storm$Daily_rain_mm)


##total rain through period 
sum(hydro_daily$Daily_rain_mm)

#proportion from two storms 
(sum(helene_rain$Daily_rain_mm) + sum(feb_rain_snow_storm$Daily_rain_mm)) / sum(hydro_daily$Daily_rain_mm)

