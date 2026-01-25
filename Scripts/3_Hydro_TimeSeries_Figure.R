#### Make Hydro timeseries plot 


## packages 
library(tidyverse)

## read in data
hydro_daily <- read.csv("./Data/Hydrology_Daily_Observed.csv") |> 
  mutate(Date = as.Date(Date)) |> 
  filter(Date >= ymd("2024-05-01"),
         Date <= ymd("2025-04-30")) |> 
  mutate(HPB_daily_Stage_cm = ifelse(Date > ymd("2024-12-17"), NA, HPB_daily_Stage_cm))
  
  
  
######## Gap fill missing HPB stage data ####
library(ggpmisc) #stat poly line


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
library(mgcv)


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


hydro_daily |> 
  ggplot(aes(x = predicted_hpb_CT, y = HPB_daily_Stage_cm))+
  geom_point()+
  stat_poly_line(method = "lm", linewidth = 2)+
  stat_poly_eq(formula=y~x, label.x = "left", label.y="top", parse=TRUE, inherit.aes = F,
               aes(x = predicted_hpb_CT , y = HPB_daily_Stage_cm , label=paste(..adj.rr.label..,..p.value.label..,sep="~~~"),size=3))+
  theme_bw()




#### Hydrology MS timeseries figure ####

hydro_TS_fig <- hydro_daily |> 
  mutate(interp = ifelse(is.na(HPB_daily_Stage_cm), "Interp", "Observed")) |> 
  mutate(HPB = ifelse(is.na(HPB_daily_Stage_cm), predicted_hpb_CT, HPB_daily_Stage_cm )) |> 
  select(Date, interp, Daily_rain_mm, HPB, Dam_daily_WaterLevel_m) |> 
  rename(HPB_stage_cm = HPB,
         CCR_WaterLevel_m = Dam_daily_WaterLevel_m) |> 
  pivot_longer(-c(1:2)) |> 
  mutate(interp = ifelse(name != "HPB_stage_cm", "Observed", interp )) |> #fix interp column so just affects HPB where interp happened
    ggplot(aes(x = as.Date(Date), y = value, shape = interp))+
  geom_point()+
  scale_shape_manual(values = c("Observed" = 16, "Interp" = 1), guide = "none")+
  # scale_y_log10()+
  geom_vline(xintercept = ymd("2024-05-22"))+  geom_vline(xintercept = ymd("2024-06-19"))+
  geom_vline(xintercept = ymd("2024-07-11"))+  geom_vline(xintercept = ymd("2024-08-14"))+
  geom_vline(xintercept = ymd("2024-09-16"))+  geom_vline(xintercept = ymd("2024-09-30"))+
  geom_vline(xintercept = ymd("2024-10-28"))+  geom_vline(xintercept = ymd("2024-12-17"))+
  geom_vline(xintercept = ymd("2025-02-26"))+  geom_vline(xintercept = ymd("2025-04-16"))+
  facet_wrap(~factor(name, levels = c("Daily_rain_mm", "HPB_stage_cm", "CCR_WaterLevel_m")), ncol = 1, scales = "free_y")+
  scale_x_date(breaks = "1 months", date_labels = "%b")+
  labs(x= element_blank())+
  theme_bw()+ theme(text = element_text(size = 18),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank())


hydro_TS_fig

plotly::ggplotly(hydro_TS_fig)

# ggsave("./Figures/hydro_TS_figure.png", hydro_TS_fig, width = 6.5, height = 4.5, units = "in")


## SI figure for HPB~USGS
hydro_daily |> 
  select(Date, HPB_daily_Stage_cm, Stage_Catawba_ft, Stage_Tinker_ft, predicted_hpb_T) |> 
  rename(HPB_GAM_modeled_cm = predicted_hpb_T) |> 
  pivot_longer(-1) |> 
  ggplot(aes(x = Date, y = value))+
  geom_point()+
  facet_wrap(~name, scales = "free_y", ncol = 1)


## some stats for paper 
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

