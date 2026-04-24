#### Make Hydro timeseries plot 

## packages and unit conversion factors 
library(tidyverse)
library(ggpmisc) #stat poly line

CFS_TO_LS  <- 28.3168   # cfs  → L/s
CMS_TO_LS  <- 1000      # cms  → L/s

#### Read  and prep data
hydro_daily <- read.csv("./Data/Hydrology_Daily_Observed.csv") |> 
  mutate(Date = as.Date(Date)) |> 
  filter(Date >= ymd("2024-05-01"),
         Date <= ymd("2025-04-30")) |> 
  #remove data that is after when first PT sensor sensor was lost
  mutate(
    HPB_daily_Stage_cm   = ifelse(Date > ymd("2025-02-09"), NA, HPB_daily_Stage_cm),
    Daily_Q_cms          = ifelse(Date > ymd("2025-02-09"), NA, Daily_Q_cms)
  ) |>
  ## Convert all flow columns to L/s
  mutate(
    HPB_Q_Ls          = Daily_Q_cms     * CMS_TO_LS,
    Flow_Catawba_Ls     = Flow_Catawba_cfs * CFS_TO_LS,
    Flow_Tinker_Ls      = Flow_Tinker_cfs  * CFS_TO_LS
  )
  
#### Check linear regressions between USGS flow and HPB
hydro_daily |> 
  ggplot(aes(x = Flow_Tinker_Ls, y = HPB_Q_Ls)) +
  geom_point() +
  # geom_smooth()+
  stat_poly_line(method = "lm", linewidth = 2) +
  stat_poly_eq(formula = y ~ x, label.x = "left", label.y = "top", parse = TRUE,
    inherit.aes = FALSE, aes(x = Flow_Tinker_Ls, y = HPB_Q_Ls,
        label = paste(..adj.rr.label.., ..p.value.label.., sep = "~~~"), size = 3)  ) +
  labs(x = "Tinker Flow (L/s)", y = "HPB Q (L/s)") +
  theme_bw()

hydro_daily |> 
  ggplot(aes(x = Flow_Catawba_Ls, y = HPB_Q_Ls)) +
  geom_point() +
  # geom_smooth()+
  stat_poly_line(method = "lm", linewidth = 2) +
  stat_poly_eq(formula = y ~ x, label.x = "left", label.y = "top", parse = TRUE,
               inherit.aes = FALSE,aes(x = Flow_Catawba_Ls, y = HPB_Q_Ls,
                                       label = paste(..adj.rr.label.., ..p.value.label.., sep = "~~~"), size = 3)  ) +
  labs(x = "Catawba Flow (L/s)", y = "HPB Q (L/s)") +
  theme_bw()



##### Use Linear offset of Tinker to HPB
lm_tinker <- lm(HPB_Q_Ls ~ Flow_Tinker_Ls, data = hydro_daily)
summary(lm_tinker)


hydro_daily <- hydro_daily |>
  mutate(HPB_Ls_tinkerLM = predict(lm_tinker, newdata = hydro_daily))

#look at residuals and RMSE
resid <- hydro_daily |> 
  mutate(resid_hpb_T = HPB_Q_Ls - HPB_Ls_tinkerLM)

summary(resid)

#RMSE Calcs
sqrt(mean((hydro_daily$HPB_Ls_tinkerLM - hydro_daily$HPB_Q_Ls)^2, na.rm = T))


#plot predicted vs observed
Pred_obsLM_fig <- hydro_daily |> 
  ggplot(aes(x = HPB_Ls_tinkerLM, y = HPB_Q_Ls))+
  geom_point()+
  xlim(0,300)+
  stat_poly_line(method = "lm", linewidth = 2) +
  stat_poly_eq(formula = y ~ x, label.x = "left", label.y = "top", parse = TRUE,
               inherit.aes = FALSE, 
               aes(x = HPB_Ls_tinkerLM, y = HPB_Q_Ls,
                   label = paste(..adj.rr.label.., ..p.value.label.., sep = "~~~"), size = 2)  )+
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", size = 1.2) +
  theme_bw() + labs(x = "HPB linear modeled discharge (L/s)", y = "HPB observed discharge (L/s)")

Pred_obsLM_fig


#plot timeseries for SI fig
hpb_regress_timeseries <- hydro_daily |> 
  select(Date, HPB_Q_Ls, HPB_Ls_tinkerLM, Flow_Tinker_Ls) |> 
  rename(HPB_Q_obs_L_s = HPB_Q_Ls,
         HPB_Q_modeled_L_s = HPB_Ls_tinkerLM,
         Tinker_Q_obs_L_s = Flow_Tinker_Ls) |> 
  pivot_longer(-1) |> 
  ggplot(aes(x = Date, y = value))+
  geom_point()+ facet_wrap(~factor(name, levels = c("HPB_Q_obs_L_s", "HPB_Q_modeled_L_s", "Tinker_Q_obs_L_s")), 
                           scales = "free_y", ncol = 1)+
  theme_bw()+ labs(y = "Discharge (L/s)")

hpb_regress_timeseries


##Make SI figure
library(patchwork)
hpb_regress_timeseries | Pred_obsLM_fig

HPB_regress_SI_fig <- cowplot::plot_grid(hpb_regress_timeseries, Pred_obsLM_fig, labels = c("a", "b"), ncol = 2)

#ggsave("./Figures/hpb_lm_SI_figure.png", HPB_regress_SI_fig, width = 6, height = 4, units = "in")







  
############################ MAKE SURE OLD CODE WORKS STILL


## write csv that has interpolated stage HPB
hydro_daily_export <- hydro_daily |> 
  mutate(HPB_Q_final = ifelse(is.na(HPB_Q_Ls), HPB_Ls_tinkerLM, HPB_Q_Ls )) |> 
  select(Date, Daily_rain_mm, Dam_daily_WaterLevel_m, HPB_Q_final) |> 
  rename(HPB_Q_L_s = HPB_Q_final)

# write.csv(hydro_daily_export, "./Data/Hydro_daily.csv", row.names = F)





#### Hydrology MS timeseries figure ####
sampling_dates <- data.frame(Date = c(ymd("2024-05-22"), ymd("2024-06-19"), ymd("2024-07-11"), ymd("2024-08-14"), ymd("2024-09-16"),
                    ymd("2024-09-30"), ymd("2024-10-28"), ymd("2024-12-17"), ymd("2025-02-26"), ymd("2025-04-16")))


precip_fig <- hydro_daily_export |> 
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



Q_fig <- hydro_daily_export |> 
  mutate(interp = ifelse(Date > ymd("2025-02-09"), "Interp", "Observed")) |> 
  ggplot(aes(x = as.Date(Date), y = HPB_Q_L_s, shape = interp))+
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
  labs(x= element_blank(), y = "Stream Discharge (L/s)")+
  theme_bw()+ theme(text = element_text(size = 12),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank())


waterlevel_fig <- hydro_daily_export |> 
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


hydro_TS_fig <- cowplot::plot_grid(precip_fig, Q_fig, waterlevel_fig, 
                                   labels = c("a", "b", "c"), nrow = 3)

hydro_TS_fig
# ggsave("./Figures/hydro_TS_figure_v2.png", hydro_TS_fig, width = 6, height = 7, units = "in")

#interactive plots
# plotly::ggplotly(waterlevel_fig) #For interactive plots
# plotly::ggplotly(precip_fig) #For interactive plots



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

