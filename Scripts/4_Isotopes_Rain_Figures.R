#### Make Isotope timeseries figures


##packages 
library(tidyverse)


##get data
hydro <- read_csv("./Data/Hydrology_Daily_Observed.csv") |> 
  filter(Date >= ymd("2024-05-01"),
         Date <= ymd("2025-04-30"))

iso <- read_csv("./Data/chemistry_joined.csv")


#### Visualize isotope data sets #####

#plot over distance 
##d2H and d18O and D excess
isotopes_across_space_fig <- iso |> 
  dplyr::select(1:8, d18O_VSMOW, d2H_VSMOW, D_excess) |> 
  pivot_longer(-c(1:8)) |> 
  ggplot(aes(x = Distance, y = value, fill = Depth_m
  ))+
  geom_point(shape = 21, size = 3) + 
  #geom_point(data = iso_rain, mapping = aes(x = 1250, y = value, color = Date), size = 2, shape = 17)+
  facet_wrap(~name, scales = "free_y", nrow = 1)+
  theme_bw()+ theme(legend.position = "top", text = element_text(size = 18),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x= "Distance from Stream (m)",  fill = "Depth (m)", color = "Rain")+
  scale_x_continuous(breaks = c(0, 500, 1000, 1500))+  
  scale_fill_gradient2(low = "red",  high ="blue",
                       midpoint = 6,  guide = "colourbar", breaks = c(3,6,9, 15, 20))

isotopes_across_space_fig

# ggsave("./Figures/Iso_overSpace_SI_fig.png", isotopes_across_space_fig, width = 6.5, height = 3.5, units = "in")


#### Iso timeseries with rain ####

### Set levels and color scales
iso_levels_sites <- c("S1","S2","B1","B2","C1","C2","P1","P2")

site_colors <- c("S1" = "#30123BFF", "S2" = "#30123BFF",
                 "B1" = "#1BD0D5FF", "B2" = "#1BD0D5FF",
                 "C1" = "#D2E935FF", "C2" = "#D2E935FF",
                 "P1" = "#DB3A07FF", "P2" = "#DB3A07FF")

site_shapes <- c("S1" = 15, "S2" = 16,
                 "B1" = 15, "B2" = 16,
                 "C1" = 15, "C2" = 16,
                 "P1" = 15, "P2" = 16)




#### Make plot for surface water ####
#combine data frames 
plot_data_surface <- iso %>%
  select(1:6, d2H_VSMOW, d18O_VSMOW, D_excess) |> 
  mutate(Depth_new = as.character(ifelse(Site %in% c(98,96,92, 90) & Depth_m > 0.1, "BOT", Depth_m)),
         Depth_new = as.character(ifelse(Site %in% c(88,50) & Depth_m > 9, "BOT", Depth_new))
  ) |> 
  filter(Depth_new %in% c("0.1")) |> 
  full_join(hydro, by = "Date") 


# Scaling factor for rainfall bars
max_dexcess <- max(plot_data_surface$D_excess, na.rm = TRUE)
max_rain    <- max(plot_data_surface$Daily_rain_mm, na.rm = TRUE)
scale_factor <- max_dexcess / max_rain



# Build plotting dataset with rain

iso_rain_surface_fig <- plot_data_surface |> 
  ggplot() +
  # Rain bars (no legend)
  geom_col(aes(x = Date, y = Daily_rain_mm * scale_factor),
           fill = "lightblue", width = 1,
           show.legend = FALSE) +
  
  # Isotope points and lines
  geom_point(aes(x = Date, y = D_excess,
                 color = Site_code,
                 shape = Site_code,
                 group = Site_code),
             size = 3) +
  geom_line(aes(x = Date, y = D_excess,
                color = Site_code,
                group = Site_code),
            size = 1.3) +
  
  # Labels and annotation
  labs(x = "Date", y = "d-excess (\u2030)", color = "Site", shape = "Site") +
  # annotate("text", x = ymd("2024-06-01"), y = 18,
  #          label = "D excess = dD - 8*d18O",
  #          size = 5, color = "black", hjust = 0) +
  
  # Theme
  theme_bw() +
  theme(
    legend.position = "top",
    text = element_text(size = 18),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  
  # Manual scales with enforced order
  scale_x_date(date_labels = "%b %Y ", date_breaks = "3 month" )+
  scale_color_manual(values = site_colors, breaks = iso_levels_sites) +
  scale_shape_manual(values = site_shapes, breaks = iso_levels_sites) +
  
  # Secondary axis for rainfall
  scale_y_continuous(
    sec.axis = sec_axis(~ . / scale_factor, name = "Rainfall (mm)")
  )

iso_rain_surface_fig

# ggsave("./Figures/Iso_Rain_surf_MS_fig.png", iso_rain_surface_fig, width = 6.5, height = 4, units = "in")


#### Make plots for 0.1 and BOT ####

iso_levels_sites <- c("S1","S2","B1","B2","C1","C2","P1","P2")

site_colors <- c("S1" = "#30123BFF", "S2" = "#30123BFF",
                 "B1" = "#1BD0D5FF", "B2" = "#1BD0D5FF",
                 "C1" = "#D2E935FF", "C2" = "#D2E935FF",
                 "P1" = "#DB3A07FF", "P2" = "#DB3A07FF")

site_shapes <- c("S1" = 15, "S2" = 16,
                 "B1" = 15, "B2" = 16,
                 "C1" = 15, "C2" = 16,
                 "P1" = 15, "P2" = 16)


plot_data_surf_bot <- iso %>%
  select(1:6, d2H_VSMOW, d18O_VSMOW, D_excess) |> 
  mutate(Depth_new = as.character(ifelse(Site %in% c(98,96,92, 90) & Depth_m > 0.1, "BOT", Depth_m)),
         Depth_new = as.character(ifelse(Site %in% c(88,50) & Depth_m > 9, "BOT", Depth_new))
  ) |> 
  filter(Depth_new %in% c("0.1", "BOT")) |> 
  full_join(hydro, by = "Date") |> 
  mutate(Depth_new = ifelse(is.na(Depth_new), "0.1", Depth_new)) #Making all rows where is just precip data have a depth assoiated so there's no NA in the legend


# Scaling factor for rainfall bars
max_dexcess <- max(plot_data_surf_bot$D_excess, na.rm = TRUE)
max_rain    <- max(plot_data_surf_bot$Daily_rain_mm, na.rm = TRUE)
scale_factor <- max_dexcess / max_rain


#### Plot surface and bottom with rain
iso_rain_SURFBOT_fig <- plot_data_surf_bot |> 
  ggplot() +
  # Rain bars (no legend)
  geom_col(aes(x = Date, y = Daily_rain_mm * scale_factor),
           fill = "lightblue", width = 1,
           show.legend = FALSE) +
  
  # Isotope points and lines
  geom_point(aes(x = Date, y = D_excess,
                 color = Site_code,
                 shape = Site_code,
                 group = interaction(Site_code, Depth_new)),
             size = 3) +
  geom_line(aes(x = Date, y = D_excess,
                color = Site_code,
                linetype = Depth_new ,
                group = interaction(Site_code, Depth_new )),
            size = 1.3) +
  
  # Labels and annotation
  labs(x = "Date", y = "d-excess (\u2030)",
       color = "Site", shape = "Site", linetype = "Depth") +
  # annotate("text", x = ymd("2024-06-01"), y = 18,
  #          label = "D excess = dD - 8*d18O",
  #          size = 5, color = "black", hjust = 0) +
  
  # Theme
  theme_bw() +
  theme(
    legend.position = "top",
    text = element_text(size = 18),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  
  # Manual scales with enforced order
  scale_x_date(date_labels = "%b %Y ") +
  scale_color_manual(values = site_colors, breaks = iso_levels_sites) +
  scale_shape_manual(values = site_shapes, breaks = iso_levels_sites) +
  scale_linetype_manual(values = c(
    "0.1" = "solid",
    "BOT"   = "twodash"
  )) +
  
  # Secondary axis for rainfall
  scale_y_continuous(
    sec.axis = sec_axis(~ . / scale_factor, name = "Rainfall (mm)")
  )

iso_rain_SURFBOT_fig

# ggsave("./Figures/Iso_Rain_surfBOT_SI_fig.png", iso_rain_SURFBOT_fig, width = 6.5, height = 5, units = "in")



######## Depth on Y plots by date #####

levels_db <- c("22 May", "19 Jun", "11 Jul", "14 Aug", "16 Sep", 
               "30 Sep", "28 Oct", "17 Dec", "26 Feb", "16 Apr")

####Stream to C1

iso |> 
  filter(!Site %in% c(50,88,90)) |> 
  mutate(Dry_start = ifelse(Depth_m == 0.1 & Site_code == "S1", Dry_start, NA),
         Dry_end = ifelse(Depth_m == 0.1 & Site_code == "S1", Dry_end, NA)) |> #so just one row has info for dry start and end, was having issues where laying multiple rectangles for more observations affected shading color
  mutate(Date1 = format(Date, "%d %b")) |> 
  ggplot(aes(x = Distance, y = Depth_m, fill = D_excess))+
  geom_rect(aes(xmin = Dry_start, xmax = Dry_end, ymin = -Inf, ymax = Inf), 
            alpha = 0.5, fill = "gray", color = NA )+ #color gets rid of border
  facet_wrap(~factor(Date1, levels = levels_db), scales = "fixed", nrow = 1)+
  geom_point(size = 3, shape = 21)+ #need shape here if not doing site type
  scale_y_reverse()+
  theme_bw()+ theme(legend.position = "top", text = element_text(size = 18),
                    legend.text = element_text(size = 12),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x= "Distance from Stream (m)",
       y = "Depth (m)", fill = "D excess")+
  scale_x_continuous(breaks = c(0, 150, 300)) +
  #scale_fill_gradient(low = "blue",  high ="red")
  scale_fill_gradient2(low = "blue",  high ="red", breaks = c(5,10,15),
                       midpoint = 10,  guide = "colourbar")


####all sites

iso |> 
  mutate(Dry_start = ifelse(Depth_m == 0.1 & Site_code == "S1", Dry_start, NA),
         Dry_end = ifelse(Depth_m == 0.1 & Site_code == "S1", Dry_end, NA)) |> #so just one row has info for dry start and end, was having issues where laying multiple rectangles for more observations affected shading color
  mutate(Date1 = format(Date, "%d %b")) |> 
  ggplot(aes(x = Distance, y = Depth_m, fill = D_excess))+
  geom_rect(aes(xmin = Dry_start, xmax = Dry_end, ymin = -Inf, ymax = Inf), 
            alpha = 0.5, fill = "gray", color = NA )+ #color gets rid of border
  facet_wrap(~factor(Date1, levels = levels_db), scales = "fixed", nrow = 1)+
  geom_point(size = 3, shape = 21)+ #need shape here if not doing site type
  scale_y_reverse()+
  theme_bw()+ theme(legend.position = "top", text = element_text(size = 18),
                    legend.text = element_text(size = 12),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x= "Distance from Stream (m)",
       y = "Depth (m)", fill = "D excess")+
  scale_x_continuous(breaks = c(0, 500, 1000, 1500))+  
  #scale_fill_gradient(low = "blue",  high ="red")
  scale_fill_gradient2(low = "blue",  high ="red", breaks = c(5,10,15),
                       midpoint = 10,  guide = "colourbar")
