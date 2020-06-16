## 01c Plotting Maps of Start Dates and R0
library(globallmicresults)
library(tidyverse)
library(lubridate)
library(rgdal)
library(raster)
library(viridis)
library(plyr)
library(dplyr)
library(fields)
library(squire)
library(rmapshaper)
library(rgeos)
library(ggpubr)


##  ------------------------------
## Analysis ------------------------------
##  ------------------------------

# get the reports
date_0 <- "2020-06-01"

grids <- grid_out_list(date_0)
param_sums <- lapply(seq_along(grids), function(i) {
  x <- grids[[i]]
  pars <- x$replicate_parameters
  pars$start_date <- as.numeric(pars$start_date)
  q_025 <- function(x) { quantile(x, 0.025)}
  q_975 <- function(x) { quantile(x, 0.975)}
    y <- summarise_all(pars, "mean")
  ymin <- summarise_all(pars, q_025)
  ymax <- summarise_all(pars, q_975)
  df <- data.frame("var" = names(y),
                   "y" = as.numeric(y),
                   "ymin" = as.numeric(ymin),
                   "ymax" = as.numeric(ymax))
  df$iso <- names(grids)[i]
  return(df)
})

params <- do.call(rbind, param_sums) %>%
  filter(var == "R0")

map_data_and_shape <- read.csv("analysis/data/map_plot_data/start_R0_Meff_date.csv", stringsAsFactors = FALSE) %>%
  select(long, lat, group, GID_0) %>%
  left_join(params, by = c("GID_0" = "iso"))

#Plotting R0 start
R0_start_map <- ggplot() +
  geom_polygon(data = map_data_and_shape,
               aes(x = long, y = lat, group = group, fill = y), color = "black", size = 0.1) +
  theme_void() +
  scale_fill_viridis(option = "C") +
  labs(fill = "Starting R0",
       ylab = "",
       xlab = "") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank())


##  ------------------------------
## Plotting ------------------------------
##  ------------------------------
map_data_and_shape <- read.csv("analysis/data/map_plot_data/start_R0_Meff_date.csv", stringsAsFactors = FALSE)

map_data_and_shape$start_date_mid <- as.Date(map_data_and_shape$start_date_mid, origin = ymd("1970/01/01"))


#Plotting R0 start
R0_start_map <- ggplot() +
  geom_polygon(data = map_data_and_shape,
               aes(x = long, y = lat, group = group, fill = R0_mid), color = "black", size = 0.1) +
  theme_void() +
  scale_fill_viridis(option = "C") +
  labs(fill = "Starting R0",
       ylab = "",
       xlab = "") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank())

Meff_start_map <- ggplot() +
  geom_polygon(data = map_data_and_shape,
               aes(x = long, y = lat, group = group, fill = Meff_mid), color = "black", size = 0.1) +
  theme_void() +
  scale_fill_viridis(option = "C") +
  labs(fill = "Starting Meff",
       ylab = "",
       xlab = "") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank())

lab_dates <- pretty(map_data_and_shape$start_date_mid, n = 5)

Date_start_map <- ggplot() +
  geom_polygon(data = map_data_and_shape,
               aes(x = long, y = lat, group = group, fill = as.numeric(start_date_mid)), color = "black", size = 0.1) +
  theme_void() +
  scale_fill_viridis(option = "C",
                     breaks = as.numeric(lab_dates),
                     labels = lab_dates) +
  labs(fill = "Starting Date",
       ylab = "",
       xlab = "") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank())


all_maps <- ggarrange(R0_start_map,
          Meff_start_map,
          Date_start_map,
          ncol = 1)


ggsave("analysis/figures/01c_R0_start_date_maps.png", height = 6, width = 6, dpi = 400)
