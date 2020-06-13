# Loading Required Libraries
library(globallmicresults); library(tidyverse); library(lubridate);library(rgdal)
library(raster); library(viridis); library(dplyr); library(fields)
library(squire); library(rmapshaper); library(rgeos); library(ggpubr);
library(patchwork); library(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("area", "patchwork")

# Collating Report Dates
date_0 <- "2020-06-01"
reports <- reports_day(date_0)

# Accessing World Bank Metadata
base_path <- here::here()
file_path <- "/analysis/data/raw_data/server_results/archive/brt_google_mobility/20200602-002850-6baa471e/World_Bank_Country_Metadata.csv"
raw_wb_metadata <- read.csv(paste0(base_path, file_path))
wb_metadata <- raw_wb_metadata %>% 
  rename(iso = ï..country_code) %>%
  dplyr::select(iso, income_group) %>%
  filter(income_group != "") %>%
  mutate(income_group = factor(income_group, levels = rev(c("Low income", "Lower middle income", "Upper middle income", "High income"))))

# Loading ECDC Data and Joining World Bank Metadata
ecdc <- get_ecdc(date_0)
ecdc <- ecdc %>%
  mutate(continentExp = factor(continentExp), dateRep = as.Date(dateRep)) %>%
  left_join(wb_metadata, by = c("countryterritoryCode" = "iso")) %>%
  filter(!is.na(income_group))

ecdc_income_group <- ecdc %>%
  ungroup() %>%
  group_by(dateRep, income_group) %>%
  summarise(deaths = sum(deaths))

ecdc_income_group_cumdeaths <- ecdc_income_group %>%
  ungroup() %>%
  group_by(income_group) %>%
  mutate(cum_deaths = cumsum(deaths)) 

# Plotting Cumulative Deaths by Income Strata 
a <- ggplot(ecdc_income_group_cumdeaths, aes(x = dateRep, y = cum_deaths, fill = income_group)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(x = "Date", y = "Cumulative Deaths") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9", "blue"), 
                    name = "Income Strata",
                    breaks = c("High income", "Upper middle income", "Lower middle income", "Low income"),
                    labels = c("High Income", "Upper-Middle Income", "Lower Middle Income", "Low Income")) +
  guides(fill = guide_legend(reverse = TRUE))

#
grids <- grid_out_list(date_0)
param_sums <- lapply(seq_along(grids), function(i) {
  x <- grids[[i]]
  pars <- x$replicate_parameters
  pars$start_date <- as.numeric(pars$start_date)
  y <- summarise_all(pars, "mean")
  df <- data.frame("var" = names(y),
                   "y" = as.numeric(y))
  df$iso <- names(grids)[i]
  return(df)
})
params <- do.call(rbind, param_sums)
R0 <- params %>%
  filter(var == "R0")

map_data_and_shape <- read.csv("/analysis/data/map_plot_data/start_R0_Meff_date.csv", stringsAsFactors = FALSE)
map_data_and_shape$start_date_mid <- as.Date(map_data_and_shape$start_date_mid, origin = ymd("1970/01/01"))


