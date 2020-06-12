# Loading Required Libraries and Dependencies
library(devtools);  library(ggfortify); library(factoextra); library(cowplot)
library(pbapply); library(tidyverse); library(MALDIquant); library(rgl);
library(patchwork); library(fmsb)
#devtools::load_all()
#devtools::install_deps()
set.seed(10)

# Loading Required Functions and Scripts
setwd(here::here())
source("R/data_incoming.R")
source("R/reports.R")

# Getting Correct Meff Data, ECDC Data, Google Mobility Data Etc
date_0 <- "2020-06-01"
reports <- reports_day(date_0)
brt <- get_brt_predictions(date_0)
meffs <- pbapply::pblapply(seq_along(reports$id), function(x) {

  iso <- reports$country[x]
  out <- file.path(here::here(),
                   "analysis/data/raw_data/server_results/archive/lmic_reports_google",
                   reports$id[x], "grid_out.rds")
  out <- readRDS(out)

  dfs <- lapply(1:100, function(y) {

    # this function formats the dates related to the assumed start date
    # here R0_change is google mobility
    tt <- squire:::intervention_dates_for_odin(dates = out$interventions$date_R0_change,
                                               change = out$interventions$R0_change,
                                               start_date = out$replicate_parameters$start_date[y],
                                               steps_per_day = 1/out$parameters$dt)

    # Rt is related to R0, Meff and R0_change
    Rt <- vapply(tt$change, out$scan_results$inputs$Rt_func, numeric(1),
                 R0 = out$replicate_parameters$R0[y], Meff = out$replicate_parameters$Meff[y])

    df <- data.frame(
      "R0" = out$replicate_parameters$R0[y],
      "Rt" = tail(Rt, 1),
      "Rt_min" = min(Rt),
      "Meff" = out$replicate_parameters$Meff[y],
      "date" = as.character(out$replicate_parameters$start_date[y]),
      "iso" = iso,
      "max_mobility_change" = max(out$interventions$R0_change)-min(out$interventions$R0_change),
      stringsAsFactors = FALSE)
    return(df)
  } )

  df <- do.call(rbind, dfs)
  return(df)
})

# Creating the meff_all dataframe, converting dates and then summarising to 1 value per country
meff_all <- do.call(rbind, meffs)
meff_all$continent <- countrycode::countrycode(meff_all$iso, "iso3c", "continent")
meff_all$country <- countrycode::countrycode(meff_all$iso, "iso3c", "country.name")
meff_all$start_date <- as.numeric(as.Date(meff_all$date)) - as.numeric(as.Date("2020-01-01"))
index <- which(colnames(meff_all) == "date")
meff_all <- meff_all[, -index] 
meff_sum <- group_by(meff_all, iso, continent, country) %>%
  summarise_all(mean) %>%
  ungroup %>%
  as.data.frame()
rownames(meff_sum) <- meff_sum$country

# Adding in date of minimum mobility relative to start of epidemic
meff_sum$rel_mob_min_date <- vapply(meff_sum$iso, function(x) {
  mobdf <- brt[[x]]
  m <- predict(loess(C~as.numeric(date), data=mobdf, span = 0.2), type = "response")
  mob_min_date <- mobdf$date[which.min(m)]
  return(mob_min_date)},
  numeric(1)) - meff_sum$start_date - as.numeric(as.Date("2020-01-01"))
meff_sum$date_of_epidemic_start <- as.Date(meff_sum$start_date, origin = "2020-01-01")
meff_sum$date_minimum_mobility <- as.Date(meff_sum$rel_mob_min_date + meff_sum$start_date, origin = "2020-01-01")

# PCA and k-means to identify clusters
clustering_variables <- c("R0", "Rt_min", "Meff", "start_date", "max_mobility_change", "rel_mob_min_date")
cor(meff_sum[, clustering_variables])
pca <- prcomp(meff_sum[, clustering_variables], scale. = TRUE)
summary(pca)
contribution <- sweep(abs(pca$rotation), 2, colSums(abs(pca$rotation)), "/") * 100

# Checking optimal cluster number using silhouette and gap statistic methods
factoextra::fviz_nbclust(pca$x[, 1:ncol(pca$x)], kmeans, method = "silhouette")
gap_stat <- cluster::clusGap(pca$x[, 1:ncol(pca$x)], FUN = kmeans, nstart = 25, K.max = 10, B = 50)
factoextra::fviz_gap_stat(gap_stat)

# Clustering For Plotting Based On the First 2 Principal Components 
number_clusters <- 6
kmeans_out_plot <- kmeans(pca$x[, 1:2], centers = number_clusters)
clust <- fviz_cluster(kmeans_out_plot, data = pca$x[, 1:2], repel = TRUE) +
  theme_bw() +
  theme(legend.position = "right", plot.title = element_blank())
clust

# Clustering For Comparison of Empirical Properties Based On All Principal Components
kmeans_out_examine <- kmeans(pca$x[, 1:ncol(pca$x)], centers = number_clusters)
meff_sum$cluster <- unname(kmeans_out_examine$cluster)

# Summarising Results By Cluster, Getting the Mean and SD
summary <- meff_sum %>%
  group_by(cluster) %>%
  summarise(n = n(), 
            R0_mean = mean(R0), 
            Rt_min_mean = mean(Rt_min), 
            Meff_mean = mean(Meff), 
            max_mobility_change_mean = mean(max_mobility_change), 
            start_date_mean = mean(start_date),
            rel_mob_min_date = mean(rel_mob_min_date))
summary_plot <- summary %>%
  gather(metric, value, -cluster, -n)

summary_sd <- meff_sum %>%
  group_by(cluster) %>%
  summarise(n = n(), 
            R0_sd = sd(R0), 
            Rt_min_sd = sd(Rt_min)/sqrt(n), 
            Meff_sd = sd(Meff)/sqrt(n), 
            max_mobility_change_sd = sd(max_mobility_change)/sqrt(n), 
            start_date_sd = sd(start_date)/sqrt(n),
            rel_mob_min_date_sd = sd(rel_mob_min_date)/sqrt(n)) %>%
  gather(metric, sd, -cluster, -n)

summary_plot$sd <- summary_sd$sd

b <- ggplot(summary_plot, aes(x = cluster, y = value, fill = factor(cluster))) +
  geom_bar(stat = "identity") +
  facet_wrap(~as.factor(metric), scales = "free") +
  geom_errorbar(aes(ymin = value - 1.96 * sd, ymax = value + 1.96 * sd), width = 0.2) +
  theme_bw()
b

clust + b

# Radar Plots
min <- c(2, 0.25, 2, 0.25, 0)
max <- c(3.1, 2, 6.5, 1, 90)

radar_1 <- summary[1, c("R0_mean", "Rt_min_mean", "Meff_mean", "max_mobility_change_mean", "rel_mob_min_date")]
colnames(radar_1) <- c("R0" , "Rt_min" , "Meff" , "mob_red" , "date")
radar_1 <- rbind(max, min, radar_1)

radar_2 <- summary[2, c("R0_mean", "Rt_min_mean", "Meff_mean", "max_mobility_change_mean", "rel_mob_min_date")]
colnames(radar_2) <- c("R0" , "Rt_min" , "Meff" , "mob_red" , "date")
radar_2 <- rbind(max, min, radar_2)

radar_3 <- summary[3, c("R0_mean", "Rt_min_mean", "Meff_mean", "max_mobility_change_mean", "rel_mob_min_date")]
colnames(radar_3) <- c("R0" , "Rt_min" , "Meff" , "mob_red" , "date")
radar_3 <- rbind(max, min, radar_3)

radar_4 <- summary[4, c("R0_mean", "Rt_min_mean", "Meff_mean", "max_mobility_change_mean", "rel_mob_min_date")]
colnames(radar_4) <- c("R0" , "Rt_min" , "Meff" , "mob_red" , "date")
radar_4 <- rbind(max, min, radar_4)

radar_5 <- summary[5, c("R0_mean", "Rt_min_mean", "Meff_mean", "max_mobility_change_mean", "rel_mob_min_date")]
colnames(radar_5) <- c("R0" , "Rt_min" , "Meff" , "mob_red" , "date")
radar_5 <- rbind(max, min, radar_5)

radar_6 <- summary[6, c("R0_mean", "Rt_min_mean", "Meff_mean", "max_mobility_change_mean", "rel_mob_min_date")]
colnames(radar_6) <- c("R0" , "Rt_min" , "Meff" , "mob_red" , "date")
radar_6 <- rbind(max, min, radar_6)


colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9), rgb(249/256, 86/256, 79/256, 0.9), rgb(58/256, 169/256, 98/256, 0.9), rgb(123/256, 30/256, 122/256, 0.9))
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4), rgb(249/256, 86/256, 79/256, 0.4), rgb(58/256, 169/256, 98/256, 0.4), rgb(123/256, 30/256, 122/256, 0.4))

# plot with default options:
par(mfrow = c(2,3), mar=c(1, 1, 1, 1))
radarchart(radar_1 , pcol=colors_border[1], pfcol=colors_in[1], plwd=4 , plty=1,
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), 
           cglwd=0.8, vlcex=0.8)
radarchart(radar_2 , pcol=colors_border[2], pfcol=colors_in[2], plwd=4 , plty=1,
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), 
           cglwd=0.8, vlcex=0.8)
radarchart(radar_3 , pcol=colors_border[3], pfcol=colors_in[3], plwd=4 , plty=1,
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), 
           cglwd=0.8, vlcex=0.8)
radarchart(radar_4 , pcol=colors_border[4], pfcol=colors_in[4], plwd=4 , plty=1,
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), 
           cglwd=0.8, vlcex=0.8)
radarchart(radar_5 , pcol=colors_border[5], pfcol=colors_in[5], plwd=4 , plty=1,
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), 
           cglwd=0.8, vlcex=0.8)
radarchart(radar_6, pcol=colors_border[6], pfcol=colors_in[6], plwd=4 , plty=1,
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), 
           cglwd=0.8, vlcex=0.8)
