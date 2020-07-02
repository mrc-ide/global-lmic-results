## 02a Inferred Deaths Averted

library(globallmicresults)
library(tidyverse)
library(lubridate)
library(rgdal)
library(raster)
library(viridis)
library(plyr)
library(fields)
library(squire)
library(rmapshaper)
library(rgeos)
# library(ggpubr)
# library(broom)
# library(data.table)
# 
# ##  ------------------------------
# ## Analysis ------------------------------
# ##  ------------------------------
# 
# date_0 <- "2020-06-01"
# 
# # get the reports
# grids <- grid_out_list(date_0)
# 
# # function to run an unmitigated simulation using calibrate object (x)
# <<<<<<< HEAD
# unmit_sim_i <- function(x, i, date_end) {
# =======
# unmit_sim_i <- function(x, i, modifier = 1) {
# 
#   R0_use <- if(i == "mean") mean(x$replicate_parameters$R) else x$replicate_parameters$R[i]
#   R0_use <- R0_use * modifier
# 
#   start_date_use <- if(i == "mean") mean(x$replicate_parameters$start_date) else x$replicate_parameters$start_date[i]
# >>>>>>> 05882d2f1662b5e95ce7588310f990c4de210026
# 
#   run <- squire::run_explicit_SEEIR_model(
#     country = x$parameters$country,
#     R0 = R0_use,
#     day_return = TRUE,
#     replicates = 1,
#     dt = 0.1,
# <<<<<<< HEAD
#     time_period = as.integer(as.Date(date_end) - as.Date(x$replicate_parameters$start_date[i]))
# =======
#     time_period = as.integer(as.Date(date_0) - as.Date(start_date_use))
# >>>>>>> 05882d2f1662b5e95ce7588310f990c4de210026
#   )
#   run
# }
# 
# # function to run multiple unmitigated simulations based on replicate_parameters
# <<<<<<< HEAD
# unmit_sim <- function(x, date_end) {
# 
#   # run the first one
#   r <- unmit_sim_i(x, 1, date_end)
# =======
# unmit_sim <- function(x, modifier) {
# 
#   # run the first one
#   r <- unmit_sim_i(x, "mean", modifier) # this was set to 1, there's lots of different values here - do we not want to either cycle through all of them or take the mean R0 of everything?
# >>>>>>> 05882d2f1662b5e95ce7588310f990c4de210026
# 
#   # assign to our results
#   out <- list()
#   out[[1]] <- r
# 
#   # what is the mix mat
#   mat <- squire:::process_contact_matrix_scaled_age(r$parameters$contact_matrix_set[[1]],
#                                                     r$parameters$population)
# 
#   # running and storing the model output for each of the different initial seeding cases
#   for(i in 2:nrow(x$replicate_parameters)) {
# 
#     beta <- squire:::beta_est_explicit(dur_IMild = r$parameters$dur_IMild,
#                                        dur_ICase = r$parameters$dur_ICase,
#                                        prob_hosp = r$parameters$prob_hosp,
#                                        mixing_matrix = mat,
#                                        R0 = x$replicate_parameters$R0[i])
#     r$model$set_user(beta_set = beta)
# 
#     time_period <- as.integer(as.Date(date_end) - as.Date(x$replicate_parameters$start_date[i]))
#     t <- seq(from = 1, to = time_period/x$parameters$dt)
#     t <- round(seq(1/x$parameters$dt, length(t) + (1/x$parameters$dt), by = 1/x$parameters$dt))
#     r$output <- r$model$run(t, replicate = 1)
#     out[[i]] <- r
#   }
# 
#   # different lengths of sims
#   num_rows <- unlist(lapply(out, function(x){nrow(x$output)}))
#   max_rows <- max(num_rows)
#   seq_max <- seq_len(max_rows)
# 
#   # build results again
#   outarray <- array(NA, dim = c(max_rows, ncol(out[[1]]$output), length(out)))
# 
#   # assign the names
#   colnames(outarray) <- colnames(x$output)
#   rownames(outarray) <- rownames(x$output)
# 
#   # fill it in
#   for(i in seq_len(length(out))){
#     outarray[tail(seq_max, num_rows[i]), ,i] <- out[[i]]$output[, , 1]
#     outarray[, "time", i] <- outarray[, "time", i] - max(outarray[, "time", i], na.rm = TRUE)
#   }
# 
#   r$output <- outarray
#   r$parameters$replicates <- length(out)
#   r
# }
# 
# ## Now we want to run an unmitgated epidemic (as well as an x% reduction epidemic
# ## where x is aiming to show what a country's transmission would have likely done
# ## in the absence of any formal intervention but just due to global societal awareness
# ## of covid-19. Perhaps 40%, which is the approximate mean max reduction in mobility seen in all countries
# 
# # We would do the above for each country presenting deaths averted to date.
# 
# # Here is an example for one country. So repeat and save as a table for continent level
# # and one for country. And then some figures as you see best to summarise this
# all_scenarios_go <- do.call(rbind, sapply(c(1, 0.2, 0.4), function(y){
# 
#   go_all_data <- do.call(rbind, sapply(names(grids), function(t){
# 
#     print(paste0(y, " - ", t))
# 
#     unmit_country <- unmit_sim(x = grids[[which(names(grids) == t)]],
#                                modifier = y)
# 
#     total_deaths_unmit <- squire::format_output(unmit_country, "deaths")
#     total_deaths_unmit_summary <- total_deaths_unmit %>%
#       filter(!is.na(y)) %>%
#       group_by(replicate) %>%
#       mutate(cy = cumsum(y)) %>%
#       ungroup %>%
#       group_by(t) %>%
#       dplyr::summarise(deaths_025 = quantile(cy, 0.025),
#                        mean_deaths = mean(cy),
#                        deaths_975 = quantile(cy, 0.975))
# 
#     res_df <- total_deaths_unmit_summary[nrow(total_deaths_unmit_summary), 2:4] -
#       sum(grids[[t]]$scan_results$inputs$data$deaths)
#     res_df$ISO <- t
# 
# <<<<<<< HEAD
# x <- unmit_sim(grids$AFG, "2020-06-01")
# 
# deaths_unmit <- squire::format_output(x, "deaths")
# 
# daily_deaths_unmit_summary <- deaths_unmit %>%
#   filter(!is.na(y)) %>%
#   group_by(t) %>%
#   summarise(deaths_025 = quantile(y, 0.025),
#             mean_deaths = mean(y),
#             deaths_975 = quantile(y, 0.975))
# plot(daily_deaths_unmit_summary$t, daily_deaths_unmit_summary$mean_deaths, type = "l", lwd = 2)
# lines(daily_deaths_unmit_summary$t, daily_deaths_unmit_summary$deaths_025)
# lines(daily_deaths_unmit_summary$t, daily_deaths_unmit_summary$deaths_975)
# 
# total_deaths_unmit_summary <- deaths_unmit %>%
#   filter(!is.na(y)) %>%
#   group_by(replicate) %>%
#   mutate(cy = cumsum(y)) %>%
#   ungroup %>%
#   group_by(t) %>%
#   summarise(deaths_025 = quantile(cy, 0.025),
#             mean_deaths = mean(cy),
#             deaths_975 = quantile(cy, 0.975))
# 
# plot(total_deaths_unmit_summary$t, total_deaths_unmit_summary$mean_deaths)
# 
# 
# res_df <- total_deaths_unmit_summary[nrow(total_deaths_unmit_summary), 2:4] -
#   sum(grids$AFG$scan_results$inputs$data$deaths)
# =======
#     res_df
# 
#   }, simplify = FALSE))
# 
#   go_all_data$R0_modifier <- y
#   go_all_data
# 
# }, simplify = FALSE))
# >>>>>>> 05882d2f1662b5e95ce7588310f990c4de210026
# 
# 
# ##  ------------------------------
# ## Plotting ------------------------------
# ##  ------------------------------
# 
# 
# global_shp <- readOGR("analysis/data/shp/global_shapefile_small_islands_removed.shp.shp", stringsAsFactors = FALSE)
# global_shp_df <- tidy(global_shp, region = "GID_0") %>% data.table()
# colnames(global_shp_df)[which(colnames(global_shp_df) == "id")] <- "ISO"
# 
# global_shp_simp_df_data <- join(global_shp_df, as.data.frame(global_shp_all_data_simp), by = "GID_0")
# 
# 
# 
# 
