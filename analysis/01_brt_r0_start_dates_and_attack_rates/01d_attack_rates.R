## 01d Attack Rate Distribution

library(globallmicresults)
library(tidyverse)

##  ------------------------------
## Analysis ------------------------------
##  ------------------------------

# get the reports
reports <- reports_day(date_0)

# get attack rate data
attack_rates <- pbapply::pblapply(seq_along(reports$id), function(x) {

  fs <- file.path(here::here(),
                  "analysis/data/raw_data/server_results/archive/lmic_reports_google",
                  reports$id[x],
                  "grid_out.rds")

  out <- readRDS(fs)
  index <- squire:::odin_index(out$model)
  ars <- vapply(seq_len(dim(out$output)[3]), function(i) {
    infected <- sum(out$output[nrow(out$output), unlist(index[3:27]), i])
    pop <- sum(out$output[nrow(out$output), unlist(index[2:27]), i])
    return(infected/pop)
  }, numeric(1))

  #ar <- as.data.frame(as.list(summary(ars)))
  ar <- data.frame("ar" = ars, "rep" = seq_along(ars), "iso" = reports$country[x])

  return(ar)

})
attack_rates <- do.call(rbind, attack_rates)
attack_rates$continent <- countrycode::countrycode(attack_rates$iso, "iso3c", "continent")

##  ------------------------------
## Plotting ------------------------------
##  ------------------------------

## Plot to show continental range in attack rates
ar_plot <- ggplot(attack_rates, aes(x=ar, fill=continent)) +
  geom_density(alpha=0.5) +
  theme_bw() +
  xlab("Attack Rate") +
  ylab("") +
  scale_x_log10(labels = scales::percent_format(accuracy = 0.001, ), limits = c(NA,0.50)) +
  scale_fill_discrete(name = "Continent") +
  theme(
    legend.position = c(0.05, 0.7),
    legend.justification = c(0, 0)
    )

# check size before saving
x11(width = 6,height = 6)
ar_plot
cowplot::save_plot(plot = ar_plot,
                   filename = file.path(here::here(),"analysis/figures/01d_attack_rates.png"),
                   base_height = 6,
                   base_width = 6)


ar_plot_ridge <- attack_rates %>%
  mutate(iso = fct_reorder(.f = iso, .x = ar, .fun = "mean")) %>%
  ggplot(aes(x=ar, y = iso, fill=iso)) +
  ggridges::geom_density_ridges(alpha=0.5, scale = 1, rel_min_height = 0.00001) +
  theme_bw() +
  xlab("Attack Rate") +
  ylab("") +
  scale_x_log10(labels = scales::percent_format(accuracy = 0.001), limits = c(NA,0.99)) +
  scale_fill_discrete(name = "Continent") +
  facet_wrap(~continent, scales = "free_y") +
  theme(
    legend.position = "none"
  )

# check size before saving
x11(width = 8,height = 12)
ar_plot_ridge
cowplot::save_plot(plot = ar_plot_ridge,
                   filename = file.path(here::here(),"analysis/figures/01d_attack_rates_country.png"),
                   base_height = 12,
                   base_width = 8)
