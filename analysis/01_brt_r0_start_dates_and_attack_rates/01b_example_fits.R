## 01b Example fits using same countries as in 01a

library(globallmicresults)
library(tidyverse)

##  ------------------------------
## Analysis ------------------------------
##  ------------------------------

# load the correct brt model
brt_model <- get_brt_model(date_0)
### need to load the dataframe initially used to train the model 
# called overall, looks like
# overall <- new_ACAPs_cat %>%
#   left_join(mob, by = c("country" = "country_region", "date" = "date")) %>% 
#   filter(!is.na(overall)) %>%
#   left_join(wb_metadata, by = "ISO")


##  ------------------------------
## Plotting ------------------------------
##  ------------------------------
predicted <- predict.gbm(brt_model, x[, c(2:74)], n.trees = brt_model$gbm.call$best.trees, type = "response")
plot(overall$overall, predicted, ylim = c(-100, 20), xlim = c(-100, 20), pch = 20, cex = 2, ylab = "")
par(mfrow = c(5, 5), mar = c(5, 1, 1, 1))
countries <- unique(overall$country)
for (i in 1:length(countries)) {
  indices <- which(overall$country %in% countries[i])
  actual <- overall[overall$country %in% countries[i], ]
  pred <- predicted[indices]
  plot(actual$date, actual$overall, ylim = c(-100, 10), pch = 20, cex = 2, ylab = "", xlab = countries[i])
  lines(actual$date, pred, type = "l", ylim = c(-100, 10), lwd = 2, col = "red")
}

# Plots for the Paper
overall$prediction <- predicted

rep_countries <- overall %>%
  filter(country == "Colombia" | country == "India" | country == "Philippines" | country == "Zimbabwe")
b <- ggplot(rep_countries, aes(x = date, y = overall)) +
  geom_point(size = 2) +
  facet_wrap(~country) +
  geom_line(aes(x = date, y = prediction, col = country), size = 2) +
  scale_colour_manual(values = c("#DBC453", "#DD954D", "#BD7EE2", "#63AD4A")) +
  theme_bw() +
  labs(x = "Date", y = "Mobility Change (%)") +
  theme(legend.position = "none")