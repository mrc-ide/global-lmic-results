## 01a Boosted Regression Tree

library(globallmicresults)
library(tidyverse)

##  ------------------------------
## Analysis ------------------------------
##  ------------------------------

# load the correct brt model
brt_model <- brt_get(date_0)

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
a <- ggplot(overall, aes(x = overall, y = prediction, col = income_group)) +
  geom_point(size = 3) +
  scale_colour_manual(labels = c("Low Income", "Lower Middle Income", "Upper Middle Income", "High Income"),
                      values = c("#C6E9F2", "#A5D6EF", "#5C97BC", "#29547A")) +
  theme_bw() +
  ylim(c(-95, 15)) +
  xlim(c(-95, 25)) +
  labs(x = "Observed Mobility Change (%)", y = "Predicted Mobility Change (%)", colour = "") +
  theme(legend.position = "bottom", axis.title.x = element_text(vjust = -2, size = 12),
        axis.title.y = element_text(vjust = +4, size = 12), 
        legend.text = element_text(size = 12), axis.text = element_text(size = 11),
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")) +
  guides(col=guide_legend(ncol=2)) +
  geom_abline(intercept = 0, slope = 1, color = "black", 
              linetype = "dashed", size = 1)
