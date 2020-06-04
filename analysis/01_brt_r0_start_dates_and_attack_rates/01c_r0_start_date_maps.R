## 01c Plotting Maps of Start Dates and R0

library(globallmicresults)
library(tidyverse)

##  ------------------------------
## Analysis ------------------------------
##  ------------------------------

# get the reports
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

params <- do.call(rbind, param_sums)


##  ------------------------------
## Plotting ------------------------------
##  ------------------------------

## Plot to show the global range in R0 and start date
