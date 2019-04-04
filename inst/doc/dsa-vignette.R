## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(dsa)
set.seed(5)
x = daily_sim(5)$original
res <- dsa(x, cval=7, model=c(3,1,0),fourier_number = 13, reg.create=NULL) 

## ------------------------------------------------------------------------
output(res)

## ---- fig.show='hold'----------------------------------------------------
sa <- res$output[,1]
original <- res$output[,2]
plot(merge(original, sa), main="Results of Daily Seasonal Adjustment")

