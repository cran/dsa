#' Change a daily to a weekly differenced time series
#' 
#' This function computes the weekly aggregates or differences (by default Friday to Friday) for any daily time series in the xts format.
#' @param x input series
#' @param incl_forecast whether the series contains a forecast that shall be omitted
#' @param forecast_length length of forecast
#' @param diff should series be differenced
#' @param dayofweek which day of the week (friday=5)
#' @author Daniel Ollech
#' @examples to_weekly(xts::xts(rnorm(365, 10,1), seq.Date(as.Date("2010-01-01"), length.out=365, by="days")))
#' @export

to_weekly <- function(x, incl_forecast=T, forecast_length=365, diff=T, dayofweek=5) {
if(!any(class(x)=="xts")) {stop("x needs to be of class xts")}

a <- x[format(as.POSIXlt(zoo::index(x), format = "%m/%d/%Y", origin = as.Date("1970-01-01 00:00.00 UTC")), 
                                             "%w") == as.character(dayofweek)]
  if (!incl_forecast) {
TheDate <- xts::last(as.Date(zoo::index(x), format = "%m/%d/%Y", origin = as.Date("1970-01-01 00:00.00 UTC"))) - 
      as.numeric(forecast_length)

      a <- a[paste("/", TheDate, sep = "")]
}

if (diff) {data_comp <- diff(a)} else {data_comp <- a}    
return(data_comp)
}