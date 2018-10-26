#' Change ts to xts
#' 
#' Change the format of a time series from ts to xts. Has been optimised for the use in dsa(), i.e. for daily time series. 
#' @param x_ts ts series to be changed to xts
#' @author Daniel Ollech
#' @examples ts2xts(stats::ts(rnorm(1000, 10,1), start=c(2001,1), freq=365))
#' @details This function is used internally in dsa(). Does not create values for the 29th of February.
#' @export

ts2xts <- function(x_ts) {

day_number <- ifelse(trunc(zoo::index(x_ts))%%4==0 & ((zoo::index(x_ts)-trunc(zoo::index(x_ts)))*365)+1>59.5, ((zoo::index(x_ts)-trunc(zoo::index(x_ts)))*365)+2, ((zoo::index(x_ts)-trunc(zoo::index(x_ts)))*365)+1) 

cal <- as.Date(paste(as.character(trunc(zoo::index(x_ts))), as.character(round(day_number, digits=0))), format="%Y %j")
# cal <- na.approx(as.Date(cal))
xts_out <- xts::xts(x_ts, order.by=cal) 
xts_out
}