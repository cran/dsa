#' Extending a daily time series to having 31 days each month.
#' 
#' This function extends a time series to have 31 days.
#' @param x_ts Time series that will be extended to 31 days each month.
#' @param fill Method that is used to fill up time series. "locf": last observation carried forward, "lin": linear interpolation, "spline": spline interpolation.
#' @param to_ts Boolean. Determines format of the output series. Either ts or xts.
#' @author Daniel Ollech
#' @examples x<-xts::xts(rnorm(1095, 100,1), seq.Date(as.Date("2009-01-01"), length.out=1095, by="days"))
#' a31 <- fill31(x)
#' a <- drop31(a31, 1, 365)
#' @details This function is used internally in dsa()
#' @export


fill31  <- function(x_ts, fill="locf", to_ts=TRUE) {
a <- paste(expand.grid(1:31, 1:12, format(stats::start(x_ts), "%Y"):format(stats::end(x_ts), "%Y"))$Var3, ifelse(expand.grid(01:31, 1:12, format(stats::start(x_ts), "%Y"):format(stats::end(x_ts), "%Y"))$Var2<9.5, paste("0", expand.grid(01:31, 1:12, format(stats::start(x_ts), "%Y"):format(stats::end(x_ts), "%Y"))$Var2, sep=""),expand.grid(01:31, 1:12, format(stats::start(x_ts), "%Y"):format(stats::end(x_ts), "%Y"))$Var2), ifelse(expand.grid(01:31, 1:12, format(stats::start(x_ts), "%Y"):format(stats::end(x_ts), "%Y"))$Var1<9.5, paste("0", expand.grid(01:31, 1:12, format(stats::start(x_ts), "%Y"):format(stats::end(x_ts), "%Y"))$Var1, sep=""),expand.grid(01:31, 1:12, format(stats::start(x_ts), "%Y"):format(stats::end(x_ts), "%Y"))$Var1) , sep="-")

a <- gsub("02-29", "02-28", a)

fill_date  <- a[grep(stats::start(x_ts), a):grep(stats::end(x_ts), a)]
x_ts <- x_ts; 

long_xts <- xts::xts(rep(NA, length(fill_date)), order.by=zoo::na.locf(as.Date(fill_date)))

if (fill=="locf") {
for (j in 1:length(long_xts)){
long_xts[j] <- x_ts[zoo::index(x_ts)==zoo::index(long_xts)[j]]
}}

if (fill=="lin"){
for (j in 1:length(long_xts)){
long_xts[j] <- x_ts[zoo::index(x_ts)==zoo::index(long_xts)[j]]
}

for (k in 2:length(long_xts)){
if (zoo::index(long_xts)[k-1]==zoo::index(long_xts)[k]) {long_xts[k] <- NA}
}

a <- xts::xts(long_xts, seq.Date(from=xts::first(zoo::index(long_xts)), by="days", length.out=length(long_xts)))
long_xts <- xts::xts(zoo::na.approx(a), zoo::index(long_xts))

}

if (fill=="spline"){
for (j in 1:length(long_xts)){
long_xts[j] <- x_ts[zoo::index(x_ts)==zoo::index(long_xts)[j]]
}  

for (k in 2:length(long_xts)){
if (zoo::index(long_xts)[k-1]==zoo::index(long_xts)[k]) {long_xts[k] <- NA}
} 

a <- xts::xts(long_xts, seq.Date(from=xts::first(zoo::index(long_xts)), by="days", length.out=length(long_xts)))
long_xts <- xts::xts(zoo::na.spline(a), zoo::index(long_xts))

}


if (to_ts) {
stats::ts(as.numeric(long_xts), start=c(as.integer(format(xts::first(zoo::index(x_ts)), "%Y")), (as.integer(format(zoo::index(x_ts[1]), "%m"))-1)*31+as.integer(format(zoo::index(x_ts[1]), "%d"))), freq=372)
} else {long_xts}

}


