#' Forecasts the days of the week
#' 
#' This function splits a time series into the days of the week and forecasts them using the X-11 heuristic or ETS.
#' @param series Input time series
#' @param use Which method to use. "heur" or "ets".
#' @param h Length of the Forecast
#' @author Daniel Ollech
#' @examples day_split(series=ts(rnorm(730, 100,1), start=c(2001,1), frequency=7), use="ets", h=365)
#' @details This function is used internally in dsa()
#' @export

day_split <- function(series=NULL, use="heur", h=365) {

if (stats::frequency(series)!=7) {stop("This function is intended for working with weekdays. The input has to be a time series with frequency 7")}
  
if (use=="heur") {
for (j in 0:6) {
a <- series[round(zoo::index(series)-trunc(zoo::index(series)), digits=3)==round(j/7, digits=3)]
for (k in 1:53) {
  a <- c(a, (xts::last(a)+(xts::last(a)-xts::last(a, 2)[1])*0.5))  # Umso höher der Faktor (hier 0.5) gesetzt wird, umso mehr ähnelt es in der Tendenz dem Ergebnis von use=="ets.
}
assign(paste("sf", j, sep=""), a )
}}

if (use=="ets") {
  for (j in 0:6) {
assign("a", series[round(zoo::index(series)-trunc(zoo::index(series)), digits=3)==round(j/7, digits=3)])
fit <- forecast::ets(a, model="AAN")
b <- forecast::forecast(fit, h=ceiling(h/7))

assign(paste("sf", j, sep=""), c(b$x, b$mean))
}}

maxx <- max(length(sf0), length(sf1), length(sf2), length(sf3), length(sf4), length(sf5), length(sf6))
if (length(sf0)<maxx) {sf0 <- c(sf0, NA)}
if (length(sf1)<maxx) {sf1 <- c(sf1, NA)}
if (length(sf2)<maxx) {sf2 <- c(sf2, NA)}
if (length(sf3)<maxx) {sf3 <- c(sf3, NA)}
if (length(sf4)<maxx) {sf4 <- c(sf4, NA)}
if (length(sf5)<maxx) {sf5 <- c(sf5, NA)}
if (length(sf6)<maxx) {sf6 <- c(sf6, NA)}

if (xts::first(round(zoo::index(series)-trunc(zoo::index(series)), digits=3))==round(0/7, digits=3)) { sf <- utils::stack(data.frame(t(as.matrix(cbind(sf0, sf1, sf2, sf3, sf4, sf5, sf6)))))$values} 
if (xts::first(round(zoo::index(series)-trunc(zoo::index(series)), digits=3))==round(1/7, digits=3)) { sf <- utils::stack(data.frame(t(as.matrix(cbind(sf1, sf2, sf3, sf4, sf5, sf6, sf0)))))$values} 
if (xts::first(round(zoo::index(series)-trunc(zoo::index(series)), digits=3))==round(2/7, digits=3)) { sf <- utils::stack(data.frame(t(as.matrix(cbind(sf2, sf3, sf4, sf5, sf6, sf0, sf1 )))))$values} 
if (xts::first(round(zoo::index(series)-trunc(zoo::index(series)), digits=3))==round(3/7, digits=3)) { sf <- utils::stack(data.frame(t(as.matrix(cbind(sf3, sf4, sf5, sf6, sf0, sf1, sf2)))))$values} 
if (xts::first(round(zoo::index(series)-trunc(zoo::index(series)), digits=3))==round(4/7, digits=3)) { sf <- utils::stack(data.frame(t(as.matrix(cbind(sf4, sf5, sf6, sf0, sf1, sf2, sf3)))))$values} 
if (xts::first(round(zoo::index(series)-trunc(zoo::index(series)), digits=3))==round(5/7, digits=3)) { sf <- utils::stack(data.frame(t(as.matrix(cbind(sf5, sf6, sf0, sf1, sf2, sf3, sf4)))))$values} 
if (xts::first(round(zoo::index(series)-trunc(zoo::index(series)), digits=3))==round(6/7, digits=3)) { sf <- utils::stack(data.frame(t(as.matrix(cbind(sf6, sf0, sf1, sf2, sf3, sf4, sf5)))))$values} 


sf <- sf[!is.na(sf)]

list(sf=sf, day1=sf0, day2=sf1, day3=sf2, day4=sf3, day5=sf4, day6=sf5, day7=sf6) }




