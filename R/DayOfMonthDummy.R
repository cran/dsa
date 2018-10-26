#' Dummy for the Day of the Month
#' 
#'  Creates dummies for each chosen day of the week.
#' @param day Day of the Month for which dummy is created
#' @param start Startdate
#' @param end Enddate
#' @param delete29 Should the 29th of February be deleted?
#' @author Daniel Ollech
#' @details This function is used in dsa() to create day of the month dummies.
#' @examples plot(dom_dummy())
#' @export

dom_dummy <- function(day="01", start="2010/1/1", end="2015/01/01",delete29=T) {
if (length(start)==2) {
start <- as.Date(start[2] - 1, origin = paste(start[1], "-01-01", sep=""))
}

if (length(end)==2) {
end <- as.Date(end[2] - 1, origin = paste(end[1], "-01-01", sep=""))
}

date <- seq.Date(from=as.Date(start), to=as.Date(end), by="days")
day <- as.integer(day)
day2 <- ifelse(day<10, paste("0", as.character(day), sep=""), as.character(day))

series <- xts::xts(rep(0, times=length(date)), order.by=date)
series[format(zoo::index(series), "%d")==day2] <- 1
if (delete29) {
  series[format(zoo::index(series), "%m/%d")=="02/29"] <- NA}
series <- stats::ts(series[!is.na(series)], start=c(as.integer(format(as.Date(start), "%Y")), timeDate::dayOfYear(timeDate::timeDate(start, format="%Y/%m/%d"))), frequency=365)

series  
}


