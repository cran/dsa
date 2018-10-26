#' Dummy for the Day of the Year
#' 
#' Creates dummies for each chosen day of the year
#' @param day Day of the year for which dummy is created
#' @param start Startdate
#' @param end Enddate
#' @author Daniel Ollech
#' @details This function is used in dsa() to create day of the year dummies.
#' @examples plot(doy_dummy())
#' @export

doy_dummy <- function(day="1", start="2010/1/1", end="2015/01/01") {
  if (length(start)==2) {
    start <- as.Date(start[2] - 1, origin = paste(start[1], "-01-01", sep=""))
  }
  
  if (length(end)==2) {
    end <- as.Date(end[2] - 1, origin = paste(end[1], "-01-01", sep=""))
  }
  
  date <- seq.Date(from=as.Date(start), to=as.Date(end), by="days")
  day <- as.integer(day)
  day2 <- ifelse(day<10, paste("00", as.character(day), sep=""), ifelse(day<100, paste("0", as.character(day), sep=""), as.character(day)))
  
  series <- xts::xts(rep(0, times=length(date)), order.by=date)
  series[format(zoo::index(series), "%j")==day2] <- 1
  series <- stats::ts(series[!is.na(series)], start=c(as.integer(format(as.Date(start), "%Y")), timeDate::dayOfYear(timeDate::timeDate(start, format="%Y/%m/%d"))), frequency=365)
  
  series  
}


