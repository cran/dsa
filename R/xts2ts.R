#' Change xts to ts
#' 
#' Change the format of a time series from xts to ts. Has been optimised for the use in dsa(), i.e. for daily time series. 
#' @param series xts series to be changed to ts
#' @param freq frequency of ts series
#' @author Daniel Ollech
#' @examples xts2ts(xts::xts(rnorm(1095, 10,1), seq.Date(as.Date("2010-01-01"), length.out=1095, by="days")))
#' @details This function is used internally in dsa(). Does not create values for the 29th of February.
#' @export



xts2ts <- function(series, freq=NULL) {
  
  if (is.null(freq)) {freq = freq_xts(series)}  
  
  newTS <- series  
  
  if (freq==365) {
    newTS[format(zoo::index(series), "%m-%d")=="02-29"] <- NA}
  
  time <- sum(as.numeric(format(zoo::index(series), "%Y")==format(xts::first(zoo::index(series)), "%Y")))
  
  .is.leapyear <- function(x) {
    year <- as.numeric(x)
    if((year %% 4) == 0) {
      if((year %% 100) == 0) {
        if((year %% 400) == 0) {
          out <- TRUE
        } else {out <- FALSE}
      } else {out <- TRUE }
    } else {out <- FALSE}
    
    return(out)
  }
  
  if (.is.leapyear(format(xts::first(zoo::index(series)), "%Y"))) {time <- time-1}
  
  newstart <- c(as.numeric(format(xts::first(zoo::index(series)), "%Y")), freq-time+1)
  
  newseries <- as.numeric(newTS[!is.na(newTS)])
  
  outTS <- stats::ts(newseries, start=newstart, frequency=freq)
  
  outTS
  
} 






  
  
  