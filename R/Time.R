#' Creating Several Holiday dummy 
#' 
#' This function uses the Holiday dates of the timeDate package to create several dummies on a specified holiday.
#' @param from Relative to Holiday, starting date
#' @param to Relative to Holiday, end date
#' @param dates Which Holidays shall be used
#' @details With shift the user can specify for how many days before (negative value) or after (positive value) the holiday a dummy will be created.
#' @author Daniel Ollech
#' @examples \dontrun{output(Time(from=5, to=10, dates=timeDate::Easter(2000:2030))}
#' @export

Time <- function(from=-10, to=10, dates=timeDate::Easter(2000:2030)) {
  
  Holiday <- function(dates="", shift=0) {
    Holiday_dates <- as.Date(dates)+shift
    
    vec <- rep(1, times=length(Holiday_dates))
    Holiday_ones <- xts::xts(vec, order.by=Holiday_dates)
    
    # Version von xts2ts die jedoch eine Zeitreihe erstellt, die am 1 Januar des Startjahres beginnt und am 31. Dezember des Schlussjahres endet. Bei der Originalen Version von xts2ts wird Startdatum und Enddatum+1 Tag der Inputreihe verwendet.
    xts2ts <- function(series) {
      newTS <-  stats::ts(NA, start=c(as.integer(format(zoo::index(series)[1], "%Y")), 1), end=c(as.integer(format(zoo::index(series)[length(series)], "%Y")), 365),   frequency=365)
      
      delta <- rep(-99, times=length(series))
      
      for (t in 1:length(series)) {
        if (as.integer(format(zoo::index(series)[t], "%Y")) %% 4==0 && timeDate::dayOfYear(timeDate::timeDate(zoo::index(series)[t], FinCenter = "EET")) < 60)
        {delta[t] <- round(as.integer(format(zoo::index(series)[t], "%Y")) + (timeDate::dayOfYear(timeDate::timeDate(zoo::index(series)[t], FinCenter = "CET"))-1)/365, digits=3)  }
        
        if (as.integer(format(zoo::index(series)[t], "%Y")) %% 4==0 && timeDate::dayOfYear(timeDate::timeDate(zoo::index(series)[t], FinCenter = "EET")) > 60)
        {delta[t] <- round(as.integer(format(zoo::index(series)[t], "%Y")) + (timeDate::dayOfYear(timeDate::timeDate(zoo::index(series)[t], FinCenter = "CET"))-2)/365, digits=3)  }   
        
        if (as.integer(format(zoo::index(series)[t], "%Y")) %% 4!=0 )
        {delta[t] <- round(as.integer(format(zoo::index(series)[t], "%Y")) + (timeDate::dayOfYear(timeDate::timeDate(zoo::index(series)[t], FinCenter = "EET"))-1)/365, digits=3)  }
      }  
      
      for (m in 1:length(newTS)) {
        index <- round(zoo::index(newTS)[m], digits=3)
        if(sum(as.numeric(delta==index))==1) {
          newTS[m] <- series[delta == index]
        } }
      newTS
    }
    
    Holiday_dummy <- xts2ts(Holiday_ones)
    Holiday_dummy[is.na(Holiday_dummy)] <- 0 
    
    Holiday_dummy*shift
  }
  
  b <- lapply(a<-from:to, function(x) Holiday(shift=x, dates=dates))
  d <- b[[1]]
  
  if (length(b)>1)
    for (j in 2:length(b)) {
      d <- d + b[[j]]
    }
  
  d

}