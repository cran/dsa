#' Creating Holiday dummy 
#' 
#' This function uses the Holiday dates of the timeDate::timeDate package to create dummies on a specified holiday.
#' @param dates Holiday and period for which dummy shall be created
#' @param shift shifting point in time for dummy
#' @details With shift the user can specify for how many days before (negative value) or after (positive value) the holiday a dummy will be created.
#' @author Daniel Ollech
#' @examples Holiday(dates=timeDate::Easter(2000:2030), shift=-1)
#' @export

Holiday <- function(dates=timeDate::Easter(2000:2030), shift=0) {
  Holiday_dates <- timeDate::as.Date.timeDate(dates)+shift
  
  vec <- rep(1, times=length(Holiday_dates))
  Holiday_ones <- xts::xts(vec, order.by=Holiday_dates); 
  
  
  # Version of xts2ts that starts on January 1st of the start year and end December 31st in the last year (instead of the original series start and end date)
  xts2ts <- function(series) {
    newTS <-  stats::ts(NA, start=c(as.integer(format(zoo::index(series)[1], "%Y")), 1), end=c(as.integer(format(zoo::index(series)[length(series)], "%Y")), 365), frequency=365)

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
      }
    }
    
    
    newTS
    
  }
  
  Holiday_dummy <- xts2ts(Holiday_ones)
  
  Holiday_dummy[is.na(Holiday_dummy)] <- 0 
  
  Holiday_dummy
}