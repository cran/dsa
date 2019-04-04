#' Get Trend-Cycle  
#' 
#' Calculate the trend-cycle based on a seasonally adjusted series obtained from a seasonal adjustment object created by the dsa function
#' @param daily.object Output from dsa
#' @param trend_length Number of neighbouring points to use, in days
#' @param forecast Include forecast of component
#' @author Daniel Ollech
#' @examples x = daily_sim(n=4)$original # series with length 4 years
#' res <- dsa(x, cval=7, model=c(3,1,0),fourier_number = 13, reg.create=NULL) 
#' get_trend(res)
#' @details If not odd the parameter trend_length is set to the next highest odd number. 
#' @seealso get_sa, get_original
#' @export

get_trend <- function(daily.object, trend_length=93, forecast=FALSE) {
  sa_out <- daily.object$output[,1]
  
  trend_length <- round(trend_length)
   if (trend_length %% 2 == 0) {trend_length <- trend_length + 1}
  fraction = trend_length / length(sa_out)
  
  trend_out <- xts::xts(stats::predict(stats::loess(sa_out ~ seq.int(length(sa_out)), span=fraction), newdata=seq.int(length(sa_out))), order.by=zoo::index(sa_out))
  

  if (!forecast) {
    days <- daily.object$info[3]
    end_date <- stats::end(trend_out)-as.numeric(days)
    trend_out <- trend_out[paste0("/", end_date)]
  }
  
  names(trend_out) <- "trend"
  
  return(trend_out)
}