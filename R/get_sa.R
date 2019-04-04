#' Get Seasonally Adjusted Series 
#' 
#' Get the calendar- and seasonally adjusted series from a seasonal adjustment object created by the dsa function
#' @param daily.object Output from dsa
#' @param forecast Include forecast of component
#' @author Daniel Ollech
#' @examples x = daily_sim(n=4)$original # series with length 4 years
#' res <- dsa(x, cval=7, model=c(3,1,0),fourier_number = 13, reg.create=NULL) 
#' get_sa(res)
#' @seealso get_trend, get_original
#' @export

get_sa <- function(daily.object, forecast=FALSE) {
  sa_out <- daily.object$output[,1]
  
  if (!forecast) {
    days <- daily.object$info[3]
    end_date <- stats::end(daily.object$output[,1])-as.numeric(days)
    sa_out <- sa_out[paste0("/", end_date)]
  }
  
  return(sa_out)
}