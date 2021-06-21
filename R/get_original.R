#' Get Original Time Series
#'
#' Get the original time series from a seasonal adjustment object created by the dsa function. Can deviate from the input data as missings are filled up, usually using zoo::na.locf().
#' @param daily.object Output from dsa
#' @param forecast Include forecast of component
#' @author Daniel Ollech
#' @examples set.seed(123)
#' x = daily_sim(n=4)$original # series with length 4 years
#' res <- dsa(x, cval=7, model=c(3,1,0),fourier_number = 13)
#' get_original(res)
#' @seealso get_sa, get_trend
#' @export

get_original <- function(daily.object, forecast=FALSE) {
  o_out <- daily.object$output[,2]

  if (!forecast) {
    days <- daily.object$info[3]
    end_date <- stats::end(daily.object$output[,2])-as.numeric(days)
    o_out <- o_out[paste0("/", end_date)]
  }

  return(o_out)
}
