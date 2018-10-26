#' Adding xts together
#' 
#' Adding xts together while treating NAs as zeros.
#' @param x,y Input time series
#' @param ... further time series to be added
#' @author Daniel Ollech
#' @details Sometimes, if a xts contains missing values, the behaviour of the usual addition-function is not ideal, 
#' at least for the purposes of seasonal adjustment of daily time series. This function changes the behaviour.
#' @examples series1 <- xts::xts(rnorm(5, 5, 5), seq.Date(from=as.Date("2010-01-01"), length.out=5, by="days"))
#' series2 <- xts::xts(c(3,4,NA, 6,7), seq.Date(from=as.Date("2010-01-01"), length.out=5, by="days"))
#' Add(series1, series2)
#' # Compare this to:
#' series1 + series2
#' @export

Add <- function(x, y, ...) {
  x[is.na(x)] <- 0
  y[is.na(y)] <- 0
  
  dots <- list(...)
  
  for (j in dots){
    a <- j
    a[is.na(a)] <- 0
    x <- x+a
  }
  
  return(x+y)
}

