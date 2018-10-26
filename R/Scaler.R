#' Take logs and differences of  a time series
#' 
#' Logarithmise and / or difference a time series 
#' @param x time series
#' @param Diff number of differences to be taken
#' @param Sdiff number of seasonal differences to be taken
#' @param Log Should time series be logarithmised
#' @details Function is used in dsa to let the user decide whether logs and differences should be taken.
#' @author Daniel Ollech
#' @examples a = ts(rnorm(100, 100, 10), start=c(2015,1), frequency=12)
#' Scaler(a, Diff=1, Log=TRUE)
#' @export



Scaler <- function(x, Diff=0, Sdiff=0, Log=FALSE) {
  if (Log) x=log(x)
  if (Diff>0) x = diff(x, differences=Diff) 
  if (Sdiff>0) x = diff(x, lag=12, differences=Sdiff)
  return(stats::na.omit(x))
}






