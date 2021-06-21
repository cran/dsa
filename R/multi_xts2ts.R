#' Change multiple xts to a multivariate ts
#'
#' Change multiple xts to a multivariate ts
#' @param x xts time series
#' @param short Is series too short for xts2ts to work?
#' @author Daniel Ollech
#' @details If the ts are used for forecasting
#' @examples x <- dsa::daily_sim()$original
#' y <- dsa::daily_sim()$original
#' multi_xts2ts(merge(x,y))
#' @export

multi_xts2ts <- function(x, short=FALSE) {
  if (nrow(x) < 366*2 & !short) short=TRUE
  if (short) {
    x[format(zoo::index(x), "%m-%d") == "02-29"] <- NA
    out <- stats::ts(stats::na.omit(x), frequency=365)
  } else{
    ref <- dsa::xts2ts(x[,1])
    a <- lapply(1:ncol(x), function(k) as.numeric(dsa::xts2ts(x[,k], 365)))
    out <- as.data.frame(a)
    colnames(out) <- colnames(x)
    out <- stats::ts(out, start=stats::start(ref), frequency=stats::frequency(ref))}
  return(out)
}
