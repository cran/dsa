#' Fill up NAs
#' 
#' Copy values from series to another to fill up missing values
#' @param fill_up_series Series that has missing values
#' @param use_series Series that is used fo fill up missing values
#' @author Daniel Ollech
#' @details This function is used internally in dsa()
#' @examples a <- b <- daily_sim(n=3)$original
#' a[c(355,376)] <- NA
#' a_new <- fill_up(a, b)
#' all(b==a_new)
#' @export

fill_up <- function(fill_up_series=NA, use_series=NA) {

   time <- as.POSIXlt(seq.Date(from=as.Date(xts::first(zoo::index(fill_up_series))), to=as.Date(xts::last(zoo::index(fill_up_series))), by="days"))
attr(time, "tzone") <- "GMT"; attr(zoo::index(fill_up_series), "tzone") <-"GMT"
Fillx <- xts::xts(rep(NA, times=length(time)), order.by=time)
if (length(Fillx) != length(fill_up_series)) {fill_up_series <- merge(fill_up_series, Fillx)[,1]}
  
  a <-zoo::index(fill_up_series[is.na(fill_up_series)])

if (any(as.Date(a) > as.Date(stats::end(use_series)))) {
for (m in 1:length(a)) {
  if (as.Date(a[m]) > as.Date(stats::end(use_series))) {a[m] <- NA}}
  a <- a[!is.na(a)]
fill_up_series[as.Date(zoo::index(fill_up_series))==as.Date(a)] <- use_series[as.Date(zoo::index(use_series))==as.Date(a)]  
fill_up_series <- zoo::na.spline(fill_up_series)  
} else {
suppressWarnings(fill_up_series[as.Date(zoo::index(fill_up_series))==as.Date(a)] <- use_series[as.Date(zoo::index(use_series))==as.Date(a)])  
}

fill_up_series
  
  
}

