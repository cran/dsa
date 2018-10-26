#' Create a simple, exemplary, seasonal, daily time series 
#' 
#' Create a seasonal daily time series and its seasonal and non-seasonal components
#' @param n length of time series in years
#' @param week_effect increase size of seasonal factor for day-of-the-week
#' @param month_effect increase size of seasonal factor for day-of-the-month
#' @param year_effect increase size of seasonal factor for day-of-the-year
#' @param model ARIMA model for trend and irregular component of series
#' @param ar coefficients for AR terms
#' @param ma coefficients for MA terms
#' @param moving should seasonal factors be moving (=T) or constant (=F)
#' @param week_cycles number of cycles per week
#' @param month_cycles number of cycles per month
#' @param year_cycles number of cycles per year
#' @details The output is an xts time series containing the time series, the true seasonally adjusted series,
#' @details the day-of-the-week seasonal component, the day-of-the-month seasonal component and the
#' @details day-of-the-year seasonal component.
#' @author Daniel Ollech
#' @examples time_series <- daily_sim(n=4, year_effect=3)
#' xtsplot(time_series[,1], font="sans") # Plot of the time series
#' xtsplot(time_series[,3:5], font="sans") # Plot of the seasonal factors
#' @export
daily_sim <- function(n=8, week_effect=1, month_effect=1, year_effect=1, model=c(3,1,1), ar=c(-0.2, 0.5,0.1), ma=-0.4, moving=T, week_cycles=2, month_cycles=3, year_cycles=8) {

  
if (model[3]==0) {ma=NULL}
if (model[1]==0) {ar=NULL}

a <- stats::ts(stats::arima.sim(list(order = model, ar = ar, ma=ma), n = 365.25*n), freq=365.25)
a7 <- stats::ts(a, freq=7)
a31 <- stats::ts(a, freq=30.4)


if (moving) {
bb <- rbind(matrix(matrix(0.7^(1:week_cycles)*1.6, week_cycles, 2, byrow=T), 1, week_cycles*2), matrix(0,length(a7)-1, week_cycles*2))

s7 <- rowSums(forecast::fourier(a7,week_cycles) *apply(bb, 2, f <- function(x) stats::filter(x, filter=stats::rnorm(1,1,0.0001), "recursive")))*week_effect } else {
  s7 <- rowSums(forecast::fourier(a7,week_cycles) * matrix(rep(matrix(matrix(rep(0.7^(1:week_cycles)*1.6, 2), week_cycles, 2, byrow=T), week_cycles*2, 1) + stats::rnorm(week_cycles*2, 0,0.4), length(a7)), nrow=length(a7), ncol=week_cycles*2, byrow=T))*week_effect
}

if (moving) {
  bb <- rbind(matrix(matrix(0.6^(1:month_cycles)*1.6, month_cycles, 2, byrow=T), 1, month_cycles*2), matrix(0,length(a31)-1, month_cycles*2))
  
  s31 <- rowSums(forecast::fourier(a31,month_cycles) *apply(bb, 2, f <- function(x) stats::filter(x, filter=stats::rnorm(1,1,0.00015), "recursive")))*month_effect } else {
    s31 <- rowSums(forecast::fourier(a31,month_cycles) * matrix(rep(matrix(matrix(rep(0.7^(1:month_cycles)*1.6, 2), month_cycles, 2, byrow=T), month_cycles*2, 1) + stats::rnorm(month_cycles*2, 0,0.4), length(a31)), nrow=length(a31), ncol=month_cycles*2, byrow=T))*month_effect
  }

if (moving) {
  bb <- rbind(matrix(matrix(0.9^(1:year_cycles)*1.1, year_cycles, 2, byrow=T), 1, year_cycles*2), matrix(0,length(a)-1, year_cycles*2))
  
  s365 <- rowSums(forecast::fourier(a,year_cycles) *apply(bb, 2, f <- function(x) stats::filter(x, filter=stats::rnorm(1,1,0.00025), "recursive")))*year_effect } else {
    s365 <- rowSums(forecast::fourier(a,year_cycles) * matrix(rep(matrix(matrix(rep(0.6^(1:year_cycles)*3.2, 2), year_cycles, 2, byrow=T), year_cycles*2, 1) + stats::rnorm(year_cycles*2, 0,0.4), length(a)), nrow=length(a), ncol=year_cycles*2, byrow=T))*year_effect
  }

original <- xts::xts(a + 100 + s7 + s31 + s365 , order.by=seq.Date(from=as.Date("2005-01-01"), length.out=length(a), by="days") )
sa <- xts::xts(a+100, zoo::index(original))
seasonal <- merge(s7=xts::xts(s7, zoo::index(original)), s31=xts::xts(s31, zoo::index(original)), s365=xts::xts(s365, zoo::index(original)))

return(merge(original, sa, seasonal))

}
