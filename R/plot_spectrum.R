#' Plot the periodogram of a daily time series
#' 
#' Plot the periodogram of a daily time series
#' @param x xts or ts, daily timeseries
#' @param xlog should x-axis be log transformed
#' @param size linesize
#' @details Plot uses ggplot2 and can be changed accordingly. The spectrum is build around the spec.pgram() function
#' @author Daniel Ollech
#' @examples x <- daily_sim(3)$original
#' plot_spectrum(x)
#' @export

plot_spectrum <- function(x, xlog=FALSE, size=1) {
  
  if (any(class(x)=="ts")) {
    if (stats::frequency(x) != 365) warning("Time series should be a daily time series")
    x <- ts2xts(x)}
  
title <- ifelse(xlog, "Differenced original series, x-axis=log", "Differenced series")  
axistransform <- ifelse(xlog, "xy", "y")
  
  df <- data.frame(spectrum=(stats::spec.pgram(diff(xts2ts(x)), plot=F)$spec), freq=stats::spec.pgram(diff(xts2ts(x)), plot=F)$freq)
  plot1 <- ggplot2::ggplot(df, ggplot2::aes(x=df$freq, y=df$spectrum)) + ggplot2::geom_line(size=size) +  ggplot2::scale_y_continuous(trans='log10') + ggplot2::geom_vline(ggplot2::aes(xintercept=12), colour="#6F87B2", linetype="dotted") +ggplot2::geom_vline(ggplot2::aes(xintercept=24), colour="#6F87B2", linetype="dotted")  +ggplot2::geom_vline(ggplot2::aes(xintercept=365/7), colour="#6F87B2", linetype="dotted") +ggplot2::geom_vline(ggplot2::aes(xintercept=365/7*2), colour="#6F87B2", linetype="dotted") + ggplot2::geom_vline(ggplot2::aes(xintercept=365/7*3), colour="#6F87B2", linetype="dotted") + ggplot2::theme_bw() + ggplot2::theme(panel.grid = ggplot2::element_blank()) + ggplot2::ggtitle(title) + ggplot2::xlab("Cycles per year") + ggplot2::ylab("")
  
  
  if (xlog) {
    plot1 <- plot1 + ggplot2::scale_x_continuous(trans='log10') + ggplot2::geom_vline(ggplot2::aes(xintercept=1), colour="#3AA625", linetype="dotted") + ggplot2::geom_vline(ggplot2::aes(xintercept=2), colour="#3AA625", linetype="dotted") + ggplot2::geom_vline(ggplot2::aes(xintercept=3), colour="#3AA625", linetype="dotted") + ggplot2::geom_vline(ggplot2::aes(xintercept=4), colour="#3AA625", linetype="dotted") + ggplot2::geom_vline(ggplot2::aes(xintercept=5), colour="#3AA625", linetype="dotted") + ggplot2::geom_vline(ggplot2::aes(xintercept=6), colour="#3AA625", linetype="dotted") + ggplot2::geom_vline(ggplot2::aes(xintercept=7), colour="#3AA625", linetype="dotted") + ggplot2::geom_vline(ggplot2::aes(xintercept=8), colour="#3AA625", linetype="dotted") + ggplot2::geom_vline(ggplot2::aes(xintercept=9), colour="#3AA625", linetype="dotted") + ggplot2::geom_vline(ggplot2::aes(xintercept=10), colour="#3AA625", linetype="dotted")  
  }
  
  
  return(plot1)
}



