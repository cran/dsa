#' Plot the periodogram of a daily time series
#'
#' Plot the periodogram of a daily time series
#' @param x xts or ts, daily timeseries
#' @param xlog should x-axis be log transformed
#' @param size linesize
#' @param color color of line
#' @param vline_color color of vertical lines
#' @details Plot uses ggplot2 and can be changed accordingly. The spectrum is build around the spec.pgram() function
#' @author Daniel Ollech
#' @examples x <- daily_sim(3)$original
#' plot_spectrum(x)
#' @export

plot_spectrum <- function(x, xlog=FALSE, size=1, color="black", vline_color="#6F87B2") {

  if (any(class(x)=="ts")) {
    if (stats::frequency(x) != 365) warning("Time series should be a daily time series")
    x <- ts2xts(x)}

title <- ifelse(xlog, "Differenced series, x-axis=log", "Differenced series")
axistransform <- ifelse(xlog, "xy", "y")

  df <- data.frame(spectrum=(stats::spec.pgram(diff(xts2ts(x)), plot=F)$spec), freq=stats::spec.pgram(diff(xts2ts(x)), plot=F)$freq)
  plot1 <- ggplot2::ggplot(df, ggplot2::aes_string(x="freq", y="spectrum")) + ggplot2::geom_line(size=size, color=color) +  ggplot2::scale_y_continuous(trans='log10') + ggplot2::geom_vline(ggplot2::aes(xintercept=12), colour=vline_color, linetype="dotted") +ggplot2::geom_vline(ggplot2::aes(xintercept=24), colour=vline_color, linetype="dotted")  +ggplot2::geom_vline(ggplot2::aes(xintercept=365/7), colour=vline_color, linetype="dotted") +ggplot2::geom_vline(ggplot2::aes(xintercept=365/7*2), colour=vline_color, linetype="dotted") + ggplot2::geom_vline(ggplot2::aes(xintercept=365/7*3), colour=vline_color, linetype="dotted") + ggplot2::theme_bw() + ggplot2::theme(panel.grid = ggplot2::element_blank()) + ggplot2::ggtitle(title) + ggplot2::xlab("Cycles per year") + ggplot2::ylab("")


  if (xlog) {
    plot1 <- plot1 + ggplot2::scale_x_continuous(trans='log10') + ggplot2::geom_vline(ggplot2::aes(xintercept=1), colour=vline_color, linetype="dotted") + ggplot2::geom_vline(ggplot2::aes(xintercept=2), colour=vline_color, linetype="dotted") + ggplot2::geom_vline(ggplot2::aes(xintercept=3), colour=vline_color, linetype="dotted") + ggplot2::geom_vline(ggplot2::aes(xintercept=4), colour=vline_color, linetype="dotted") + ggplot2::geom_vline(ggplot2::aes(xintercept=5), colour=vline_color, linetype="dotted") + ggplot2::geom_vline(ggplot2::aes(xintercept=6), colour=vline_color, linetype="dotted") + ggplot2::geom_vline(ggplot2::aes(xintercept=7), colour=vline_color, linetype="dotted") + ggplot2::geom_vline(ggplot2::aes(xintercept=8), colour=vline_color, linetype="dotted") + ggplot2::geom_vline(ggplot2::aes(xintercept=9), colour=vline_color, linetype="dotted") + ggplot2::geom_vline(ggplot2::aes(xintercept=10), colour=vline_color, linetype="dotted")
  }


  return(plot1)
}



