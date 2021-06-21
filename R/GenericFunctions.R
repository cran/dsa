#' Plot daily time series
#'
#' Plotting output for objects of class "daily"
#' @param x Result of dsa() that will be plotted
#' @param trend Boolean. Inclusion of a trend estimate.
#' @param dy should dygraphs be used for plotting
#' @param ... Other plot parameters (only if dy=FALSE)
#' @details The original series is plotted in black, the seasonally adjusted series is colored in red, and if trend=T, a blue trend line is added.
#' @author Daniel Ollech
#' @examples x <- daily_sim(3)$original
#' \dontrun{res<- dsa(x, fourier_number = 24, outlier.types="AO", reg.create=NULL, model=c(3,1,0))}
#' \dontrun{plot(res, dy=FALSE)}
#' @export

plot.daily <- function(x, dy=TRUE, trend=FALSE, ...) {

if(!("dygraphs" %in% rownames(utils::installed.packages())) | !dy)  {
  original <- stats::ts(x$output[,2], start=c(as.numeric(format(xts::first(zoo::index(x$output[,2])), "%Y")), as.numeric(format(xts::first(zoo::index(x$output[,2])), "%m"))*30+as.numeric(format(xts::first(zoo::index(x$output[,2])), "%d"))), frequency=365)
  sa <- stats::ts(x$output[,1], start=c(as.numeric(format(xts::first(zoo::index(x$output[,1])), "%Y")), as.numeric(format(xts::first(zoo::index(x$output[,1])), "%m"))*30+as.numeric(format(xts::first(zoo::index(x$output[,1])), "%d"))), frequency=365)
  Trend <- stats::ts(x$output[,4], start=c(as.numeric(format(xts::first(zoo::index(x$output[,4])), "%Y")), as.numeric(format(xts::first(zoo::index(x$output[,4])), "%m"))*30+as.numeric(format(xts::first(zoo::index(x$output[,4])), "%d"))), frequency=365)
  if (trend) {
    stats::ts.plot(original, Trend, sa, col=c("#000000","#0062a1","#f08927"), ...)
    } else {
    stats::ts.plot(original, sa, col=c("#000000", "#f08927"), ...)
  }
} else {
  if (trend) {
    dygraphs::dyOptions(dygraphs::dyRangeSelector(dygraphs::dygraph(x$output[,c(2,1,4)])),colors=c("#000000","#f08927", "#0062a1"))} else {
  dygraphs::dyOptions(dygraphs::dyRangeSelector(dygraphs::dygraph(x$output[,c(2,1)])),colors=c("#000000", "#f08927"))
}

}
}




#' Print daily time series
#'
#' Print output for objects of class "daily"
#' @param x Result of dsa() that will be printed
#' @param ... further arguments handed to print()
#' @author Daniel Ollech
#' @examples x <- daily_sim(3)$original
#' \dontrun{res<- dsa(x, fourier_number = 24, outlier.types="AO", reg.create=NULL, model=c(3,1,0))}
#' \dontrun{print(res)}
#' @export

print.daily <- function(x, ...) {
print(x$reg, ...)
  SD <- function(x) {stats::sd(x, na.rm=TRUE)}
z <- round(apply(x$sfac_result, 2, SD),2)
names(z) <- paste("SD of", names(z))
cat("\n")
for (j in seq(z)){
  cat(paste(names(z)[j], z[j], "\n"))
}

z <- round(apply(x$output, 2, SD),2)
names(z) <- paste("SD of", names(z))
cat("\n")
for (j in seq(z)){
  cat(paste(names(z)[j], z[j], "\n"))
}

}
