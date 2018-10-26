#' Add time series
#' 
#' Sequentially add a set of time series
#' @param ... list of ts time series that are added together
#' @author Daniel Ollech
#' @examples ts.sum(list(ts(rnorm(100,10,1)), ts(rnorm(100,10,1)), ts(rnorm(100,10,1))))
#' @details This function is used internally in dsa()
#' @export

ts.sum <- function(...) {
dots <- (...)  
  if (!is.list(dots)) {dots <- list(...)}
  a <- dots[[1]]
  if (length(dots)>1){
  for (j in 2:length(dots)) {
    a <- '+'(a, dots[[j]])
  }}
  a
}


