#' Delete name of xts
#' 
#' Delete name of xts
#' @param x xts time series
#' @details This function can be helpful if one xts is created to be equal to another xts and then changed afterwards. In these cases the new xts inherits the column name of the old xts.
#' @author Daniel Ollech
#' @examples timeseries <- dsa::daily_sim()$original # timeseries inherits name from original
#' colnames(timeseries)
#' colnames(del_names(timeseries))
#' y <- del_names(timeseries)
#' colnames(merge(timeseries, y))
#' @export

del_names <- function(x){ 
  xts::xts(as.numeric(x), zoo::index(x))
}