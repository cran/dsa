#' Creating set of dummy variables for specified Holidays
#' 
#' Creating set of dummy variables for specified Holidays
#' @param holidays holidays for which dummy variables will be created
#' @param from start of holiday regressor. Relative to specified holiday
#' @param to end of holiday regressor. Relative to specified holiday
#' @param h forecast horizon
#' @param original xts time series which characteristics will be used
#' @param original2 ts time series which characteristics will be used
#' @author Daniel Ollech
#' @details This function is used internally in dsa()
#' @export


make_dummy <- function(holidays=NULL, from=-5, to=5, h=365, original=NA, original2=NA) {

  if (is.null(holidays)) {
    outhol=louthol=NULL
  }   else { 
    
    s_start <- as.integer(format(xts::first(zoo::index(original)), "%Y"))
    e_end  <- as.integer(format(xts::last(zoo::index(original)), "%Y"))

    for (j in 1:length(holidays)) {    
      mhol_create <- function(back=1) {
        stats::window(make_holiday(eval(parse(text=paste("timeDate::", holidays[j], "(", as.character(s_start), ":", as.character(e_end), ")", sep=""))), shift=back), start=stats::start(original2), end=stats::end(original2), frequency=365)}
      for (k in from:to) {
            mhol <- ts_sum(lapply(c(k), function(x) {mhol_create(x)  }))
      if (j == 1 & k==from) { outhol <- mhol} else {
        outhol <- cbind(outhol, mhol)}
    } }
    
    
    for (j in 1:length(holidays)) {
      h <- h
      lhol_create <- function(back=1, k=1) { 

        stats::window(make_holiday(eval(parse(text=paste("timeDate::", holidays[j], "(", as.character(s_start), ":", as.character(e_end+h/365), ")", sep=""))), shift=back), start=(stats::end(original2)+c(0,1)), end=(stats::end(original2)+c(k,0)), frequency=365) }
      
      for (q in from:to) {
      lhol <- ts_sum(lapply(q, function(x) {lhol_create(back=x, k=h/365) }))
      
      if (j == 1 & q==from) { louthol <- lhol} else {
        louthol <- cbind(louthol, lhol)}
      
    } }
    
    if (!is.null(colnames(outhol))) {
      colnames(outhol) <- paste(rep(holidays, each=length(from:to)), from:to, sep="") 
      colnames(louthol) <- paste(rep(holidays, each=length(from:to)), from:to, sep="") 
    }
    
    
  }
  
  list(mhol=outhol, lhol=louthol)
}