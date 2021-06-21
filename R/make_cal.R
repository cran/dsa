#' Creating holiday regressor that increases linearly up to holiday and decreases afterwards
#' 
#' Creating holiday regressor that increases linearly up to holiday and decreases afterwards
#' @param holidays Holidays for which regressor will be created
#' @param h Forecast horizon
#' @param original xts time series which characteristics will be used
#' @param original2 ts time series which characteristics will be used
#' @author Daniel Ollech
#' @details This function is used internally in dsa()
#' @examples a <- daily_sim(n=8)$original
#' \dontrun{make_cal(holidays="Easter", original=a, original2=xts2ts(a, freq=365))}
#' @export

make_cal <- function(holidays=NULL, h=365, original=NA, original2=NA) {

if (is.null(holidays)) {
  outhol=louthol=NULL
}   else { 

  s_start <- as.integer(format(xts::first(zoo::index(original)), "%Y"))
  e_end  <- as.integer(format(xts::last(zoo::index(original)), "%Y"))

  for (j in 1:length(holidays)) {    
  mhol_create <- function(back=1) {
    stats::window(make_holiday(eval(parse(text=paste("timeDate::", holidays[j], "(", as.character(s_start), ":", as.character(e_end), ")", sep=""))), shift=-back), start=stats::start(original2), end=xts::last(zoo::index(original2)), frequency=365)}
  
  mhol_x <- ts_sum(lapply(c(-3), function(x) {mhol_create(x)  })) ; 
  mhol2 <- ts_sum(lapply(c(-2), function(x) {mhol_create(x) *2 })) 
  mhol3 <- ts_sum(lapply(c(-1), function(x) {mhol_create(x) *3 })) 
  mhol4 <- ts_sum(lapply(c(0), function(x) {mhol_create(x) *4 })) 
  mhol5 <- ts_sum(lapply(c(1), function(x) {mhol_create(x) *5 })) 
  mhol6 <- ts_sum(lapply(c(2), function(x) {mhol_create(x) *4 })) 
  mhol7 <- ts_sum(lapply(c(3), function(x) {mhol_create(x) *3 })) 
  mhol8 <- ts_sum(lapply(c(4), function(x) {mhol_create(x) * 2 })) 
  mhol9 <- ts_sum(lapply(c(5), function(x) {mhol_create(x) })) 
  mhol <- mhol_x+mhol2+mhol3+mhol4+mhol5+mhol6+mhol7+mhol8+mhol9;
  
  if (j == 1) { outhol <- mhol} else {
    outhol <- cbind(outhol, mhol)}
  } 


  for (j in 1:length(holidays)) {
    h <- h
  lhol_create <- function(back=1, k=1) { 
  stats::window(make_holiday(eval(parse(text=paste("timeDate::", holidays[j], "(", as.character(s_start), ":", as.character(e_end+2), ")", sep=""))), shift=-back), start=(stats::end(original2)+c(0,1)), end=(stats::end(original2)+c(k,0)), frequency=365) }
  
  lhol_x <- ts_sum(lapply(-3, function(x) {lhol_create(back=x, k=h/365) }))
  lhol_2 <- ts_sum(lapply(-2, function(x) {lhol_create(back=x, k=h/365)*2 }))
  lhol_3 <- ts_sum(lapply(-1, function(x) {lhol_create(back=x, k=h/365)*3 }))
  lhol_4 <- ts_sum(lapply(0, function(x) {lhol_create(back=x, k=h/365)*4 }))
  lhol_5 <- ts_sum(lapply(1, function(x) {lhol_create(back=x, k=h/365)*5 }))
  lhol_6 <- ts_sum(lapply(2, function(x) {lhol_create(back=x, k=h/365)*4 }))
  lhol_7 <- ts_sum(lapply(3, function(x) {lhol_create(back=x, k=h/365)*3 }))
  lhol_8 <- ts_sum(lapply(4, function(x) {lhol_create(back=x, k=h/365)*2 }))
  lhol_9 <- ts_sum(lapply(5, function(x) {lhol_create(back=x, k=h/365) }))
  lhol <- lhol_x + lhol_2 + lhol_3+ lhol_4+ lhol_5+ lhol_6+ lhol_7+ lhol_8+ lhol_9;
  
  if (j == 1) { louthol <- lhol} else {
    louthol <- cbind(louthol, lhol)}
  
  }

  if (!is.null(colnames(outhol))) {
    colnames(outhol) <- holidays 
    colnames(louthol) <- holidays  
  }
  

}
  
list(mhol=outhol, lhol=louthol)
}





