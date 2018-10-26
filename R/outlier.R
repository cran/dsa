#' Outlier adjustment of daily time series
#' 
#' Outlier adjust any daily time series with an algorithm similar to that used in TRAMO. This function draws heavily from the tsoutliers package by Javier López-de-Lacalle.
#' @param series Input time series
#' @param model ARIMA model used 
#' @param cval Critical Value for outlier Detection
#' @param types Types of Outliers included. "AO", "LS", "TC" and "IO" permitted.
#' @param maxit.oloop Maximum iterations of the outer loop
#' @param maxit.iloop Maximum iterations of the inner loop
#' @param maxit.endloop Maximum iterations of the end loop.
#' @param holidays Holiday regressors used in regARIMA
#' @param number.fourier Number of trigonometric regressors used to model seasonality
#' @references López-de-Lacalle, Javier (2017). R package tsoutliers.
#' @examples set.seed(356)
#' x <- arima.sim(list(order = c(1,1,0), ar = 0.7), n = 365*4)
#' timeseries <- ts(x, freq=365, start=c(2001,1))
#' shocks <- rbinom(length(timeseries), 1, 0.002) * 1.5 * timeseries
#' timeseries <- timeseries + shocks
#' modelfit <- arima(timeseries, order = c(1,1,0))
#' out <- outlier(timeseries, model=modelfit, cval=8)
#' ts.plot(timeseries, out$series_adj, col=c("red", "black"))
#' Names = c("Original Series", "Outlier Adjusted")
#' legend(2004.2, 140, Names, col=c("red", "black"), lty=1, bty="n", cex=0.75)
#' @details This function is used internally in dsa()
#' @export


outlier <- function(series, model, cval=7, types=c("AO", "LS", "TC"), maxit.oloop=1, maxit.iloop=2, maxit.endloop=1000, holidays=NULL, number.fourier=13) {

y.oloop <- series
fit.oloop  <- model
moall  <- NULL

holidays <- holidays

iter <- 0
fo <- number.fourier
n <- length(series)

while(iter < maxit.oloop)
{
  pars <- tsoutliers::coefs2poly(model)
  resid.oloop <- stats::residuals(fit.oloop)
  resid.med  <- stats::quantile(resid.oloop, probs=0.5)
  sigma  <- 1.483*stats::quantile(abs(resid.oloop - resid.med), probs=0.5)
  mo.iloop <- suppressWarnings(tsoutliers::locate.outliers.iloop(resid.oloop, pars, cval=cval, types=types, maxit=maxit.iloop))
if (nrow(mo.iloop)==0)
  break
moall <- rbind(moall, mo.iloop)
oeff <- tsoutliers::outliers.effects(mo.iloop, n, weights=TRUE, pars=pars)
y.oloop <- y.oloop - rowSums(oeff)

if (!is.null(holidays)) {fit.oloop <- forecast::Arima(y.oloop, order=model$arma[c(1,6,2)], xreg=cbind(forecast::fourier(series, fo), holidays), method="ML", include.drift=T)} else {fit.oloop <- forecast::Arima(y.oloop, order=model$arma[c(1,6,2)], xreg=cbind(forecast::fourier(series, fo)), method="ML", include.drift=T)}

iter <- iter+1
}

if (!is.null(moall)) {
xreg_new <- tsoutliers::outliers.effects(moall, n, weights=FALSE, pars=pars)
sdx<- apply(xreg_new, 2, function(x) stats::sd(x,na.rm=TRUE))
xreg_new <- xreg_new[, sdx!=0]
moall <- moall[sdx!=0,]


iter <- 0

y <- series
while(TRUE) {

if (!is.null(holidays))  {
  fit <- tryCatch(forecast::Arima(y.oloop, order=model$arma[c(1,6,2)], xreg=cbind(xreg_new , forecast::fourier(series, fo), holidays), method="ML", include.drift=T))
  if(inherits(fit, "error")) { fit <- forecast::Arima(y.oloop, order=model$arma[c(1,6,2)], xreg=cbind(xreg_new , forecast::fourier(series, fo), holidays), method="CSS-ML", include.drift=T) }
  } else {
  fit <- tryCatch(forecast::Arima(y, order=c(model$arma[1],model$arma[6],model$arma[2]), xreg=cbind(xreg_new , forecast::fourier(series, fo)), method="ML", include.drift=T), error=function(e) e)
  if(inherits(fit, "error")) { fit <- tryCatch(forecast::Arima(y, order=c(model$arma[1],model$arma[6],model$arma[2]), xreg=cbind(xreg_new , forecast::fourier(series, fo)), method="CSS-ML", include.drift=T), error=function(e) e)   }
  
}

  xregcoefs <- stats::coef(fit)

  ind <-match(colnames(xreg_new), gsub("xreg.xreg_new.", "", names(xregcoefs)))
  if (length(ind[!is.na(ind)])==0) {ind <- match(colnames(xreg_new), names(xregcoefs))}
  xregcoefs <- xregcoefs[ind]

  tstats <- xregcoefs/sqrt(diag(fit$var.coef)[ind])
  ref <- which(abs(tstats)<cval)  
  if(length(ref)>0)
  {
    x = paste(moall$type, moall$ind, sep="")[-ref]
    moall <- moall[-ref,]
    xreg_new <- data.matrix(xreg_new[,-ref]) 
    colnames(xreg_new) = x
  } else
    break
if (nrow(moall)==0)
  break

iter <- iter+1
  
if (iter > maxit.endloop)
  break

}



outliers <- moall[order(moall$ind),]

if(dim(moall)[1]==0) {series_adj <- y; oeff=rep(0, times=length(y))} else {
xr <- tsoutliers::outliers.effects(moall, n, weights=FALSE, pars=tsoutliers::coefs2poly(fit))
xrc <- stats::coef(fit)
ind <- match(paste("xreg.xreg_new.", colnames(xr), sep=""), names(xrc))
if (length(ind[!is.na(ind)])==0) {ind <- match(colnames(xr), names(xrc))}
xrc <- xrc[ind]

oeff <- xr[,!is.na(xrc)] %*% cbind(xrc[!is.na(xrc)])
series_adj <- y - oeff

if (length(ind[!is.na(ind)])==0) {series_adj <- y}
}
} else {
  oeff <- rep(0, times=length(series))
  outliers <- "No Outliers found"
  series_adj <- series
}

oeff[is.na(oeff)] <- 0


list(outlier_eff=oeff, outliers=outliers, series_adj=series_adj, regressors = xreg_new, orig=series)
}



