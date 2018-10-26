#' Seasonally Adjust Daily Time Series
#' 
#' Seasonally adjust daily time series using the dsa approach
#' @param series Input time series
#' @param span.start Define when seasonal adjustment should begin
#' @param Log Boolean. Should multiplicate or additive model be used?
#' @param Diff Number of differences taken before STL is run.
#' @param model ARIMA order of non-seasonal part
#' @param automodel Set of models to be considered for automatic model detection. Either "full" or "reduced" set of fourier regressors included.
#' @param ic Information criterion that is used for automodelling. One of "bic", "aic" or "aicc"
#' @param fourier_number Number of trigometric regressors to model annual and monthly seasonality
#' @param s.window1 STL parameter s.window for the day of the week effect
#' @param s.window2 STL parameter s.window for the day of the month effect
#' @param s.window3 STL parameter s.window for the day of the year effect
#' @param t.window1 STL parameter t.window for the day of the week effect
#' @param t.window2 STL parameter t.window for the day of the month effect
#' @param t.window3 STL parameter t.window for the day of the year effect
#' @param cval Critical value for outlier adjustment
#' @param robust1 Boolean. Should robust STL be used for the day of the week effect
#' @param robust2 Boolean. Should robust STL be used for the day of the month effect
#' @param robust3 Boolean. Should robust STL be used for the day of the year effect
#' @param regressor Pre-specified regressors
#' @param forecast_regressor Pre-specified regressors to be used for forecasting
#' @param reg.create Names of Holidays for which regressors will be created
#' @param reg.dummy If specified dummy variables of specified length are created and used as regressors
#' @param modelspan Last x years used for regARIMA modelling.
#' @param outlier.types The following are possible: "LS", "TC", "AO", "IO"
#' @param trend_month Length of support period for trend estimation
#' @param outer3 Number of iterations of outer loop in STL for day of the year effect
#' @param inner3 Number of iterations of inner loop in STL for day of the year effect
#' @param reiterate3 Number of total iterations of STL for the day of the year effect
#' @param h Forecast horizon in number of days
#' @param progressBar Should a progress bar be displayed?
#' @author Daniel Ollech
#' @references Ollech, Daniel (2018). Seasonal adjustment of daily time series. Bundesbank Discussion Paper 41/2018.
#' @examples x = daily_sim(5)$original
#' res <- dsa(x, cval=7, model=c(3,1,0),fourier_number = 13, reg.create=NULL) 
#' @details This function can be used to seasonally and calendar adjust daily time series using multiplicative model.
#' @export

dsa <- function(series, span.start=NA, model=NULL, Log=FALSE, Diff=0, automodel="reduced", ic="bic", fourier_number=NA, s.window1=151, s.window2=51, s.window3=15, t.window1=NULL, t.window2=NULL, t.window3=NULL,cval=7, robust1=TRUE, robust2=TRUE, robust3=TRUE, regressor=NULL, forecast_regressor=NULL, reg.create=c("Easter", "Ascension"), reg.dummy=NULL, outlier.types=c("AO", "LS", "TC"), modelspan=NULL, trend_month=3, outer3=NULL, inner3=NULL, h=365, reiterate3=NULL, progressBar=TRUE) {
  
# Preliminary checks
  if (min(series, na.rm=TRUE)<=0) {stop("Series contains non-positive values. Only positive values allowed for multiplicative time series model.")}
  
  if (h >= 10 & h %%365 != 0) {warning("Forecast horizon h should be smaller than 10 or a multiple of 365. Might give problems if specification contains holiday regressors")}
  if (h < 10 & h > 0) {h <- h*365}

# Dealing with timezone issues  
  Euro400 <- series
  times <- as.POSIXlt(seq.Date(from=as.Date(xts::first(zoo::index(Euro400))), to=as.Date(xts::last(zoo::index(Euro400))), by="days"))
  attr(times, "tzone") <- "GMT"; attr(zoo::index(Euro400), "tzone") <-"GMT"
  times <- round(times, "days")
  Fill <- xts::xts(rep(NA, times=length(times)), order.by=times)
  Euro400 <- xts::merge.xts(Fill, Euro400)[,2]
  
  if (progressBar) {
  total <- 1
  pb <- utils::txtProgressBar(title = "Seasonal adjustment of Daily Data", min = 0, max = total, width = NA, label="Configuring data", style=3)
  utils::setTxtProgressBar(pb, 1/21, title="Seasonal adjustment of Daily Data", label="Configuring data")}

    
#### Step 1:  Adjustment of Weekday Effects
  # Changing the time series format to ts + taking logs + differencing
  
  if (is.na(span.start)) {span.start=stats::start(Euro400)}
  
  startingvalue <- suppressWarnings(c(as.integer(format(xts::first(zoo::index(Euro400[paste(span.start, "/", sep="")])), "%Y")), timeDate::dayOfYear(timeDate::timeDate(xts::first(zoo::index(Euro400[paste(span.start, "/", sep="")])), "%Y-%m-%d")+13e+3)))  ### Überprüfen
  
  if (progressBar) {utils::setTxtProgressBar(pb, 2/21, label="Estimating Day-of-the-Week")}
  na.zero <- function(x) {zoo::na.fill(x, 0)}  # Filling up missing values with 0 
  

  Euro400_7<- stats::ts(as.numeric(Scaler(Euro400[paste(span.start, "/", sep="")], Diff=Diff, Log=Log)), frequency=7, start=startingvalue)

  stl_1  <- stats::stl(Euro400_7, s.window=s.window1, na.action = na.zero, robust=robust1, t.window=t.window1) 
  stl1_E400 <- stl_1
  
  # Computing S1-adjusted
  v1 = stl_1$time.series[,2] + stl_1$time.series[,3]
  s1 <- Descaler(v1, Diff=Diff, y=Euro400[paste(span.start, sep="")])
  s1 <- s1 - mean(s1)+mean(Scaler(Euro400, Log=Log))  
  

  #### Step 2: Adjustment of Holiday Effects and Forecasting
  # Removing Feb 29th
  times <- as.POSIXlt(seq.Date(from=as.Date(xts::first(zoo::index(Euro400))), to=as.Date(xts::last(zoo::index(Euro400))), by="days"))
  s1_xts <- xts::xts(s1, order.by=xts::last(times, n=length(s1))) 
  s1_xts[format(zoo::index(s1_xts), "%m-%d")=="02-29"] <- NA
  s1_ts <- stats::ts(s1_xts[!is.na(s1_xts)], start=c(as.integer(format(zoo::index(s1_xts)[1], "%Y")), as.integer(timeDate::dayOfYear(timeDate::timeDate(zoo::index(s1_xts[1]))))), freq=365)[,1]   

    # Creating Holiday Regressors 

  if (is.null(reg.dummy)) {
    hol <- makeCal(holidays=reg.create, h=h, original=s1_xts, original2 = s1_ts)
  }    else {
    hol <- makeDummy(holidays=reg.create, from=reg.dummy[1], to=reg.dummy[2], h=h, original=s1_xts, original2 = s1_ts)}
  
  # Setting Modelspan
  if (!is.null(modelspan)) {
    keep_s1_ts <- s1_ts
    s1_ts <- stats::window(s1_ts, start=c(stats::end(s1_ts)[1]-modelspan, stats::end(s1_ts)[2]), end=stats::end(s1_ts))
    keep_s1_xts <- s1_xts
    s1_xts <- ts2xts(s1_ts)
    keep_regressor <- regressor; ll <- length(s1_ts); ll2 <- length(keep_s1_ts)
    regressor <- regressor[(ll2-ll+1):length(s1_ts)]
    
    if (is.null(reg.dummy)) {
      keep_hol <- hol
      hol <- makeCal(holidays=reg.create, h=h, original=s1_xts, original2 = s1_ts)
    }    else {
      keep_hol <- hol
      hol <- makeDummy(holidays=reg.create, from=reg.dummy[1], to=reg.dummy[2], h=h, original=s1_xts, original2 = s1_ts)}
  }
  
  ### Determining the length of the Fourier-series
  aicc400 <-  aicc400_2 <- bic400 <- rep(NA, times=30)
  

  if (!is.null(model)) {
    if (progressBar) {utils::setTxtProgressBar(pb, 5/21, label="Estimating preliminary ARIMA model")}
    automodel=NULL
    s_arima365_prior=data.frame(arma=rep(0,6))
    s_arima365_prior$arma[1]=model[1]
    s_arima365_prior$arma[6]=model[2]
    s_arima365_prior$arma[2]=model[3]
  }
  
  if (!is.null(automodel)){
    if (progressBar) { utils::setTxtProgressBar(pb, 6/21, label="Estimating prelimary automatic ARIMA model")}
  if (substr(automodel,1,1)=="r") {
    s_arima365_prior <- tryCatch(forecast::auto.arima(s1_ts, stepwise=TRUE, max.d=1, seasonal=F, ic=ic, xreg=cbind(forecast::fourier(s1_ts, 24)[,c(1:10, c(23:24, 47:48))], hol$mhol, regressor)), error=function(e) e)
    if(inherits(s_arima365_prior, "error")) {s_arima365_prior$arma[1] <- 1; s_arima365_prior$arma[6] <- 1; s_arima365_prior$arma[2] <- 0 }

  }
  
  if (substr(automodel,1,1)=="f") {
  s_arima365_prior <- tryCatch(forecast::auto.arima(s1_ts, stepwise=TRUE, max.d=1, seasonal=F, ic=ic, xreg=cbind(forecast::fourier(s1_ts, 24), hol$mhol, regressor)), error=function(e) e)
  if(inherits(s_arima365_prior, "error")) {s_arima365_prior$arma[1] <- 1; s_arima365_prior$arma[6] <- 1; s_arima365_prior$arma[2] <- 0 }}}

  if (is.na(fourier_number)){
    for (j in 1:30){
      
      a3 <- tryCatch(forecast::Arima(s1_ts, order=c(s_arima365_prior$arma[1],s_arima365_prior$arma[6],s_arima365_prior$arma[2]), xreg=cbind(forecast::fourier(s1_ts,j), hol$mhol, regressor), method="ML"), error=function(e) e)
      if(inherits(a3, "error")) {next}
      
      aicc400[j] <- a3$aicc#; bic400[j] <- a3$bic; #aicc400_2[j] <- a$aicc
    }} else {
      aicc400 <- c(NA, rep=30)
      aicc400[fourier_number] <- 5
    }
  
  if(length(aicc400[!is.na(aicc400)])==0) {aicc400[24] <- 4.3391883295} # Just some random number so that if the automatic detection fails, 24 fourier_terms are included, which capture annually and monthly recurring patterns.
  
  # Putting things together
  s_arima365 <- forecast::Arima(s1_ts, order=c(s_arima365_prior$arma[1],s_arima365_prior$arma[6],s_arima365_prior$arma[2]), xreg=cbind(forecast::fourier(s1_ts, which.min(aicc400)), hol$mhol, regressor), method="ML")
  
  if (progressBar) {utils::setTxtProgressBar(pb, 8/21, label="Outlier Detection")}
  
  # Outlier Detection
    cval_new <- cval
    ol <- tryCatch(suppressWarnings(outlier(series=s1_ts, model=s_arima365, cval=cval,  types=outlier.types, holidays=cbind(hol$mhol, regressor), number.fourier=which.min(aicc400))), error=function(e) e)
    
    if(inherits(ol, "error")) {
      cval_new <- cval_new+1
      tryCatch(suppressWarnings(outlier(series=s1_ts, model=s_arima365, cval=cval_new ,  types=outlier.types, holidays=cbind(hol$mhol, regressor), number.fourier=which.min(aicc400))), error=function(e) e)}
    
    if(inherits(ol, "error")) {
      cval_new <- cval_new+1
      tryCatch(suppressWarnings(outlier(series=s1_ts, model=s_arima365, cval=cval_new,  types=outlier.types, holidays=cbind(hol$mhol, regressor), number.fourier=which.min(aicc400))), error=function(e) e)}
    
    if(inherits(ol, "error")) {
      s1_ol <- s1_ts
      outlier_effect <- rep(0, length(s1_ol))
      } 
    
    if(!inherits(ol, "error")) {
      s1_ol <- ol$series_adj
      outlier_effect <- ol$outlier_eff
    }
  
  if (progressBar) {utils::setTxtProgressBar(pb, 14/21, label="Estimating calendar effects")}
  
  arima_reg365   <-  forecast::Arima(s1_ol, order=c(s_arima365$arma[1],s_arima365$arma[6],s_arima365$arma[2]), xreg=cbind(forecast::fourier(s1_ts, which.min(aicc400)), hol$mhol, regressor), method="ML", include.constant=TRUE); model_aicc_E400 <- arima_reg365
  

  fc1 <- forecast::forecast(arima_reg365, h=h, xreg=cbind(forecast::fourier(s1_ts,which.min(aicc400), h=h), hol$lhol, forecast_regressor))
  
  if (!is.null(modelspan)) {
    firstpart <- keep_s1_ts[1:(length(keep_s1_ts)-length(s1_ts))]
    s1_fc <- stats::ts(c(firstpart, fc1$model$x, fc1$mean), start=stats::start(keep_s1_ts), frequency=365)
    hol <- keep_hol
    regressor <- keep_regressor
    s1_xts <- keep_s1_xts
    s1_ts <- keep_s1_ts
  } else {
    s1_fc <- stats::ts(c(fc1$model$x,fc1$mean), start=stats::start(s1_ts), frequency=365)
  }
  

  # Calendar Adjustment (Easter and Ascension / reg.create)
  if (!is.null(reg.create)) {
    if (is.null(dim(hol$mhol))) {
      cf <- ((model_aicc_E400$coef[grep("hol", names(model_aicc_E400$coef))]) * (c(hol$mhol, hol$lhol)))
          } else {
    cf <- rep(0, times=(dim(hol$mhol)[1]+dim(hol$lhol)[1]))
    for (k in 1:dim(hol$mhol)[2]) {
      cf <- cf + ((model_aicc_E400$coef[grep("hol", names(model_aicc_E400$coef))][k]) * (rbind(hol$mhol, hol$lhol)[,k]))
    }}
    
    k1 <- s1_fc - cf
  } else {
    k1 <- s1_fc
  }
  
  # Regressor Adjustment (regressor)
  if (!is.null(regressor)) {
    if (is.null(dim(as.data.frame(regressor)))) {
      cf <- ((model_aicc_E400$coef[length(model_aicc_E400$coef)]) * (c(regressor, forecast_regressor)))
    } else {
    for (k in 1:dim(as.data.frame(regressor))[2]) {
      cf <- cf + ((model_aicc_E400$coef[(length(model_aicc_E400$coef)+1-dim(as.data.frame(regressor))[2]):length(model_aicc_E400$coef)][k]) * (rbind(matrix(regressor, nrow=nrow(data.frame(regressor))),matrix(forecast_regressor, nrow=nrow(data.frame(forecast_regressor))))[,k]))
    }}
    
    k1 <- k1 - cf
  } else {
    k1 <- k1
  }
  
  if (progressBar) { utils::setTxtProgressBar(pb, 15/21, label="Estimating Day-Of-Month Effect")}

  #### Step 3: Adjustment of Monthly Period 
  k1_xts <- ts2xts(k1);  
  k1_complete <- k1_xts   
  k1_31 <- fill31(k1_xts, fill="spline") ;  
  
  k1x <- stats::ts(Scaler(k1_31, Diff=Diff), frequency=31) # Log needs to be FALSE in any case, because its inherited from before
  
  stl_2 <- stats::stl(k1x, s.window=s.window2, robust=robust2, t.window=t.window2)
  stl2_E400 <- stl_2
  s2_31 <- stats::ts(as.numeric(Descaler(stl_2$time.series[,2] + stl_2$time.series[,3], Diff=Diff, y=ts2xts(s1_ts))), start=stats::start(k1_31), freq=372)
  s2 <- s2_31 - mean(s2_31) + mean(k1_31)

  s2 <- drop31(s2, new_start=stats::start(k1)[2], new_end=stats::end(k1)[2]) 
  s2_complete <- s2
  s2 <- xts2ts(s2) 


  
  if (progressBar) {utils::setTxtProgressBar(pb, 16/21, label="Estimating Day-Of-Year Effect")}
  
  #### Step 4: Adjustment of Annual Period
  if (is.null(outer3)){
    outerval = if(robust3) 15 else 0
  } else {outerval=outer3}
  
  if (is.null(inner3)){
    innerval = if(robust3) 1 else 2
  } else {innerval=inner3}
  
  s2x <- Scaler(s2, Diff=Diff)
  stl_3 <- stats::stl(s2x, s.window=s.window3, robust=robust3, outer=outerval, inner=innerval, t.window=t.window3)
  s3 <- stats::ts(as.numeric(Descaler(stl_3$time.series[,2] + stl_3$time.series[,3], y=ts2xts(s1_ts), Diff=Diff)), start=startingvalue, frequency=365)
  
  
  if (!is.null(reiterate3)) {
    for (i in 1:reiterate3) {
      stl_3 <- stats::stl(s3, s.window=s.window3, robust=robust3, outer=outerval, inner=innerval, t.window=t.window3)
      s3 <- stats::ts(as.numeric(Descaler(stl_3$time.series[,2] + stl_3$time.series[,3], y=ts2xts(s1_ts), Diff=Diff)), start=startingvalue, frequency=365)
    }}
  
  s3_E400 <- s3
  stl3_E400 <- stl_3
  
  if (progressBar) {utils::setTxtProgressBar(pb, 16/21, label="Preparing set of final time series")}
  
  #### Final Seasonal Series
  s_final <- s3 + c(outlier_effect, rep(xts::last(outlier_effect), times=(length(s3)- length(outlier_effect))))
  outlier_effect_xts <- ts2xts(s_final - s3)
  
  times <- as.POSIXlt(seq.Date(from=as.Date(xts::first(zoo::index(Euro400))), to=as.Date(xts::last(zoo::index(Euro400)))+h, by="days"))
  attr(times, "tzone") <- "GMT"; attr(zoo::index(Euro400), "tzone") <-"GMT"
  Fill <- xts::xts(rep(NA, times=length(times)), order.by=times)
  
  s_final_xts <- suppressWarnings(zoo::na.spline(xts::merge.xts(a=ts2xts(s_final), Fill)$a[paste(span.start, "/", sep="")]))

  ### Trend estimation
  # trend_month=number of months the trend should cover
  n_t = ceiling(365/(12/trend_month))
  if (n_t - n_t %/% 2 * 2 == 0) {n_t <- n_t + 1}
  fraction = n_t / length(s_final_xts)
  
  trend_xts <- xts::xts(stats::predict(stats::loess(s_final_xts ~ seq.int(length(s_final_xts)), span=fraction), newdata=seq.int(length(s_final_xts))), order.by=zoo::index(s_final_xts))
  
  #### Final (implicit) Seasonal Factor
  ## Forecasting original Data
  
  times <- as.POSIXlt(seq.Date(from=as.Date(xts::first(zoo::index(Euro400))), to=as.Date(xts::last(zoo::index(Euro400)))+h, by="days"))
  attr(times, "tzone") <- "GMT"; attr(zoo::index(Euro400), "tzone") <-"GMT"
  Fill <- xts::xts(rep(NA, times=length(times)), order.by=times)
  Fill <- Fill[paste(xts::first(zoo::index(s1_xts)), "/", sep="")]
  
  s1_x <- ts2xts(s1_fc)
  s1_x <- xts::merge.xts(s1_x, Fill)$s1_x
  
  outlier_effect2 <- stats::ts(suppressWarnings(as.numeric(outlier_effect)), start=stats::start(s1_ts), freq=365) 
  outliers <- ts2xts(outlier_effect2)
  outliers <- xts::merge.xts(outliers, Fill)$outliers
  outliers[is.na(outliers)] <- xts::last(outliers[!is.na(outliers)])

  #Extrapolating s1-seasonal factors
  sf_x <- day_split(stl_1$time.series[,1], use="ets", h=h)$sf
  times <- as.POSIXlt(seq.Date(from=as.Date(xts::first(zoo::index(s1_xts))), length.out=length(sf_x), by="days")) 
  attr(times, "tzone") <- "GMT"; attr(zoo::index(Euro400), "tzone") <-"GMT"
  sf_x_xts <- xts::xts(sf_x, order.by=times)

  
  times <- as.POSIXlt(seq.Date(from=as.Date(xts::first(zoo::index(s1_xts))), length.out=length(sf_x)+1, by="days")) 
  
  comp <- Scaler(Euro400[paste(xts::first(zoo::index(s1_x)), "/", sep="")], Log=Log)-as.numeric(xts::first(s1_x))-as.numeric(xts::first(outliers))

  s1_fac <- Descaler(xts::xts(sf_x, order.by=seq.Date(as.Date(stats::start(comp)), length.out=length(sf_x), by="days")), Diff=Diff, y=comp)
  
  original_forecast <- s1_x + outliers + s1_fac
  
  s1_complete <- original_forecast  - s1_fac
 

  # Extending Original Data (adding 29th February)
  o_fc_xts <- fill_up(original_forecast, Scaler(Euro400, Log=Log))

  ## Benchmarking seasonally adjusted series
  s_final_xts <- s_final_xts - mean(xts::first(s_final_xts, length(s_final_xts)-h), na.rm=TRUE) + mean(xts::first(o_fc_xts, length(o_fc_xts)-h), na.rm=TRUE)
  ## Benchmarking partially adjusted series
  s1_complete <- Descaler(s1_complete - mean(xts::first(s1_complete, length(s_final_xts)-h), na.rm=TRUE) + mean(xts::first(o_fc_xts, length(o_fc_xts)-h), na.rm=TRUE), Log=Log, y=NA)
 
  k1_complete <- k1_complete + outlier_effect_xts
  k1_complete <- Descaler(k1_complete - mean(xts::first(k1_complete, length(s_final_xts)-h), na.rm=TRUE) + mean(xts::first(o_fc_xts, length(o_fc_xts)-h), na.rm=TRUE), Log=Log, y=NA)
  
  s2_complete <- s2_complete + outlier_effect_xts
  s2_complete <- Descaler(s2_complete - mean(xts::first(s2_complete, length(s_final_xts)-h), na.rm=TRUE) + mean(xts::first(o_fc_xts, length(o_fc_xts)-h), na.rm=TRUE), Log=Log, y=NA)
  
  s1_only <- s1_complete # only s1-adjusted
  
  g <- Descaler(Scaler(s1_complete, Log=Log) - Scaler(k1_complete, Log=Log), Log=Log, y=NA) 
  v <-  Descaler(o_fc_xts, Log=Log, y=NA)
  k1_only <- Descaler(Scaler(v, Log=Log) - Scaler(g, Log=Log), Log=Log, y=NA) # only k1-adjusted
  k1_only <- k1_only - mean(xts::first(k1_only, length(k1_only)-h), na.rm=TRUE) + mean(xts::first(Descaler(o_fc_xts, Log=Log, y=NA), length(k1_only)-h), na.rm=TRUE) 
  
  g <- Descaler(Scaler(k1_complete, Log=Log) - Scaler(s2_complete, Log=Log), Log=Log, y=NA) 
  v <-  Descaler(o_fc_xts, Log=Log, y=NA)
  s2_only <- Descaler(Scaler(v, Log=Log) - Scaler(g, Log=Log), Log=Log, y=NA) #only s2-adjusted
  s2_only <- s2_only - mean(xts::first(s2_only, length(s2_only)-h), na.rm=TRUE) + mean(xts::first(Descaler(o_fc_xts, Log=Log, y=NA), length(s2_only)-h), na.rm=TRUE) 
  
  g <- Descaler(Scaler(s2_complete, Log=Log) - s_final_xts, Log=Log, y=NA) 
  v <-  Descaler(o_fc_xts, Log=Log, y=NA)
  s3_only <- Descaler(Scaler(v, Log=Log) - Scaler(g, Log=Log), Log=Log, y=NA) #only s3-adjusted
  s3_only <- s3_only - mean(xts::first(s3_only, length(s3_only)-h), na.rm=TRUE) + mean(xts::first(Descaler(o_fc_xts, Log=Log, y=NA), length(s3_only)-h), na.rm=TRUE) 
  
  
  # Benchmarking Trend
  trend <- trend_xts - mean(xts::first(trend_xts, length(trend_xts)-h), na.rm=TRUE) + mean(xts::first(o_fc_xts, length(o_fc_xts)-h), na.rm=TRUE)

  # Implicit seasonal factors
  sc_fac <- Descaler(o_fc_xts-s_final_xts, Log=Log, y=NA) * ifelse(Log, 100, 1); colnames(sc_fac) <- "seasonal factor"
  
  # Correcting 29.2
  #sc_fac
  sc_fac[format(zoo::index(sc_fac), "%m-%d")=="02-29"] <- NA
  sc_fac <- zoo::na.spline(sc_fac)
  # Seasonal Factors

  normSf <- function(x) {x - mean(x, na.rm=TRUE) + 100}
  s1_fac <- normSf(Descaler(s1_fac, Log=Log, y=NA)* ifelse(Log, 100, 1)); colnames(s1_fac) <- "s1_fac"
  k1_fac <-  normSf(Descaler(o_fc_xts, Log=Log, y=NA)/k1_only * ifelse(Log, 100, 1)); colnames(k1_fac) <- "k1_fac"
  s2_fac <-  normSf(Descaler(o_fc_xts, Log=Log, y=NA)/s2_only* ifelse(Log, 100, 1)); colnames(s2_fac) <- "s2_fac"
  s3_fac <-  normSf(Descaler(o_fc_xts, Log=Log, y=NA)/s3_only* ifelse(Log, 100, 1)); colnames(s3_fac) <- "s3_fac"
  
  sfac_result <- xts::merge.xts(s1_fac, k1_fac, s2_fac, s3_fac)
  
  if (progressBar) {utils::setTxtProgressBar(pb, 19/21, label="Creating final output")}
  # Final Output
  original <- Descaler(o_fc_xts, Log=Log, y=NA); colnames(original) <- "original series"
  final_sa <- Descaler(s_final_xts, Log=Log, y=NA); colnames(final_sa) <- "final sa series"
  final_sa[format(zoo::index(sc_fac), "%m-%d")=="02-29"] <- Descaler(Scaler(original[format(zoo::index(sc_fac), "%m-%d")=="02-29"], Log=Log) - Scaler(sc_fac[format(zoo::index(sc_fac), "%m-%d")=="02-29"], Log=Log), Log=Log, y=NA) * ifelse(Log, 100, 1)
  
  Descaler(Scaler(s1_complete, Log=Log) - Scaler(k1_complete, Log=Log), Log=Log, y=NA) 
  
  trend <- Descaler(trend, Log=Log, y=NA); colnames(trend) <- "final trend series"
  
  fourier_terms <- which.min(aicc400)
  reg <- model_aicc_E400
  
  sa_result <- xts::merge.xts(s1_complete, k1_complete, s2_complete, final_sa) ;  colnames(sa_result) <- c("SA1", "cal_adj", "SA2", "SA3") # Die Ergebnisse der Zwischenschritte
  
  sa_result2 <- xts::merge.xts(s1_only, k1_only, s2_only, s3_only); colnames(sa_result2) <- c("onlyS1", "onlyCal", "onlyS2", "onlyS3")
  # Graphs and Tables
  
  x1 <- stl1_E400$time.series; x2 <- stl2_E400$time.series; x3 <- stl3_E400$time.series
  b1 <- as.numeric(xts::first(Euro400_7)); c2 <- s1_ts; 
  
  info <- c(ifelse(Log, "Log", "Level"), Diff, h)

  output <- zoo::na.locf(xts::merge.xts(final_sa, original, sc_fac, trend)); names(output) <- c("seas_adj", "original", "sc_fac", "trend")
  
  finout <- list(output=output, fourier_terms=fourier_terms, reg=reg, info=info, stl=list(stl_1, stl_2, stl_3), outlier=ol, sa_result=sa_result, sa_result2=sa_result2, sfac_result=sfac_result)
  class(finout) <- "daily"
  if (progressBar) {utils::setTxtProgressBar(pb, 21/21, label="Done")}
  close(pb)
  if(cval_new > cval & !inherits(ol, "error")) {message(paste("The critical value for outlier adjustment has automatically been set to", cval_new))}
  
  return(finout)
  
}