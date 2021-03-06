#' Seasonally Adjust Daily Time Series
#'
#' Seasonally adjust daily time series using the dsa approach
#' @param series Input time series in xts format
#' @param span.start Define when seasonal adjustment should begin
#' @param Log Boolean. Should multiplicate or additive model be used
#' @param model ARIMA order of non-seasonal part
#' @param automodel Set of models to be considered for automatic model detection. Either "full" or "reduced" set of fourier regressors included
#' @param ic Information criterion that is used for automodelling. One of "bic", "aic" or "aicc"
#' @param include.constant Should drift be allowed for model that includes differencing
#' @param fourier_number Number of trigometric regressors to model annual and monthly seasonality
#' @param max_fourier Maximum number of trigonometric regressors allowed if the number is selected automatically, i.e. fourier_number=NULL
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
#' @param reg_create Names of Holidays for which regressors will be created
#' @param reg_dummy If specified dummy variables of specified length are created and used as regressors
#' @param model_span Last x years used for regARIMA modelling
#' @param feb29 How should February 29th be derived: interpolation of adjusted series ("sa") or combined factor ("sfac")
#' @param outlier Should an outlier adjustment be conducted?
#' @param outlier_types The following are possible: "LS", "TC", "AO", "IO"
#' @param delta The decay rate for TC outliers
#' @param trend_month Length of support period for trend estimation
#' @param outer3 Number of iterations of outer loop in STL for day of the year effect
#' @param inner3 Number of iterations of inner loop in STL for day of the year effect
#' @param reiterate3 Number of total iterations of STL for the day of the year effect
#' @param h Forecast horizon in number of days
#' @param scaler for additive model, if max(abs(series)) > 1e5, scale series
#' @param mean_correction Boolean. Should seasonal factors be standardised so that their mean (over all full cycles) is 0 for additive and 1 for multiplicative models
#' @param progress_bar Boolean. Should a progress bar be displayed
#' @return \code{dsa} returns a \code{daily} object which contains the output of the seasonal adjustment of a daily time series.
#'
#' @return {output} {Contains the calendar and seasonally adjusted series, original series, implicit calendar and seasonal component, and Loess based trend as an \code{xts} object}
#' @return {fourier_terms} {The number of sine and cosine terms used to model the seasonal pattern in the RegARIMA model}
#' @return {reg} {RegARIMA results}
#' @return {info} {Basic information on transformation (Log/Level), differencing and forecast horizon}
#' @return {stl} {A list of length 3, containing the STL results of the day-of-week, day-of-the-month and day-of-the-year adjustment, respectively}
#' @return {outlier} {Result of the outlier adjustment}
#' @return {sa_result} {The original series and the intermediate adjustment results after the day-of-week adjustment (s1_adjusted), calendar adjustment (s1k1_adjusted), day-of-the-month adjustment (s1k1s2_adjusted), and the final adjusted series after the day-of-the-year adjustment (seas_adj) as an \code{xts} object}
#' @return {sa_result2} {The original series only adjusted for single components as an \code{xts} object. Namely the original series itself (original), the original only adjusted for the day-of-the week (s1_adjusted), calendar (k1_adjusted), day-of-the-month (s2_adjusted), and day-of-the-year (s3_adjusted)}
#' @return {sfac_result} {The seasonal and calendar components as an \code{xts} object. Namely, the day-of-the-week (s1_fac), calendar (cal_fac), day-of-the-month (s2_fac), and day-of-the-year component (s3_fac) }
#' @author Daniel Ollech
#' @references Ollech, Daniel (2018). Seasonal adjustment of daily time series. Bundesbank Discussion Paper 41/2018.
#' @references Ollech, Daniel (2021). Seasonal Adjustment of Daily Time Series. Journal of Time Series Econometrics (forthcoming).
#' @examples x = daily_sim(n=4)$original # series with length 4 years
#' res <- dsa(x, cval=7, model=c(3,1,0),fourier_number = 13)
#' @details This function can be used to seasonally and calendar adjust daily time series and decomposing the series into a seasonally adjusted series, a day-of-the-week, a moving holiday, a day-of-the-month and a day-of-the-year component.
#' @details If mean_correction=TRUE (default), the seasonal and calendar factors are corrected, so that over all full years, the mean of the components is 0 in additive models. They will be close to 1 if a multiplicative decomposition (i.e. Log=TRUE) is used. Deviations from 1 may result, because the mean correction is applied to the components before inverting taking logs.
#' @details For long series, the ARIMA modelling and the outlier adjustment may take a long time. It may therefore be a good idea, to specify the ARIMA model used, e.g. model=c(3,1,0). If the series does not contain influential outliers, the outlier adjustment could be skipped by setting outlier=FALSE.
#' @details See vignette for further examples.
#' @export

dsa <- function(series, span.start=NULL, model=NULL, Log=FALSE, automodel="reduced", ic="bic", include.constant=FALSE, fourier_number=24, max_fourier=30, s.window1=53, s.window2=53, s.window3=13, t.window1=NULL, t.window2=NULL, t.window3=NULL, cval=7, robust1=TRUE, robust2=TRUE, robust3=TRUE, regressor=NULL, forecast_regressor=NULL, reg_create=NULL, reg_dummy=NULL, outlier=TRUE, outlier_types=c("AO", "LS", "TC"), delta=0.7, model_span=NULL, feb29="sfac", trend_month=3, outer3=NULL, inner3=NULL, h=365, reiterate3=NULL, scaler=1e7, mean_correction=TRUE, progress_bar=TRUE) {


# Preliminary adjustments and checks --------------------------------------

  if (min(series, na.rm=TRUE)<=0 & Log) {stop("Series contains non-positive values. Only positive values allowed for multiplicative time series model. Set Log=FALSE")}

  if (!is.null(forecast_regressor)) {
    if(nrow(forecast_regressor) != h) {warning(paste("Forecast regressor should include", h, "observations per regressor"))}

    if(ncol(regressor) != ncol(forecast_regressor)) {stop(paste("The parameter regressor contains", ncol(regressor), "columns and forecast_regressor contains", ncol(forecast_regressor), "columns. They need to contain the same number of variables."))}
  }

  if(max(abs(series), na.rm=TRUE) >1e5  & !Log) {original_series <- series/scaler} else {
    original_series <- series
  }

  # Dealing with timezone issues
  times <- as.POSIXlt(seq.Date(from=as.Date(xts::first(zoo::index(original_series))), to=as.Date(xts::last(zoo::index(original_series))), by="days"))
  attr(times, "tzone") <- "GMT"; attr(zoo::index(original_series), "tzone") <-"GMT"
  times <- round(times, "days")
  Fill <- xts::xts(rep(NA, times=length(times)), order.by=times)
  original_series <- xts::merge.xts(Fill, original_series)[,2]
  zoo::index(original_series) <- as.Date(zoo::index(original_series))

  # Filling up missing-values
  if (any(is.na(original_series))){
    message("Missing values detected. To allow DSA to work properly, missing values are imputed using the 'last observation carried forward' algorithm (zoo::na.locf). Leading NAs are removed.\nYou might want to impute the series outside of this function.")
    original_series <- zoo::na.locf(original_series, na.rm=TRUE)
  }


  if (progress_bar) {
    total <- 1
    pb <- utils::txtProgressBar(title = "Seasonal adjustment of Daily Data", min = 0, max = total, width = NA, label="Configuring data", style=3)
    utils::setTxtProgressBar(pb, 1/21, title="Seasonal adjustment of Daily Data", label="Configuring data")}


# Step 1:  Adjustment of day-of-the-week effects --------------------------

  # Changing the time series format to ts (for stl) + taking logs

  if (is.null(span.start)) {span.start=stats::start(original_series)}

  startingvalue <- stats::start(xts2ts(original_series, 365))

  if (progress_bar) {utils::setTxtProgressBar(pb, 2/21, label="Estimating Day-of-the-Week")}

  original_series_7 <- stats::ts(as.numeric(Scaler(original_series[paste(span.start, "/", sep="")], Log=Log)), frequency=7, start=startingvalue)

  if (is.null(s.window1)) {
    stl_1 <- NULL
    s1 <- original_series_7
  } else {
    stl_1  <- stats::stl(original_series_7, s.window=s.window1, robust=robust1, t.window=t.window1)

  # Computing S1-adjusted
    sf1 <- stl_1$time.series[,1]

  if(mean_correction) {
    st <- stats::start(sf1)
    ee <- stats::end(sf1)
    ed <- st + c((ee - st)[1],0)
    ed[1] <- ifelse(ed[1]+ed[2]/7 > ee[1]+ee[2]/7,  ed[1]-1, ed[1])
    sf1x <- stats::window(sf1, start=st, end=ed)
    sf1 <- sf1 - mean(utils::head(sf1x, length(sf1x)-1))

    s1 <- Descaler(original_series_7 - sf1, Log=Log)
  } else {
  s1 <- Descaler(original_series_7 - sf1, Log=Log)}
  }


# Step 2: Adjustment of holiday effects, outlier and forecasting --------

  # Removing Feb 29th
  times <- (seq.Date(from=as.Date(xts::first(zoo::index(original_series))), to=as.Date(xts::last(zoo::index(original_series))), by="days"))

  s1_xts <- xts::xts(s1, order.by=xts::last(times, n=length(s1)))
  s1_ts <- Scaler(xts2ts(s1_xts, 365), Log=Log) # Removing Feb 29th

## 2a Creating holiday regressors --------

  if (is.null(reg_dummy)) {
    hol <- make_cal(holidays=reg_create, h=h, original=s1_xts, original2 = s1_ts)
  }    else {
    hol <- make_dummy(holidays=reg_create, from=reg_dummy[1], to=reg_dummy[2], h=h, original=s1_xts, original2 = s1_ts)}

## 2b Model span settings --------
  if (!is.null(model_span)) {
    keep_s1_ts <- s1_ts
    s1_ts <- stats::window(s1_ts, start=c(stats::end(s1_ts)[1]-model_span, stats::end(s1_ts)[2]), end=stats::end(s1_ts))
    keep_s1_xts <- s1_xts
    s1_xts <- ts2xts(s1_ts)
    keep_regressor <- regressor; ll <- length(s1_ts); ll2 <- length(keep_s1_ts)

  if (is.null(dim(regressor))) {regressor <- regressor[-(1:(ll2-ll))]} else {regressor <- regressor[-(1:(ll2-ll)),]}

  if (is.null(reg_dummy)) {
      keep_hol <- hol
      hol <- make_cal(holidays=reg_create, h=h, original=s1_xts, original2 = s1_ts)
    }    else {
      keep_hol <- hol
      hol <- make_dummy(holidays=reg_create, from=reg_dummy[1], to=reg_dummy[2], h=h, original=s1_xts, original2 = s1_ts)}
  }

## 2c Determining the length of the Fourier-series -------
  ic_value <-  rep(NA, times=max_fourier)

  if (!is.null(model)) {
    if (progress_bar) {utils::setTxtProgressBar(pb, 5/21, label="Estimating preliminary ARIMA model")}
    automodel=NULL
    s_arima365_prior=data.frame(arma=rep(0,6))
    s_arima365_prior$arma[1]=model[1]
    s_arima365_prior$arma[6]=model[2]
    s_arima365_prior$arma[2]=model[3]
  }

  if (!is.null(automodel)){
    if (progress_bar) { utils::setTxtProgressBar(pb, 6/21, label="Estimating prelimary automatic ARIMA model")}
    if (tolower(substr(automodel,1,1))=="r") {

      s_arima365_prior <- tryCatch(forecast::auto.arima(s1_ts, stepwise=TRUE, max.d=1, max.p=3, max.q=3, seasonal=FALSE, ic=ic, xreg=cbind(forecast::fourier(s1_ts, 24)[,c(1:10, c(23:24, 47:48))], hol$mhol, regressor)), error=function(e) e)
      if(inherits(s_arima365_prior, "error")) {s_arima365_prior$arma[1] <- 3; s_arima365_prior$arma[6] <- 1; s_arima365_prior$arma[2] <- 0
      warning("ARIMA order could not be determined automatically and has been set to ARIMA(3,1,0). You may want to specify another model")}
    }

    if (tolower(substr(automodel,1,1))=="f") {
      s_arima365_prior <- tryCatch(forecast::auto.arima(s1_ts, stepwise=TRUE, max.d=1, seasonal=FALSE, allowdrift=include.constant, ic=ic, xreg=cbind(forecast::fourier(s1_ts, 24), hol$mhol, regressor)), error=function(e) e)
      if(inherits(s_arima365_prior, "error")) {s_arima365_prior$arma[1] <- 1; s_arima365_prior$arma[6] <- 1; s_arima365_prior$arma[2] <- 0 }}}

  if (is.null(fourier_number)){
    for (j in 1:max_fourier){

      a3 <- tryCatch(forecast::Arima(s1_ts, order=c(s_arima365_prior$arma[1],s_arima365_prior$arma[6],s_arima365_prior$arma[2]), include.constant=include.constant, xreg=cbind(forecast::fourier(s1_ts,j), hol$mhol, regressor), method="CSS-ML"), error=function(e) e)
      if(inherits(a3, "error")) {next}

      ic_value[j] <- a3$aicc
    }} else {
      ic_value <- rep(NA, times=max_fourier)
      ic_value[fourier_number] <- 5
    }

  if(all(is.na(ic_value))) {ic_value[24] <- 4.3391883295} # Just some random number so that if the automatic detection fails, 24 fourier terms are included. These will then capture annually and monthly recurring patterns.

## 2d Estimating preliminary ARIMA model -------
  rex = cbind(forecast::fourier(s1_ts, which.min(ic_value)), hol$mhol, regressor)

  s_arima365 <- tryCatch(forecast::Arima(s1_ts, order=c(s_arima365_prior$arma[1],s_arima365_prior$arma[6],s_arima365_prior$arma[2]), xreg=rex, include.constant=include.constant, method="CSS-ML"), error=function(e) e)

  if(inherits(s_arima365, "error")) {
    s_arima365 <- forecast::Arima(s1_ts, order=c(s_arima365_prior$arma[1],s_arima365_prior$arma[6],s_arima365_prior$arma[2]), xreg=rex, include.constant=include.constant, method="ML")
  }

  if (progress_bar) {utils::setTxtProgressBar(pb, 8/21, label="Outlier Detection")}

## 2e Outlier --------
  cval_new <- cval
  if (outlier){
  ol <- tryCatch(suppressWarnings(.outlier(series=s1_ts, model=s_arima365, cval=cval,  delta=delta, types=outlier_types, holidays=cbind(hol$mhol, regressor), number.fourier=which.min(ic_value))), error=function(e) e)

  if(inherits(ol, "error")) {
    cval_new <- cval+2
    ol <- tryCatch(suppressWarnings(.outlier(series=s1_ts, model=s_arima365, cval=cval_new,  delta=delta,  types=outlier_types, holidays=cbind(hol$mhol, regressor), number.fourier=which.min(ic_value))), error=function(e) e)
    }

  if(!inherits(ol, "error")) {
    s1_ol <- ol$series_adj
    outlier_effect <- ol$outlier_eff
  } else {
    s1_ol <- s1_ts
    outlier_effect <- rep(0, length(s1_ol))
  }
  } else {
    s1_ol <- s1_ts
    outlier_effect <- rep(0, length(s1_ol))
    ol <- NULL
  }

  if (progress_bar) {utils::setTxtProgressBar(pb, 14/21, label="Estimating calendar effects")}

## 2f Estimating calendar effects --------
  rex = cbind(forecast::fourier(s1_ts, which.min(ic_value)), hol$mhol, regressor)
  colnames(rex) <- gsub("forecast_", "", gsub("mhol.", "", gsub("forecast::fourier(s1_ts, which.min(ic_value)).", "", colnames(rex), fixed=TRUE), fixed=TRUE), fixed=TRUE)

  arima_reg365   <-  forecast::Arima(s1_ol, order=c(s_arima365$arma[1],s_arima365$arma[6],s_arima365$arma[2]), xreg=rex, method="ML", include.constant=include.constant);

  frex = cbind(forecast::fourier(s1_ts,which.min(ic_value), h=h), hol$lhol, forecast_regressor)
  colnames(frex) <- colnames(rex)

  fc1 <- forecast::forecast(arima_reg365, h=h, xreg=frex)

  if (!is.null(model_span)) {
    firstpart <- keep_s1_ts[1:(length(keep_s1_ts)-length(s1_ts))]
    s1_fc <- stats::ts(c(firstpart, fc1$model$x, fc1$mean), start=stats::start(keep_s1_ts), frequency=365)
    hol <- keep_hol
    regressor <- keep_regressor
    s1_xts <- keep_s1_xts
    s1_ts <- keep_s1_ts
  } else {
    s1_fc <- stats::ts(c(fc1$model$x,fc1$mean), start=stats::start(s1_ts), frequency=365)
  }


## 2f Removing calendar effects --------
  if (!is.null(reg_create)) {
    if (is.null(dim(hol$mhol))) {
      cf <- ((arima_reg365$coef[grep("hol", names(arima_reg365$coef))]) * (c(hol$mhol, hol$lhol)))
    } else {
      cf <- rep(0, times=(dim(hol$mhol)[1]+dim(hol$lhol)[1]))
      for (k in 1:dim(hol$mhol)[2]) {
        cf <- cf + ((arima_reg365$coef[grep("hol", names(arima_reg365$coef))][k]) * (rbind(hol$mhol, hol$lhol)[,k]))
      }}
    k1 <- s1_fc - cf
  } else {
    k1 <- s1_fc
  }

  # Regressor Adjustment (regressor)
  if (!is.null(regressor)) {
    if (is.null(dim(as.data.frame(regressor)))) {
      cf <- ((arima_reg365$coef[length(arima_reg365$coef)]) * (c(regressor, forecast_regressor)))
    } else {
      cf <- 0
      for (k in 1:dim(as.data.frame(regressor))[2]) {
        cf <- cf + ((arima_reg365$coef[(length(arima_reg365$coef)+1-dim(as.data.frame(regressor))[2]):length(arima_reg365$coef)][k]) * (rbind(matrix(regressor, nrow=nrow(data.frame(regressor))),matrix(forecast_regressor, nrow=nrow(data.frame(forecast_regressor))))[,k]))
      }}

    k1 <- k1 - cf
  } else {
    k1 <- k1
  }

  if(mean_correction) {
    cfac <- s1_fc - k1
    st <- stats::start(cfac)
    sf2x <- suppressWarnings(stats::window(cfac, start=st, end=st+c((stats::end(cfac) - st)[1],0)))
    cfac <- cfac - mean(utils::head(cfac, length(cfac)-1))
    k1 <- Descaler(s1_fc - cfac, Log=Log)
    cal_fac <- ts2xts(cfac)
  } else {
    cal_fac <- ts2xts(s1_fc - k1)
    k1 <- Descaler(k1, Log=Log)}


  if (progress_bar) { utils::setTxtProgressBar(pb, 15/21, label="Estimating Day-Of-Month Effect")}



# Step 3: Adjustment of day-of-the-month effect ---------------------------

  k1_xts <- ts2xts(k1)
  k1_31 <- .fill31(k1_xts, fill="spline")

  k1x <- stats::ts(Scaler(k1_31, Log=Log), frequency=31)

  if (!is.null(s.window2)) {
    stl_2 <- stats::stl(k1x, s.window=s.window2, robust=robust2, t.window=t.window2)

  # Seasonal factors and adjusted series
    sf2 <- stats::ts(stl_2$time.series[,1], start=stats::start(k1_31), freq=372)
    sf2 <- .drop31(sf2, new_start=stats::start(k1)[2], new_end=stats::end(k1)[2])
    sf2 <- xts2ts(sf2, freq=365)

    if(mean_correction) {
      st <- stats::start(sf2)
      ee <- stats::end(sf2)
      ed <- st + c((ee - st)[1],0)
      ed[1] <- ifelse(ed[1]+ed[2]/31 > ee[1]+ee[2]/31,  ed[1]-1, ed[1])
      sf2x <- stats::window(sf2, start=st, end=ed)
      sf2 <- sf2 - mean(utils::head(sf2x, length(sf2x)-1))
      s2 <- Descaler(Scaler(k1, Log=Log) - sf2, Log=Log)
      s2_fac <- ts2xts(sf2)
    } else {
      s2_fac <- ts2xts(sf2)
      s2 <- Descaler(Scaler(k1, Log=Log) - sf2, Log=Log)}
  } else {
    stl_2 <- NULL
    s2 <- k1
    s2_fac <- ts2xts(s2*0)
  }


  if (progress_bar) {utils::setTxtProgressBar(pb, 16/21, label="Estimating Day-Of-Year Effect")}


# Step 4: Adjustment of day-of-the-year effect ----------------------------

   if (is.null(outer3)){
    outerval = if(robust3) 15 else 0
  } else {outerval=outer3}

  if (is.null(inner3)){
    innerval = if(robust3) 1 else 2
  } else {innerval=inner3}

  s2x <- s2x_unchanged <- Scaler(s2, Log=Log)
  if (is.null(s.window3)) {
    stl_3 <- NULL
    s3 <- s2
    s3_fac <- s3*0 + ifelse(Log, 1, 0)
  } else {
    stl_3 <- stats::stl(s2x, s.window=s.window3, robust=robust3, outer=outerval, inner=innerval, t.window=t.window3)
    sf3 <- stl_3$time.series[,1]

    if (!is.null(reiterate3)) {
      for (i in 1:reiterate3) {
        s2x <- s2x - stl_3$time.series[,1]
        stl_3 <- stats::stl(s2x, s.window=s.window3, robust=robust3, outer=outerval, inner=innerval, t.window=t.window3)
        sf3 <- sf3 + stl_3$time.series[,1]

      }

      stl_3$time.series[,1] <- sf3
      stl_3$time.series[,3] <- s2x_unchanged - sf3 - stl_3$time.series[,2]
      }


    if(mean_correction) {
      st <- stats::start(sf3)
      ee <- stats::end(sf3)
      ed <- st + c((ee - st)[1],0)
      ed[1] <- ifelse(ed[1]+ed[2]/365 > ee[1]+ee[2]/365,  ed[1]-1, ed[1])
      sf3x <- stats::window(sf3, start=st, end=ed)
      sf3 <- sf3 - mean(utils::head(sf3x, length(sf3x)-1))
      s3 <- Descaler(s2x_unchanged - sf3, Log=Log)
      s3_fac <- ts2xts(sf3)
    } else {
      s3_fac <- ts2xts(sf3)
      s3 <- Descaler(s2x_unchanged - sf3, Log=Log)
    }
  }

  if (progress_bar) {utils::setTxtProgressBar(pb, 16/21, label="Preparing set of final time series")}


# Forecasting original data and weekday effect ----------------------------

  times <- as.POSIXlt(seq.Date(from=as.Date(xts::first(zoo::index(original_series))), to=as.Date(xts::last(zoo::index(original_series)))+h, by="days"))
  attr(times, "tzone") <- "GMT"; attr(zoo::index(original_series), "tzone") <-"GMT"
  Fill <- xts::xts(rep(NA, times=length(times)), order.by=times)
  Fill <- Fill[paste(xts::first(zoo::index(s1_xts)), "/", sep="")]

  s1_x <- ts2xts(s1_fc)
  s1_x <- xts::merge.xts(s1_x, Fill)$s1_x

  outlier_effect2 <- stats::ts(suppressWarnings(as.numeric(outlier_effect)), start=stats::start(s1_ts), freq=365)
  outliers <- ts2xts(outlier_effect2)
  outliers <- xts::merge.xts(outliers, Fill)$outliers
  outliers[is.na(outliers)] <- xts::last(outliers[!is.na(outliers)])

  #Extrapolating s1-seasonal factors
if(!is.null(s.window1)){
  sf_x <- .day_split(sf1, use="ets", h=h)$sf
  times <- as.POSIXlt(seq.Date(from=as.Date(xts::first(zoo::index(s1_xts))), length.out=length(sf_x), by="days"))
  attr(times, "tzone") <- "GMT"; attr(zoo::index(original_series), "tzone") <-"GMT"
  s1_fac <- xts::xts(sf_x, order.by=times)


  original_forecast <- s1_x + outliers + s1_fac
  } else {
    original_forecast <- s1_x + outliers
    s1_fac <- original_forecast*0 + ifelse(Log, 1, 0)
  }

  original_forecast <- original_forecast - as.numeric(original_forecast[1]) + as.numeric(original_series_7[1]) # Small rounding difference can become very extreme, if a multiplicative time series is used (due to exp())

  # Filling up 29th Feb
  original_forecast <- .fill_up(original_forecast, Scaler(original_series, Log=Log))

  if (feb29=="sfac" | feb29=="scfac" | feb29=="sc_fac") {
    dates <- seq.Date(from=stats::start(s3_fac), to=stats::end(s3_fac), by="days")
    s3_fac <- zoo::na.spline(xts::merge.xts(s3_fac, xts::xts(seq(length(dates)), dates))$s3_fac)
    s2_fac <- zoo::na.spline(xts::merge.xts(s2_fac, xts::xts(seq(length(dates)), dates))$s2_fac)
    cal_fac <- zoo::na.spline(xts::merge.xts(cal_fac, xts::xts(seq(length(dates)), dates))$cal_fac)
  }


# Final series ------------------------------------------------------------

  s1_xts <- Descaler(original_forecast - s1_fac, Log=Log)
  k1_xts <- Descaler(original_forecast - s1_fac - cal_fac, Log=Log)
  s2_xts <- Descaler(original_forecast - s1_fac - cal_fac - s2_fac, Log=Log)
  s3_xts <- Descaler(original_forecast - s1_fac - cal_fac - s2_fac - s3_fac, Log=Log)

  sa_result <- xts::merge.xts(Descaler(original_forecast, Log=Log), s1_xts,k1_xts, s2_xts, s3_xts)
  colnames(sa_result) <- c("original", "s1_adjusted", "s1k1_adjusted", "s1k1s2_adjusted", "seas_adj")


  s1_only_xts <- Descaler(original_forecast - s1_fac, Log=Log)
  k1_only_xts <- Descaler(original_forecast - cal_fac, Log=Log)
  s2_only_xts <- Descaler(original_forecast - s2_fac, Log=Log)
  s3_only_xts <- Descaler(original_forecast - s3_fac, Log=Log)

  sa_result2 <- xts::merge.xts(Descaler(original_forecast, Log=Log), s1_only_xts,k1_only_xts, s2_only_xts, s3_only_xts)
  colnames(sa_result2) <- c("original", "s1_adjusted", "k1_adjusted", "s2_adjusted", "s3_adjusted")


  # Trend estimation
  n_t = ceiling(365/(12/trend_month)) # trend_month=number of months the trend should cover
  if (n_t - n_t %/% 2 * 2 == 0) {n_t <- n_t + 1}
  fraction = n_t / length(s3_xts)

  trend <- xts::xts(stats::predict(stats::loess(s3_xts ~ seq.int(length(s3_xts)), span=fraction), newdata=seq.int(length(s3_xts))), order.by=zoo::index(s3_xts))


# Final output ------------------------------------------------------------

  output <- xts::merge.xts(s3_xts, Descaler(original_forecast, Log=Log), Descaler(s1_fac+cal_fac+s2_fac+s3_fac, Log=Log), trend)
  names(output) <- c("seas_adj", "original", "sc_fac", "trend")

  sfac_result <- Descaler(xts::merge.xts(s1_fac, cal_fac, s2_fac, s3_fac), Log=Log)
  zoo::index(sfac_result) <- as.Date(zoo::index(sfac_result))

  if (feb29=="sa") {
    Which <- format(zoo::index(output$seas_adj), "%m-%d")=="02-29"
    output$seas_adj[Which] <- NA
    output$seas_adj <- zoo::na.spline(output$seas_adj)
    output$sc_fac[Which] <- Descaler(Scaler(output$original[Which], Log=Log) - Scaler(output$seas_adj[Which], Log=Log), Log=Log)
    sfac_result$s3_fac[Which] <- Descaler(Scaler(output$sc_fac[Which], Log=Log) - Scaler(sfac_result$s1[Which], Log=Log), Log=Log)
    sa_result$seas_adj[Which]<- output$seas_adj[Which]
  }

  # Rescale the data (if they were scaled)
  if(max(abs(series), na.rm=TRUE) >1e5  & !Log) {
    sa_result <- sa_result * scaler
    sa_result2 <- sa_result2 * scaler
    sfac_result <- sfac_result * scaler
    output <- output * scaler

    stl_1$time.series <- stl_1$time.series*scaler
    stl_2$time.series <- stl_2$time.series*scaler
    stl_3$time.series <- stl_3$time.series*scaler
  }

   # Final information and object collection

  info <- c(tranformation=ifelse(Log, "Log", "Level"), cval=cval, h=h)

  finout <- list(output=output, fourier_terms=which.min(ic_value), reg=arima_reg365, info=info, stl=list(stl_1, stl_2, stl_3), outlier=ol, sa_result=sa_result, sa_result2=sa_result2, sfac_result=sfac_result)
  class(finout) <- "daily"
  if (progress_bar) {utils::setTxtProgressBar(pb, 21/21, label="Done"); close(pb)}

  if(cval_new > cval & !inherits(ol, "error")) {message(paste("No outlier results could be computed with the defined critical value. The critical value for outlier adjustment has automatically been set to", cval_new))}

  return(finout)

}
