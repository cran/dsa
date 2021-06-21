.is.leapyear <- function(x) {
  year <- as.numeric(x)
  if((year %% 4) == 0) {
    if((year %% 100) == 0) {
      if((year %% 400) == 0) {
        out <- TRUE
      } else {out <- FALSE}
    } else {out <- TRUE }
  } else {out <- FALSE}
  
  return(out)
}

.drop31 <- function(x_ts, new_start=335, new_end=55) {
  
  a <- paste(expand.grid(1:31, 1:12, stats::start(x_ts)[1]:stats::end(x_ts)[1])$Var3, ifelse(expand.grid(01:31, 1:12, stats::start(x_ts)[1]:stats::end(x_ts)[1])$Var2<9.5, paste("0", expand.grid(01:31, 1:12,stats::start(x_ts)[1]:stats::end(x_ts)[1])$Var2, sep=""),expand.grid(01:31, 1:12, stats::start(x_ts)[1]:stats::end(x_ts)[1])$Var2), ifelse(expand.grid(01:31, 1:12, stats::start(x_ts)[1]:stats::end(x_ts)[1])$Var1<9.5, paste("0", expand.grid(01:31, 1:12, stats::start(x_ts)[1]:stats::end(x_ts)[1])$Var1, sep=""),expand.grid(01:31, 1:12, stats::start(x_ts)[1]:stats::end(x_ts)[1])$Var1) , sep="-")
  
  st <- base::as.Date(paste(stats::start(x_ts)[1], new_start), format="%Y %j")
  en <- base::as.Date(paste(stats::end(x_ts)[1], new_end), format="%Y %j")
  
  fill_date <- a[grep(st, a):grep(en, a)]
  
  round <- 0
  while(length(x_ts) > length(fill_date) & round < 1000) {   
    round <- round + 1 ;                                 
    en <- en + 1; # print(paste(en), 1)
    fill_date <- a[grep(st, a):grep(en, a)];  
  }
  
  while(length(x_ts) < length(fill_date) & round < 1000) {   
    round <- round + 1 ;                                 
    en <- en - 1; # print(paste(en), 1)
    fill_date <- a[grep(st, a):grep(en, a)];  
  }
  
  b <- is.na(as.Date(fill_date))
  long_xts <- xts::xts(x_ts, order.by=zoo::na.locf(base::as.Date(fill_date)))
  
  long_xts <- long_xts[!b]
  
  return(long_xts)
}


.fill31  <- function(x_ts, fill="locf", to_ts=TRUE) {
  a <- paste(expand.grid(1:31, 1:12, format(stats::start(x_ts), "%Y"):format(stats::end(x_ts), "%Y"))$Var3, ifelse(expand.grid(01:31, 1:12, format(stats::start(x_ts), "%Y"):format(stats::end(x_ts), "%Y"))$Var2<9.5, paste("0", expand.grid(01:31, 1:12, format(stats::start(x_ts), "%Y"):format(stats::end(x_ts), "%Y"))$Var2, sep=""),expand.grid(01:31, 1:12, format(stats::start(x_ts), "%Y"):format(stats::end(x_ts), "%Y"))$Var2), ifelse(expand.grid(01:31, 1:12, format(stats::start(x_ts), "%Y"):format(stats::end(x_ts), "%Y"))$Var1<9.5, paste("0", expand.grid(01:31, 1:12, format(stats::start(x_ts), "%Y"):format(stats::end(x_ts), "%Y"))$Var1, sep=""),expand.grid(01:31, 1:12, format(stats::start(x_ts), "%Y"):format(stats::end(x_ts), "%Y"))$Var1) , sep="-")
  
  a <- gsub("02-29", "02-28", a)
  
  fill_date  <- a[grep(stats::start(x_ts), a):grep(stats::end(x_ts), a)]
  
  long_xts <- xts::xts(rep(NA, length(fill_date)), order.by=zoo::na.locf(as.Date(fill_date)))
  
  if (fill=="locf") {
    for (j in 1:length(long_xts)){
      long_xts[j] <- x_ts[zoo::index(x_ts)==zoo::index(long_xts)[j]]
    }}
  
  if (fill=="lin"){
    for (j in 1:length(long_xts)){
      long_xts[j] <- x_ts[zoo::index(x_ts)==zoo::index(long_xts)[j]]
    }
    
    for (k in 2:length(long_xts)){
      if (zoo::index(long_xts)[k-1]==zoo::index(long_xts)[k]) {long_xts[k] <- NA}
    }
    
    a <- xts::xts(long_xts, seq.Date(from=xts::first(zoo::index(long_xts)), by="days", length.out=length(long_xts)))
    long_xts <- xts::xts(zoo::na.approx(a), zoo::index(long_xts))
  }
  
  if (fill=="spline"){
    for (j in 1:length(long_xts)){
      long_xts[j] <- x_ts[zoo::index(x_ts)==zoo::index(long_xts)[j]]
    }  
    
    for (k in 2:length(long_xts)){
      if (zoo::index(long_xts)[k-1]==zoo::index(long_xts)[k]) {long_xts[k] <- NA}
    } 
    
    a <- xts::xts(long_xts, seq.Date(from=xts::first(zoo::index(long_xts)), by="days", length.out=length(long_xts)))
    long_xts <- xts::xts(zoo::na.spline(a), zoo::index(long_xts))
  }
  
  
  if (to_ts) {
    return(stats::ts(as.numeric(long_xts), start=c(as.integer(format(xts::first(zoo::index(x_ts)), "%Y")), (as.integer(format(zoo::index(x_ts[1]), "%m"))-1)*31+as.integer(format(zoo::index(x_ts[1]), "%d"))), freq=372))
  } else {return(long_xts)}
  
}


.fill_up <- function(fill_up_series=NA, use_series=NA) {
  
  time <- as.POSIXlt(seq.Date(from=as.Date(xts::first(zoo::index(fill_up_series))), to=as.Date(xts::last(zoo::index(fill_up_series))), by="days"))
  attr(time, "tzone") <- "GMT"; attr(zoo::index(fill_up_series), "tzone") <-"GMT"
  Fillx <- xts::xts(rep(NA, times=length(time)), order.by=time)
  if (length(Fillx) != length(fill_up_series)) {fill_up_series <- merge(fill_up_series, Fillx)[,1]}
  
  a <-zoo::index(fill_up_series[is.na(fill_up_series)])
  
  if (any(as.Date(a) > as.Date(stats::end(use_series)))) {
    for (m in 1:length(a)) {
      if (as.Date(a[m]) > as.Date(stats::end(use_series))) {a[m] <- NA}}
    a <- a[!is.na(a)]
    #fill_up_series[as.Date(zoo::index(fill_up_series))==as.Date(a)] <- use_series[as.Date(zoo::index(use_series))==as.Date(a)]  
    suppressWarnings(fill_up_series[as.Date(zoo::index(fill_up_series)) %in% as.Date(a)] <- use_series[as.Date(zoo::index(fill_up_series)) %in% as.Date(a)])  
    fill_up_series <- zoo::na.spline(fill_up_series)  
  } else {
    #suppressWarnings(fill_up_series[as.Date(zoo::index(fill_up_series))==as.Date(a)] <- use_series[as.Date(zoo::index(use_series))==as.Date(a)])  
    suppressWarnings(fill_up_series[as.Date(zoo::index(fill_up_series)) %in% as.Date(a)] <- use_series[as.Date(zoo::index(fill_up_series)) %in% as.Date(a)])  
  }
  
  return(fill_up_series)
}


.add <- function(x, y, ...) {
  x[is.na(x)] <- 0
  y[is.na(y)] <- 0
  
  dots <- list(...)
  
  for (j in dots){
    a <- j
    a[is.na(a)] <- 0
    x <- x+a
  }
  
  return(x+y)
}


.day_split <- function(series=NULL, use="heur", h=365) {
  
  if (stats::frequency(series)!=7) {stop("This function is intended for working with weekdays. The input has to be a time series with frequency 7")}
  
  if (use=="heur") {
    for (j in 0:6) {
      a <- series[round(zoo::index(series)-trunc(zoo::index(series)), digits=3)==round(j/7, digits=3)]
      for (k in 1:53) {
        a <- c(a, (xts::last(a)+(xts::last(a)-xts::last(a, 2)[1])*0.5))  # Umso höher der Faktor (hier 0.5) gesetzt wird, umso mehr ähnelt es in der Tendenz dem Ergebnis von use=="ets.
      }
      assign(paste("sf", j, sep=""), a )
    }}
  
  if (use=="ets") {
    for (j in 0:6) {
      assign("a", series[round(zoo::index(series)-trunc(zoo::index(series)), digits=3)==round(j/7, digits=3)])
      fit <- forecast::ets(a, model="AAN")
      b <- forecast::forecast(fit, h=ceiling(h/7))
      
      assign(paste("sf", j, sep=""), c(b$x, b$mean))
    }}
  
  maxx <- max(length(sf0), length(sf1), length(sf2), length(sf3), length(sf4), length(sf5), length(sf6))
  if (length(sf0)<maxx) {sf0 <- c(sf0, NA)}
  if (length(sf1)<maxx) {sf1 <- c(sf1, NA)}
  if (length(sf2)<maxx) {sf2 <- c(sf2, NA)}
  if (length(sf3)<maxx) {sf3 <- c(sf3, NA)}
  if (length(sf4)<maxx) {sf4 <- c(sf4, NA)}
  if (length(sf5)<maxx) {sf5 <- c(sf5, NA)}
  if (length(sf6)<maxx) {sf6 <- c(sf6, NA)}
  
  if (xts::first(round(zoo::index(series)-trunc(zoo::index(series)), digits=3))==round(0/7, digits=3)) { sf <- utils::stack(data.frame(t(as.matrix(cbind(sf0, sf1, sf2, sf3, sf4, sf5, sf6)))))$values} 
  if (xts::first(round(zoo::index(series)-trunc(zoo::index(series)), digits=3))==round(1/7, digits=3)) { sf <- utils::stack(data.frame(t(as.matrix(cbind(sf1, sf2, sf3, sf4, sf5, sf6, sf0)))))$values} 
  if (xts::first(round(zoo::index(series)-trunc(zoo::index(series)), digits=3))==round(2/7, digits=3)) { sf <- utils::stack(data.frame(t(as.matrix(cbind(sf2, sf3, sf4, sf5, sf6, sf0, sf1 )))))$values} 
  if (xts::first(round(zoo::index(series)-trunc(zoo::index(series)), digits=3))==round(3/7, digits=3)) { sf <- utils::stack(data.frame(t(as.matrix(cbind(sf3, sf4, sf5, sf6, sf0, sf1, sf2)))))$values} 
  if (xts::first(round(zoo::index(series)-trunc(zoo::index(series)), digits=3))==round(4/7, digits=3)) { sf <- utils::stack(data.frame(t(as.matrix(cbind(sf4, sf5, sf6, sf0, sf1, sf2, sf3)))))$values} 
  if (xts::first(round(zoo::index(series)-trunc(zoo::index(series)), digits=3))==round(5/7, digits=3)) { sf <- utils::stack(data.frame(t(as.matrix(cbind(sf5, sf6, sf0, sf1, sf2, sf3, sf4)))))$values} 
  if (xts::first(round(zoo::index(series)-trunc(zoo::index(series)), digits=3))==round(6/7, digits=3)) { sf <- utils::stack(data.frame(t(as.matrix(cbind(sf6, sf0, sf1, sf2, sf3, sf4, sf5)))))$values} 
  
  
  sf <- sf[!is.na(sf)]
  
  list(sf=sf, day1=sf0, day2=sf1, day3=sf2, day4=sf3, day5=sf4, day6=sf5, day7=sf6) }




.df2HTML <- function(dataframe, file) {
  if(file.exists(file)) {file.remove(file)}
  
  the_names <- paste("<td style='text-align:left;'>", rownames(dataframe), "</td>")
  
  change_right <- function(x) {x <- paste("<td style='text-align:right;'>", x, "</td>"); return(x)}
  change_right_header <- function(x) {x <- paste("<th style='text-align:right;'>", x, "</th>"); return(x)}
  
  cnames <- change_right_header(colnames(dataframe))
  dataframe <- apply(dataframe, 2, change_right)
  
  if (is.null(nrow(dataframe))) {
    dataframe <- data.frame(t(dataframe))
  }
  
  nr <- ifelse(is.null(nrow(dataframe)), 1, nrow(dataframe))
  df_out <- cbind(rep("<tr>", nr), the_names, dataframe, rep("</tr>", nr))
  
  header <- c("<table width=600 style='border-collapse:collapse;' class=table_3131 border=0>", "<thead> <tr style='border-bottom:2px solid black;border-top:3px solid black;'> <th style='text-align:center;'> </th>",  cnames   , "</thead>", "<tbody>")
  
  fend <- c("</tbody>", "</table>")
  
  cat(paste(c(header, t(df_out), fend), collapse =""), file=file)
}


.df2HTML <- function(dataframe, file) {
  if(file.exists(file)) {file.remove(file)}
  
  the_names <- paste("<td style='text-align:left;'>", rownames(dataframe), "</td>")
  
  change_right <- function(x) {x <- paste("<td style='text-align:right;'>", x, "</td>"); return(x)}
  change_right_header <- function(x) {x <- paste("<th style='text-align:right;'>", x, "</th>"); return(x)}
  
  cnames <- change_right_header(colnames(dataframe))
  dataframe <- apply(dataframe, 2, change_right)
  
  if (is.null(nrow(dataframe))) {
    dataframe <- data.frame(t(dataframe))
  }
  
  nr <- ifelse(is.null(nrow(dataframe)), 1, nrow(dataframe))
  df_out <- cbind(rep("<tr>", nr), the_names, dataframe, rep("</tr>", nr))
  df_out <- gsub("NA", "&nbsp;", df_out, ignore.case=FALSE)
  
  header <- c("<style> .example_table tr:nth-child(2n+1) { background-color: white; } .example_table tr:nth-child(2n+0) { background-color: #f8f8f8; } </style><table width=600 style='border-collapse:collapse;' class='example_table' border=0>", "<thead> <tr style='border-bottom:2px solid black;border-top:3px solid black;'> <th style='text-align:center;'> </th>",  cnames   , "</thead>", "<tbody>")
  
  fend <- c("</tbody>", "</table>")
  
  cat(paste(c(header, t(df_out), fend), collapse =""), file=file)
}






.Time <- function(from=-10, to=10, dates=timeDate::Easter(2000:2030)) {
  
  Holiday <- function(dates="", shift=0) {
    Holiday_dates <- as.Date(dates)+shift
    
    vec <- rep(1, times=length(Holiday_dates))
    Holiday_ones <- xts::xts(vec, order.by=Holiday_dates)
    
    # Version von xts2ts die jedoch eine Zeitreihe erstellt, die am 1 Januar des Startjahres beginnt und am 31. Dezember des Schlussjahres endet. Bei der Originalen Version von xts2ts wird Startdatum und Enddatum+1 Tag der Inputreihe verwendet.
    xts2ts <- function(series) {
      newTS <-  stats::ts(NA, start=c(as.integer(format(zoo::index(series)[1], "%Y")), 1), end=c(as.integer(format(zoo::index(series)[length(series)], "%Y")), 365),   frequency=365)
      
      delta <- rep(-99, times=length(series))
      
      for (t in 1:length(series)) {
        if (as.integer(format(zoo::index(series)[t], "%Y")) %% 4==0 && timeDate::dayOfYear(timeDate::timeDate(zoo::index(series)[t], FinCenter = "EET")) < 60)
        {delta[t] <- round(as.integer(format(zoo::index(series)[t], "%Y")) + (timeDate::dayOfYear(timeDate::timeDate(zoo::index(series)[t], FinCenter = "CET"))-1)/365, digits=3)  }
        
        if (as.integer(format(zoo::index(series)[t], "%Y")) %% 4==0 && timeDate::dayOfYear(timeDate::timeDate(zoo::index(series)[t], FinCenter = "EET")) > 60)
        {delta[t] <- round(as.integer(format(zoo::index(series)[t], "%Y")) + (timeDate::dayOfYear(timeDate::timeDate(zoo::index(series)[t], FinCenter = "CET"))-2)/365, digits=3)  }   
        
        if (as.integer(format(zoo::index(series)[t], "%Y")) %% 4!=0 )
        {delta[t] <- round(as.integer(format(zoo::index(series)[t], "%Y")) + (timeDate::dayOfYear(timeDate::timeDate(zoo::index(series)[t], FinCenter = "EET"))-1)/365, digits=3)  }
      }  
      
      for (m in 1:length(newTS)) {
        index <- round(zoo::index(newTS)[m], digits=3)
        if(sum(as.numeric(delta==index))==1) {
          newTS[m] <- series[delta == index]
        } }
      newTS
    }
    
    Holiday_dummy <- xts2ts(Holiday_ones)
    Holiday_dummy[is.na(Holiday_dummy)] <- 0 
    
    Holiday_dummy*shift
  }
  
  b <- lapply(a<-from:to, function(x) Holiday(shift=x, dates=dates))
  d <- b[[1]]
  
  if (length(b)>1)
    for (j in 2:length(b)) {
      d <- d + b[[j]]
    }
  
  return(d)
}


.outlier <- function(series, model, cval=5, types=c("AO", "LS", "TC"), delta=0.7, maxit.oloop=1, maxit.iloop=2, holidays=NULL, number.fourier=13) {
  
  o <- tsoutliers::tso(series, cval=cval,types=types, delta=delta, maxit.oloop=maxit.oloop, maxit.iloop=maxit.iloop,  tsmethod="arima", args.tsmethod=list(order=model$arma[c(1,6,2)], xreg=data.frame(stats::ts(forecast::fourier(series, number.fourier), frequency=stats::frequency(series), start=stats::start(series)), stats::ts(holidays, frequency=stats::frequency(series))), method="CSS"))
  
  u <- o$outliers
  if (nrow(u) > 0) {
    xreg_new = tsoutliers::outliers.effects(u, n=length(series), freq=stats::frequency(series))} else {
      xreg_new = NULL
      o$effects <- series*0
    }
  
  
  list(outlier_eff=o$effects, outliers=o$outliers, series_adj=o$yadj, regressors = xreg_new, orig=series)
}


.to_month <- function (x, fun = mean, shift = 0, ...) 
{
  ep <- xts::endpoints(x, on = "months") - shift
  for (j in 1:ncol(x)) {
    outX <- xts::period.apply(x[, j], INDEX = ep[ep >= 0 & 
                                                   ep <= length(x)], FUN = fun, ...)
    if (exists("out", envir = environment(), inherits = FALSE)) {
      out <- merge(out, outX)
    }
    else {
      out <- outX
    }
  }
  colnames(out) <- colnames(x)
  zoo::index(out) <- as.Date(zoo::index(out))
  return(out)
}


