## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(dsa)

## ---- eval=TRUE---------------------------------------------------------------
set.seed(5)
x = dsa::daily_sim(5)$original
res <- dsa(x,  
           reg_create="Easter", reg_dummy=c(-3,4), 
           progress_bar = FALSE) 

## ---- eval=FALSE--------------------------------------------------------------
#  output(res)

## ---- fig.width=7.5, fig.height=4.5-------------------------------------------
plot(res, main="Results of Daily Seasonal Adjustment")

## ---- eval=FALSE--------------------------------------------------------------
#  currency_in_circulation <- zoo::na.locf(zoo::na.locf(daily_data$currency_circulation))
#  
#  reference_series <- currency_in_circulation
#  restrict <- seq.Date(from=stats::start(reference_series),
#                       to=stats::end(reference_series), by="days")
#  restrict_forecast <- seq.Date(from=stats::end(reference_series)+1,
#                                length.out=365, by="days")
#  
#  AllHol <- merge(lag(holidays$GoodFriday, -2), lag(holidays$GoodFriday, -1),
#                  holidays[, c("GoodFriday")], lag(holidays$GoodFriday, 1),
#                  holidays[, c("EasterSunday", "EasterMonday", "EasterMondayAft1Day")],
#                  holidays$AscensionBef1Day, holidays$PentecostMonday)
#  colnames(AllHol) <- c("GoodFriday-2", "GoodFriday-1", "GoodFriday",
#                        "GoodSaturday", "EasterSunday", "EasterMonday",
#                        "EasterMondayAft1Day", "AscensionBef1Day",
#                        "PentecostMonday")
#  
#  AllHolUse <- multi_xts2ts(AllHol[restrict])
#  AllHolForecast <- multi_xts2ts(AllHol[restrict_forecast], short=TRUE)
#  AllHolForecast <- AllHolForecast[,colSums(AllHolUse)!=0]
#  AllHolUse <- AllHolUse[,colSums(AllHolUse)!=0]
#  
#  # Adjustment with DSA
#  cic_sa <- dsa(currency_in_circulation, Log=FALSE,
#                robust1=T, robust3=F,
#                s.window1=53, s.window2=41, s.window3=21,
#                cval=7.0, model_span=3,
#                fourier_number=24,
#                regressor=AllHolUse, forecast_regressor = AllHolForecast,
#                reiterate3=7,
#                feb29="sa",
#                progress_bar = FALSE)
#  

## -----------------------------------------------------------------------------
# Workaround to reduce the compilation time for CRAN. Results obtained from dsa() using the specification above.
cic_sa <- dsa_examples[[1]]
dsa_out <- get_sa(cic_sa)

## ---- fig.width=7.5, fig.height=7.5, eval=TRUE--------------------------------

g1 <- xtsplot(cic_sa$output[,c(2,1)]["2018/2020-05"], 
              color=c("#9c9e9f", "darkred"), 
              main="Original and Seasonally adjusted series", 
              names=c("Original", "Adjusted")) + 
  ggplot2::theme(legend.position = c(0.175, 0.775))
g2 <- xtsplot(cic_sa$sfac_result[,1]["2018/2020-05"], 
              color="#0062a1", 
              main="Intra-weekly seasonal component", 
              linesize=0.3) + 
  ggplot2::theme(legend.position = "None")
g3 <- xtsplot(cic_sa$sfac_result[,2]["2018/2020-05"], 
              color="#0062a1", 
              main="Moving holiday effect") + 
  ggplot2::theme(legend.position = "None")
g4 <- xtsplot(cic_sa$sfac_result[,3]["2018/2020-05"], 
              color="#0062a1", 
              main="Intra-monthly seasonal component") + 
  ggplot2::theme(legend.position = "None")
g5 <- xtsplot(cic_sa$sfac_result[,4]["2018/2020-05"], 
              color="#0062a1", 
              main="Intra-annual seasonal component") + 
  ggplot2::theme(legend.position = "None")
gridExtra::grid.arrange(g1, g2, g3, g4, g5, nrow=5)

## ---- fig.width=7.5, fig.height=4.5, eval=TRUE--------------------------------
knitr::kable(round(data.frame(Estimate=cic_sa$reg$coef, 
                              SE=sqrt(diag(cic_sa$reg$var.coef)))[51:59,]*1000,1))

## ---- fig.width=7.5, fig.height=4.5, eval=TRUE--------------------------------
g1 <- plot_spectrum(cic_sa$output$original, color="darkred") + 
  ggplot2::ggtitle("Differenced original series") + 
  ggplot2::theme(axis.text.x = ggplot2::element_blank(), 
                 axis.title.x = ggplot2::element_blank())

g2 <- plot_spectrum(cic_sa$output$seas_adj, color="darkred") + 
  ggplot2::ggtitle("Differenced seasonally and calendar adjusted series")

gridExtra::grid.arrange(g1, g2)

## ---- fig.width=7.5, fig.height=4.5-------------------------------------------
### Set of functions to output the seasonality tests neatly.

set_of_seastests <- function(x) {
fried365 <- seastests::fried(dsa::xts2ts(x, 365), freq=365)  
qs365 <- seastests::qs(dsa::xts2ts(x, 365), freq=365) 
fried12 <- seastests::fried(dsa:::.to_month(x), freq=12)
qs12 <- seastests::qs(dsa:::.to_month(x), freq=12)
fried7 <- seastests::fried(xts::last(x,70), freq=7)
qs7 <- seastests::qs(xts::last(x,70), freq=7)
fried7_all <- seastests::fried(x, freq=7)
qs7_all <- seastests::qs(x, freq=7)

stats <- round(c(fried365$stat, qs365$stat, fried12$stat, qs12$stat, 
                 fried7$stat, qs7$stat, fried7_all$stat, qs7_all$stat),1)

pvals <- round(c(fried365$Pval, qs365$Pval, fried12$Pval, qs12$Pval, 
                 fried7$Pval, qs7$Pval, fried7_all$Pval, qs7_all$Pval),3)

out <- cbind(stats, pvals)
rownames(out) <- c("Friedman365", "QS365", "Friedman12", "QS12", 
                   "Friedman7", "QS7", "Friedman7all", "QS7all")
colnames(out) <- c("Teststat", "P-value")
return(out)
}

all_seas <- function(x, ...) {
  if (missing(...)) {
    bc <- x
  } else {
    bc <- list(x, ...)
  }
  
  out <- set_of_seastests(bc[[1]])
  if (length(bc)>1) {
    for (j in 2:length(bc)) {
      out <- cbind(out, set_of_seastests(bc[[j]]))
    }
  }
  return(out)
}

## ---- fig.width=7.5, fig.height=4.5-------------------------------------------
knitr::kable(all_seas(zoo::na.locf(daily_data$currency_circulation), dsa_out))

## ---- fig.width=7.5, fig.height=4.5, eval=FALSE-------------------------------
#  no2 <- daily_data$no2
#  no2 <- no2[!is.na(no2)]
#  
#  # Interpolation
#  no2_NA <- xts::xts(as.numeric(ts(no2, frequency=7)), zoo::index(no2))
#  no2_spline <- xts::xts(as.numeric(zoo::na.spline(ts(no2, frequency=7))), zoo::index(no2))
#  no2 <- xts::xts(as.numeric(forecast::na.interp(ts(no2, frequency=7))), zoo::index(no2))
#  
#  # Regressors
#  reference_series <- no2
#  restrict <- seq.Date(from=stats::start(reference_series),
#                       to=stats::end(reference_series), by="days")
#  restrict_forecast <- seq.Date(from=stats::end(reference_series)+1,
#                                length.out=365, by="days")
#  
#  XmasPeriodWeekday=del_names(holidays$XmasPeriodMon+holidays$XmasPeriodTue+
#                                holidays$XmasPeriodWed+holidays$XmasPeriodThu+
#                                holidays$XmasPeriodFri)
#  
#  AllHol <- merge(holidays[, c("GoodFriday", "EasterMonday",
#                               "Ascension", "CorpusChristi",
#                               "PentecostMonday")])
#  AllHolUse <- multi_xts2ts(AllHol[restrict])
#  AllHolForecast <- multi_xts2ts(AllHol[restrict_forecast], short=TRUE)
#  AllHolForecast <- AllHolForecast[,colSums(AllHolUse)!=0]
#  AllHolUse <- AllHolUse[,colSums(AllHolUse)!=0]
#  
#  # Adjustment with DSA
#  no2_sa <- dsa(no2, Log=FALSE, s.window1 = 31, s.window2 = NULL, s.window3 = 13,
#                fourier_number = 1, regressor=AllHolUse,
#                forecast_regressor = AllHolForecast,
#                robust3=FALSE, feb29="sfac", progress_bar = FALSE)

## -----------------------------------------------------------------------------
# Workaround to reduce the compilation time for CRAN. Results obtained from dsa() using the specification above.
no2_sa <- dsa_examples[[3]]
dsa_no2_out <- get_sa(no2_sa)

## ---- fig.width=7.5, fig.height=4.5, eval=TRUE--------------------------------
suppressPackageStartupMessages(library(stR, quietly=TRUE))
suppressPackageStartupMessages(library(forecast, quietly=TRUE))
# STR
no2_cal <- zoo::na.approx(no2_sa$sa_result2$k1_adjusted[zoo::index(na.omit(daily_data$no2))])

deTot_msts <- msts(no2_cal, seasonal.periods=c(7, 365.2524))
str_no2_res <- AutoSTR(deTot_msts)

str_no2_out <- xts::xts(stR::seasadj(str_no2_res), zoo::index(na.omit(daily_data$no2)))

# TBATS
deTot_msts <- msts(no2_cal, seasonal.periods=c(7, 365.2524))
tbats_no2_res <- tbats(deTot_msts)

tbats_no2_out <- xts::xts(as.numeric(seasadj(tbats_no2_res)), zoo::index(na.omit(daily_data$no2)))


## ---- fig.width=7.5, fig.height=7.5-------------------------------------------
g1 <- xtsplot(merge(na.omit(daily_data$no2), tbats_no2_out, str_no2_out, dsa_no2_out)["2015/"], 
        names=c("Original", "TBATS", "STR", "DSA"), color = c("darkgrey", "blue", "orange", "red"),
        main="Comparison of seasonal adjustment result", 
        submain="From 2015",
        linesize=0.75) + 
  ggplot2::theme(legend.position="None")

g2 <- xtsplot(merge(na.omit(daily_data$no2), tbats_no2_out, str_no2_out, dsa_no2_out)["2019/"], 
        names=c("Original", "TBATS", "STR", "DSA"), color = c("darkgrey", "blue", "orange", "red"),
        main="Comparison of seasonal adjustment result", 
        submain="From 2019",
        linesize=0.75) + 
  ggplot2::theme(legend.position="None", plot.title = ggplot2::element_blank())

g3 <- xtsplot(merge(na.omit(daily_data$no2), tbats_no2_out, str_no2_out, dsa_no2_out)["2020-01-01/2020-03-31"], 
        names=c("Original", "TBATS", "STR", "DSA"), color = c("darkgrey", "blue", "orange", "red"),
        main="Comparison of seasonal adjustment result", 
        submain="2020-01-01 to 2020-03-31",
        linesize=0.75) + 
  ggplot2::theme(plot.title = ggplot2::element_blank())

gridExtra::grid.arrange(g1, g2, g3, layout_matrix=matrix(c(1,1,2,2,3,3,3), ncol=1))

## ---- fig.width=7.5, fig.height=4.5-------------------------------------------
knitr::kable(all_seas(na.omit(daily_data$no2), tbats_no2_out, str_no2_out, dsa_no2_out))

## ---- fig.width=7.5, fig.height=4.5, eval=FALSE-------------------------------
#  # Load data
#  elec <- daily_data$elec_consumption
#  elec <- elec[!is.na(elec)]
#  
#  # Regressors
#  reference_series <- elec
#  restrict <- seq.Date(from=stats::start(reference_series),
#                       to=stats::end(reference_series), by="days")
#  restrict_forecast <- seq.Date(from=stats::end(reference_series)+1,
#                                length.out=365, by="days")
#  
#  ReformationDayPost2017 <- del_names(holidays$ReformationDay)
#  ReformationDayPost2017["/2017"] <- 0
#  ReformationDayWedPost2017 <- del_names(holidays$Oct31Wed)
#  ReformationDayWedPost2017["/2017"] <- 0
#  ReformationDayThuPost2017 <- del_names(holidays$Oct31Thu)
#  ReformationDayThuPost2017["/2017"] <- 0
#  ReformationDayFriPost2017 <- del_names(holidays$Oct31Fri)
#  ReformationDayFriPost2017["/2017"] <- 0
#  ReformationDaySatPost2017 <- del_names(holidays$Oct31Sat)
#  ReformationDaySatPost2017["/2017"] <- 0
#  ReformationDayTuePost2017 <- del_names(holidays$Oct31Tue)
#  ReformationDayTuePost2017["/2017"] <- 0
#  ReformationDayMonPost2017 <- del_names(holidays$Oct31Mon)
#  ReformationDayMonPost2017["/2017"] <- 0
#  ReformationDaySunPost2017 <- del_names(holidays$Oct31Sun)
#  ReformationDaySunPost2017["/2017"] <- 0
#  
#  NationalHolWeekday=del_names(holidays$May1Mon+holidays$Oct3Mon+0.6*holidays$Jan6Mon+
#                                 0.1*holidays$Aug15Mon+0.2*ReformationDayMonPost2017+
#                                 0.6*holidays$Nov1Mon+
#                                 holidays$May1Tue+holidays$Oct3Tue+0.6*holidays$Jan6Tue+
#                                 0.1*holidays$Aug15Tue+0.2*ReformationDayTuePost2017+
#                                 0.6*holidays$Nov1Tue+
#                                 holidays$May1Wed+holidays$Oct3Wed+0.6*holidays$Jan6Wed+
#                                 0.1*holidays$Aug15Wed+0.2*ReformationDayWedPost2017+
#                                 0.6*holidays$Nov1Wed+
#                                 holidays$May1Thu+holidays$Oct3Thu+ 0.6*holidays$Jan6Thu+
#                                 0.1*holidays$Aug15Thu+0.2*ReformationDayThuPost2017+
#                                 0.6*holidays$Nov1Thu+
#                                 holidays$May1Fri+holidays$Oct3Fri+0.6*holidays$Jan6Fri+
#                                 0.1*holidays$Aug15Fri+0.2*ReformationDayFriPost2017+
#                                 0.6*holidays$Nov1Fri)
#  
#  NationalHolSat=del_names(holidays$May1Sat+holidays$Oct3Sat+0.6*holidays$Jan6Sat+
#                             0.1*holidays$Aug15Sat+0.2*ReformationDaySatPost2017+
#                             0.6*holidays$Nov1Sat)
#  
#  NationalHolSun=del_names(holidays$May1Sun+holidays$Oct3Sun+0.6*holidays$Jan6Sun+
#                             0.1*holidays$Aug15Sun+0.2*ReformationDaySunPost2017+
#                             0.6*holidays$Nov1Sun)
#  
#  XmasPeriodWeekday=del_names(holidays$XmasPeriodMon+holidays$XmasPeriodTue+
#                                holidays$XmasPeriodWed+holidays$XmasPeriodThu+
#                                holidays$XmasPeriodFri)
#  
#  
#  AllHol <- merge(holidays[, c("CarnivalMonday", "HolyThursday", "GoodFriday",
#                               "HolySaturday", "EasterSunday", "EasterMonday",
#                               "EasterMondayAft1Day", "AscensionBef1Day",
#                               "Ascension", "AscensionAft1Day", "CorpusChristiBef1Day",
#                               "CorpusChristi", "CorpusChristiAft1Day")],
#                  NationalHolWeekday, NationalHolSat, XmasPeriodWeekday,
#                  holidays[, c("ReformationDay2017",  "May1Bridge", "PentecostBef1Day",
#                               "PentecostMonday", "PentecostAft1Day",  "Oct3Bridge",
#                               "Nov1Bridge", "PreXmasSat3d", "Dec24Sat", "Dec25Sat",
#                               "Dec26Sat", "PostXmasSat10d", "PreXmasSun3d", "Dec24Sun",
#                               "Dec25Sun", "Dec26Sun", "PostXmasSun10d")])
#  
#  AllHolUse <- multi_xts2ts(AllHol[restrict])
#  AllHolForecast <- multi_xts2ts(AllHol[restrict_forecast], short=TRUE)
#  AllHolForecast <- AllHolForecast[,colSums(AllHolUse)!=0]
#  AllHolUse <- AllHolUse[,colSums(AllHolUse)!=0]
#  
#  # Adjustment with DSA
#  
#  elec_sa <- dsa(elec, Log=FALSE,
#                 s.window1 = 13, s.window2 = NULL, s.window3 = 13,
#                 fourier_number = 26,
#                 regressor=AllHolUse, forecast_regressor = AllHolForecast,
#                 robust3=FALSE, feb29="sfac",
#                 progress_bar = FALSE)
#  
#  dsa_elec_out <- get_sa(elec_sa)

## -----------------------------------------------------------------------------
# Workaround to reduce the compilation time for CRAN. Results obtained from dsa() using the specification above.
elec_sa <- dsa_examples[[2]]
dsa_elec_out <- get_sa(elec_sa)

## ---- fig.width=7.5, fig.height=7.5-------------------------------------------
g1 <- xtsplot(merge(na.omit(daily_data$elec_consumption), dsa_elec_out)["2015/"]/1e6, 
        names=c("Original", "DSA"), color = c("darkgrey",  "red"),
        main="Seasonal adjustment result", 
        submain="From 2015",
        linesize=0.75) + 
  ggplot2::theme(legend.position="None")

g2 <- xtsplot(merge(na.omit(daily_data$elec_consumption), dsa_elec_out)["2019/"]/1e6, 
        names=c("Original", "DSA"), color = c("darkgrey", "red"),
        main="Seasonal adjustment result", 
        submain="From 2019",
        linesize=0.75) + 
  ggplot2::theme(legend.position="None", plot.title = ggplot2::element_blank())

g3 <- xtsplot(merge(na.omit(daily_data$elec_consumption), dsa_elec_out)["2020-01-01/2020-03-31"]/1e6, 
        names=c("Original", "DSA"), color = c("darkgrey", "red"),
        main="Seasonal adjustment result", 
        submain="2020-01-01 to 2020-03-31",
        linesize=0.75) + 
  ggplot2::theme(plot.title = ggplot2::element_blank())

gridExtra::grid.arrange(g1, g2, g3, layout_matrix=matrix(c(1,1,2,2,3,3,3), ncol=1))


## ---- fig.width=7.5, fig.height=4.5-------------------------------------------
knitr::kable(all_seas(na.omit(daily_data$elec_consumption), dsa_elec_out))

