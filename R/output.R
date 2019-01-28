#' Creating Output for dsa
#' 
#' This function creates HTML output in a specified folder for objects of class daily
#' @param daily.object output of dsa() function
#' @param path Path that HTML file is written to
#' @param short Boolean. If true only short version of output is produced
#' @param SI Including graphs of SI-ratios 
#' @param SI365.seed This seed influences which days of the year are shown as SI-ratios
#' @param spec  Boolean. Inclusion of spectral plots
#' @param outlier Boolean. Inclusion of outlier plots
#' @param Factor Scaling factor for series with large values
#' @param everyDay Boolean. Inclusion of table that summarizes daily results
#' @param seasonals Boolean. Plots of seasonal factors as interactive instead of static graph.
#' @param progressBar Should a progress bar be displayed?
#' @author Daniel Ollech
#' @details This function can be used to create plots and tables necessary for the analysis of seasonally and calendar adjusted daily time series. Uses the output of dsa() as an input.
#' @examples res <- dsa(daily_sim(4)$original, cval=7, model=c(3,1,0),fourier_number = 13, reg.create=NULL)
#' \dontrun{output(res)}
#' @importFrom dygraphs %>%
#' @export

output <- function(daily.object, path=getwd(), short=FALSE, SI=TRUE, SI365.seed=3, spec=TRUE, outlier=TRUE, Factor="auto", everyDay=TRUE, seasonals=FALSE, progressBar=TRUE) {
  
  if (progressBar) {
    total <- 1
    pb <- utils::txtProgressBar(title = "Output for dsa", min = 0, max = total, width = NA, label="Getting started", style=3)
    utils::setTxtProgressBar(pb, 1/21, title="Output for dsa", label="Getting started")}
  
  `%>%` <- dygraphs::`%>%`
  
  if (short) {
  SI <- spec <- outlier <- week <- FALSE
  }
  
  path <- gsub("//", "/", path) 

  name <- as.character(substitute(daily.object))

  if (!dir.exists(paste(path))) {  
    dir.create(paste(path))  } 
  if (!dir.exists(paste(path, "Graphics", sep="/"))) {  
    dir.create(paste(path, "Graphics", sep="/"))  }
  if (!dir.exists(paste(path, "Graphics", name, sep="/"))) {  
    dir.create(paste(path, "Graphics", name, sep="/"))  }
if (!dir.exists(paste(path, "Graphics", name, "Graphics", sep="/"))) {  
dir.create(paste(path, "Graphics", name, "Graphics", sep="/"))  }
  

  if (Factor=="auto") {factor <- ifelse(mean(abs(daily.object$output[,2]), na.rm=T)>1e11, 1e9, ifelse(mean(abs(daily.object$output[,2]), na.rm=T)>1e10, 1e8, ifelse(mean(abs(daily.object$output[,2]), na.rm=T)>1e8, 1e6, ifelse(mean(abs(daily.object$output[,2]), na.rm=T)>1e5, 1e5, ifelse(mean(abs(daily.object$output[,2]), na.rm=T)>1e4, 1e4, ifelse(mean(abs(daily.object$output[,2]), na.rm=T)>1e3, 1e3, 1))))))}
  

  scaling_factor <- ifelse(factor==1e6, ", in millions", ifelse(factor==1e9, ", in billions", ifelse(factor==1e3, ", in thousands", ifelse(factor==1e4, ", in ten thousands", ifelse(factor==1e5, ", in hundred thousands", ifelse(factor==1e8, ", in hundred millions", ""))))))


  
# Objects to be used for output
  if (progressBar) {utils::setTxtProgressBar(pb, 4/21, label="Creating objects for output")}
  
  
  b <- daily.object$output[,c(2,4,1)] %>% dygraphs::dygraph(main="Extended Original Series and Final Seasonally Adjusted Series") %>% dygraphs::dyRangeSelector() %>% dygraphs::dyShading(from=(stats::end(daily.object$output[,2])-365), to=(stats::end(daily.object$output[,2])), color="#E0ECF8") %>% dygraphs::dyOptions(colors = c("black","#006474", "#DD6B00"), strokeWidth=1.2) %>% dygraphs::dyOptions(labelsKMB=T) %>% dygraphs::dyLegend(show="follow")
  
  if (progressBar) {utils::setTxtProgressBar(pb, 4.4/21, label="Creating objects for output")}
  
  htmlwidgets::saveWidget(b, paste(path, "Graphics", name, "Graphics", "finalplot.html", sep="/"))
  
  if (progressBar) {utils::setTxtProgressBar(pb, 4.8/21, label="Creating objects for output")}
  
  a <- daily.object$output[,3] %>% dygraphs::dygraph(main="Final Seasonal and Calendar Factor") %>% dygraphs::dyRangeSelector() %>% dygraphs::dyShading(from=(stats::end(daily.object$output[,3])-365), to=(stats::end(daily.object$output[,3])), color="#E0ECF8") %>% dygraphs::dyOptions(colors = c("#6E6E6E")) %>% dygraphs::dyOptions(labelsKMB=T) %>% dygraphs::dyLegend(show="follow")
  
  if (progressBar) {utils::setTxtProgressBar(pb, 5.4/21, label="Creating objects for output")}
  
  htmlwidgets::saveWidget(a, paste(path, "Graphics", name, "Graphics", "seasonal_factor.html", sep="/"))
  
  if(everyDay) {
    if (progressBar) {utils::setTxtProgressBar(pb, 6/21, label="Main Results Table")}
    
    if(short) {
 a<- daily.object$output[paste("/", zoo::index(xts::last(daily.object$output$seas_adj[!is.na(daily.object$output$seas_adj)])), sep="")]} else {
   a <- daily.object$output[paste("/",xts::last(zoo::index(daily.object$output))-365, sep="")]
 }
    if (daily.object$info[1]=="Log") {
    b <- data.frame(round(a[,c(2,1)]/factor,1), round(a[,c(3)],2))} else {
      b <- data.frame(round(a[,c(2,1)]/factor,1), round(a[,c(3)]/factor,2))
    }

    if (progressBar) {utils::setTxtProgressBar(pb, 7/21, label="Creating objects for output")}
    
    b[,c(1,2)] <- apply(b[,c(1,2)], 2, function(x) sprintf("%.1f",x))
    b[,c(3)] <- sprintf("%.2f",b[,c(3)])
    df2HTML(b[rev(row.names(b)),], file=paste(path, "Graphics", name, "Graphics", "EveryDay.html", sep="/"))
    R2HTML::HTMLCSS(paste(path, "Graphics", name, "Graphics", "EveryDay.html", sep="/"))
    
  }

  
  
  
 
    if (progressBar) {utils::setTxtProgressBar(pb, 8/21, label="Final Seasonal factors graphs")}
    if (short) { endpoint <- base::as.Date(zoo::index(xts::last(daily.object$output$seas_adj[!is.na(daily.object$output$seas_adj)])))  } else {
      endpoint <- base::as.Date(xts::last(zoo::index(daily.object$output)))-365 
    }
    
    if (daily.object$info[1]=="Log") {
    quasi_sfac1 <- daily.object$output[paste("/",endpoint, sep="")][,2]/daily.object$sa_result[paste("/",xts::last(zoo::index(daily.object$output))-365, sep="")][,1]*100} else{
      quasi_sfac1 <- daily.object$output[paste("/",endpoint, sep="")][,2] -daily.object$sa_result[paste("/",xts::last(zoo::index(daily.object$output))-365, sep="")][,1]} 
    quasi_sfac1[format(zoo::index(quasi_sfac1), "%m-%d")=="02-29"] <- NA ### Löschen des 29.2, Langfristig Bessere Lösung notwendig

    monday <- quasi_sfac1[format(zoo::index(quasi_sfac1), "%w")=="1"]
    weeked <- xts::merge.xts(quasi_sfac1, monday) ; colnames(weeked) <- c("Implicit Sfac 1", "Monday")
    
    
    if (daily.object$info[1]=="Log") {
    quasi_sfac2 <- daily.object$sa_result[paste("/",endpoint, sep="")][,2]/daily.object$sa_result[paste("/",endpoint, sep="")][,3]*100} else {
      quasi_sfac2 <- daily.object$sa_result[paste("/",endpoint, sep="")][,2] - daily.object$sa_result[paste("/",endpoint, sep="")][,3]}
    startpoint <- xts::last(zoo::index(daily.object$output)) - 365 - (62+as.numeric(format(xts::last(zoo::index(daily.object$output)), "%d")))
    quasi_sfac2[format(zoo::index(quasi_sfac2), "%m-%d")=="02-29"] <- NA
    first <- quasi_sfac2[format(zoo::index(quasi_sfac2), "%d")=="01"]
    monthed <- xts::merge.xts(quasi_sfac2, first) ; colnames(monthed) <- c("Implicit Sfac 2", "FirstofMonth")
  
    
    if (daily.object$info[1]=="Log") {
    quasi_sfac3 <- daily.object$sa_result[paste("/",endpoint, sep="")][,3]/daily.object$sa_result[paste("/",endpoint, sep="")][,4]*100} else {
      quasi_sfac3 <- daily.object$sa_result[paste("/",endpoint, sep="")][,3] - daily.object$sa_result[paste("/",endpoint, sep="")][,4]}
    
    startpoint <- xts::last(zoo::index(daily.object$output)) - 365 - (365+as.numeric(format(xts::last(zoo::index(daily.object$output)), "%j")))
    colnames(quasi_sfac3) <- c("Implicit Sfac 3")
    
    if(seasonals) {
   
    ##  Graph for weekly seasonal
    weeked_graph <- weeked %>% dygraphs::dygraph() %>% dygraphs::dyRangeSelector(dateWindow=c(endpoint-15, endpoint)) %>% dygraphs::dyOptions(drawPoints=TRUE, pointSize=3, colors=c("green", "#8E0C0C")) %>% dygraphs::dyOptions(labelsKMB=T) %>% dygraphs::dyLegend(show="follow")
    
    ##  Graph for monthly seasonal
    if (short) {startpoint <- xts::last(zoo::index(daily.object$output)) - 365 - (62+as.numeric(format(base::as.Date(zoo::index(xts::last(monthed))), "%d")))
    monthed_graph <- monthed %>% dygraphs::dygraph() %>% dygraphs::dyRangeSelector(dateWindow=c(base::as.Date(zoo::index(xts::last(monthed)))-62, base::as.Date(zoo::index(xts::last(monthed))))) %>% dygraphs::dyOptions(drawPoints=TRUE, pointSize=3, colors=c("green", "#8E0C0C")) %>% dygraphs::dyOptions(labelsKMB=T) %>% dygraphs::dyLegend(show="follow") 
    } else {
      monthed_graph <- monthed %>% dygraphs::dygraph() %>% dygraphs::dyRangeSelector(dateWindow=c(startpoint, endpoint)) %>% dygraphs::dyOptions(drawPoints=TRUE, pointSize=3, colors=c("green", "#8E0C0C")) %>% dygraphs::dyOptions(labelsKMB=T) %>% dygraphs::dyLegend(show="follow")}
    
    ## Graph for annual seasonal
     if (short) {yeared_graph <- quasi_sfac3 %>% dygraphs::dygraph() %>% dygraphs::dyRangeSelector(dateWindow=c(base::as.Date(zoo::index(xts::last(quasi_sfac3)))-366, base::as.Date(zoo::index(xts::last(quasi_sfac3))))) %>% dygraphs::dyOptions(strokeWidth=3, colors="green") %>% dygraphs::dyOptions(labelsKMB=T) %>% dygraphs::dyLegend(show="follow")} else {
    yeared_graph <- quasi_sfac3 %>% dygraphs::dygraph() %>% dygraphs::dyRangeSelector(dateWindow=c(startpoint, endpoint)) %>% dygraphs::dyOptions(strokeWidth=3, colors="green") %>% dygraphs::dyOptions(labelsKMB=T) %>% dygraphs::dyLegend(show="follow")}
    

    htmlwidgets::saveWidget(weeked_graph, paste(path, "Graphics", name, "Graphics", "SeasonalS1.html", sep="/"))
    htmlwidgets::saveWidget(monthed_graph, paste(path, "Graphics", name, "Graphics", "SeasonalS2.html", sep="/"))
    htmlwidgets::saveWidget(yeared_graph, paste(path, "Graphics", name, "Graphics", "SeasonalS3.html", sep="/"))
    } else {
      
      
      
      grDevices::png(paste(path, "/", "Graphics/", name, "/Graphics/", "weekly_sfac.png", sep=""), width=1040, height=260, res=80, type="cairo-png")
      graphics::par(mar=c(3,3,1.5,1))
      how_many = 13
      graphics::plot(xts::last(zoo::index(weeked[,1]), 7*how_many),(xts::last(weeked[,1], 7*how_many)),col=c("#44a347"), lwd=4, ylab="", xlab="", bty="l", cex.axis=1.3, type="l")
      graphics::points(xts::last(zoo::index(weeked[,1]), 7*how_many), (xts::last(weeked[,2], 7*how_many)), pch=21, col="#4B1306", cex=1.2, bg="#44a347", lwd=3.0)
      grDevices::dev.off()
      
      grDevices::png(paste(path, "/", "Graphics/", name, "/Graphics/", "monthly_sfac.png", sep=""), width=1040, height=260, res=80, type="cairo-png")
      graphics::par(mar=c(3,3,1.5,1))
      how_many = 4
      graphics::plot(xts::last(zoo::index(monthed[,1]), 31*how_many),(xts::last(monthed[,1], 31*how_many)),col=c("#9f5e1c"), lwd=4, ylab="", xlab="", bty="l", cex.axis=1.3, type="l")
      graphics::points(xts::last(zoo::index(monthed[,1]), 31*how_many), (xts::last(monthed[,2], 31*how_many)), pch=21, col="#4B1306", cex=1.2, bg="#9f5e1c", lwd=3.0)
      grDevices::dev.off()
      
      grDevices::png(paste(path, "/", "Graphics/", name, "/Graphics/", "annual_sfac.png", sep=""), width=1040, height=260, res=80, type="cairo-png")
      graphics::par(mar=c(3,3,1.5,1))
      how_many = 2
      graphics::plot(xts::last(zoo::index(quasi_sfac3[,1]), 366*how_many),(xts::last(quasi_sfac3[,1], 366*how_many)),col=c("#6A1B09"), lwd=4, ylab="", xlab="", bty="l", cex.axis=1.3, type="l")
      grDevices::dev.off()
      
    }

    final_sa <- daily.object$output[paste("/",endpoint, sep="")][,3]

    if (daily.object$info[1]=="Log") {
      quasi_cal <- daily.object$sa_result[paste("/",endpoint, sep="")][,1]/daily.object$sa_result[paste("/",endpoint, sep="")][,2]*100} else {
        quasi_cal <- daily.object$sa_result[paste("/",endpoint, sep="")][,1] - daily.object$sa_result[paste("/",endpoint, sep="")][,2]}

  
    
    ForBoxplot <- rbind(data.frame(value=as.numeric(quasi_sfac1), type=rep("Intra-weekly", length(quasi_sfac1))), data.frame(value=as.numeric(quasi_sfac2), type=rep("Intra-monthly", length(quasi_sfac2))), data.frame(value=as.numeric(quasi_sfac3), type=rep("Intra-yearly", length(quasi_sfac3))), data.frame(value=as.numeric(quasi_cal), type=rep("Calendar factor", length(quasi_cal))), data.frame(value=as.numeric(final_sa), type=rep("Combined final factor", length(final_sa))))
    
    FB.m <- reshape2::melt(ForBoxplot, id.var="type")
    boxes <- ggplot2::ggplot(data=FB.m[!is.na(FB.m$value),], ggplot2::aes(x=FB.m[!is.na(FB.m$value),]$type, y=FB.m[!is.na(FB.m$value),]$value)) + ggplot2::geom_boxplot(lwd=1, fatten=0.8) + ggplot2::stat_summary(fun.y=mean,size=3, shape=16, col='blue',geom='point') +  ggplot2::theme_minimal() + ggplot2::theme(axis.title.x=ggplot2::element_blank(), axis.title.y=ggplot2::element_blank(), axis.text=ggplot2::element_text(size=16), axis.ticks.x=ggplot2::element_blank())
    suppressMessages(ggplot2::ggsave(filename=paste(path, "Graphics", name, "Graphics", "boxplot.png", sep="/"), width=42, height=18, units="cm", plot=boxes))
  
  
  # R2HTML::HTML Output
  if (progressBar) {utils::setTxtProgressBar(pb, 10/21, label="Begin HTML Output")}
  MyReportBegin <- function(file=paste(path,"/", name, ".html", sep="")) {cat(paste("<html>", "<body bgcolor=#FFFFFF>"), file=file, append=FALSE)}
  MyReportEnd <- function(file=paste(path,"/", name, ".html", sep="")) {cat("<hr size=1></body></html>", file=file, append=TRUE)}
  
  
  R2HTML::HTMLSetFile(paste(path, "/" , name, ".html", sep=""))
  MyReportBegin()
  

  #R2HTML::HTML( R2HTML::as.title(paste("Seasonal Adjustment output for", name)))
  R2HTML::HTML(paste("<p style='font-size:22px'><br><font color='169aaf'>Seasonal Adjustment output for", name, "</font></br></p>"))
  
  R2HTML::HTML(paste("<iframe width=820 height=550 frameborder=0 src='", paste("Graphics", name, "Graphics", "finalplot.html", sep="/"), "' > </iframe>", sep=""))
  
  R2HTML::HTML(paste("<p style='font-size:14px'>For the seasonal adjustment the <font color='blue'>", daily.object$Log[1], "</font> transformation has been used.</p>"))
  
  R2HTML::HTML(paste("<iframe width=820 height=550 frameborder=0 src='", paste("Graphics", name, "Graphics", "seasonal_factor.html", sep="/"), "' > </iframe>", sep=""))
  
  if (everyDay) {
    R2HTML::HTML(paste("<h4>Seasonal Adjustment Results on a Daily Basis", scaling_factor, " </h4>", sep=""))
    R2HTML::HTML("<p style='font-size:14px'>The column <b>original</b> contains the original unadjusted data, <b>seas_adj</b> the final seasonal and moving holiday adjusted series and <b>sc_fac</b> <br> the implicit seasonal and calendar factor.</p>")
  R2HTML::HTML(paste("<iframe width=820 height=550 frameborder=0 src='", paste(path, "Graphics", name, "Graphics", "EveryDay.html", sep="/"), "' > </iframe>", sep=""))  
  }
  
  
  if (seasonals) {
    R2HTML::HTML("<h4>Seasonal Factors of the Day-of-the-Week</h4>")
    R2HTML::HTML("<p style='font-size:14px'>These intra-weekly seasonal factors have been calculated implicitely as the difference respectively the ratio of the original series and <br> the series adjusted for intra-weekly seasonality.</p>")
    R2HTML::HTML(paste("<iframe width=820 height=550 frameborder=0 src='", paste("Graphics", name, "Graphics", "SeasonalS1.html", sep="/"), "' > </iframe>", sep=""))
    
    R2HTML::HTML("<h4>Seasonal Factors of the Day-of-the-Month</h4>")
    R2HTML::HTML("<p style='font-size:14px'>These intra-monthly seasonal factors have been calculated implicitely as the difference respectively the ratio of the calendar adjusted  <br> series and the series adjusted for intra-monthly seasonality.</p>")
    R2HTML::HTML(paste("<iframe width=820 height=550 frameborder=0 src='", paste("Graphics", name, "Graphics", "SeasonalS2.html", sep="/"), "' > </iframe>", sep=""))
    
    R2HTML::HTML("<h4>Seasonal Factors of the Day-of-the-Year</h4>")
    R2HTML::HTML("<p style='font-size:14px'>These intra-weekly seasonal factors have been calculated implicitely as the difference resp. the ratio of the series adjusted for intra-  <br> monthly seasonality and the series adjusted for intra-annual seasonality.</p>")
    R2HTML::HTML(paste("<iframe width=820 height=550 frameborder=0 src='", paste("Graphics", name, "Graphics", "SeasonalS3.html", sep="/"), "' > </iframe>", sep=""))
 
  } else {
    R2HTML::HTML("<h4>Seasonal Factors of the Day-of-the-Week</h4>")
    R2HTML::HTML("<p style='font-size:14px'>These intra-weekly seasonal factors have been calculated implicitely as the difference respectively the ratio of the original series and <br> the series adjusted for intra-weekly seasonality.</p>")
    R2HTML::HTMLInsertGraph(paste(path, "/", "Graphics/", name, "/Graphics/", "weekly_sfac.png", sep=""), Align="left", WidthHTML=800)
    
    R2HTML::HTML("<h4>Seasonal Factors of the Day-of-the-Month</h4>")
    R2HTML::HTML("<p style='font-size:14px'>These intra-monthly seasonal factors have been calculated implicitely as the difference respectively the ratio of the calendar adjusted  <br> series and the series adjusted for intra-monthly seasonality.</p>")
    R2HTML::HTMLInsertGraph(paste(path, "/", "Graphics/", name, "/Graphics/", "monthly_sfac.png", sep=""), Align="left", WidthHTML=800)
    
    R2HTML::HTML("<h4>Seasonal Factors of the Day-of-the-Year</h4>")
    R2HTML::HTML("<p style='font-size:14px'>These intra-weekly seasonal factors have been calculated implicitely as the difference resp. the ratio of the series adjusted for intra-  <br> monthly seasonality and the series adjusted for intra-annual seasonality.</p>")
    R2HTML::HTMLInsertGraph(paste(path, "/", "Graphics/", name, "/Graphics/", "annual_sfac.png", sep=""), Align="left", WidthHTML=800)

   
  }
  
  R2HTML::HTML("<h4>Comparison of the Impact of the Seasonal and Calendar Factors</h4>")
  R2HTML::HTML("<p style='font-size:14px'>The boxplots indicate the variation of the seasonal and calendar factors.</p>")
  R2HTML::HTMLInsertGraph(paste(path, "/", "Graphics/", name, "/Graphics/", "boxplot.png", sep=""), Align="left", WidthHTML=700)
  
  if (!short) {
    if (progressBar) {utils::setTxtProgressBar(pb, 12/21, label="Create STL Output plots")}
  grDevices::png(paste(path, "/", "Graphics/", name, "/Graphics/", name, "_stl_1.png", sep=""), width=840, height=480, type="cairo-png")
  graphics::plot(daily.object$stl[[1]])
  grDevices::dev.off()
  grDevices::png(paste(path, "/", "Graphics/", name, "/Graphics/", name, "_stl_2.png", sep=""), width=840, height=480, type="cairo-png")
  graphics::plot(daily.object$stl[[2]])
  grDevices::dev.off()
  grDevices::png(paste(path, "/", "Graphics/", name, "/Graphics/", name, "_stl_3.png", sep=""), width=840, height=480, type="cairo-png")
  graphics::plot(daily.object$stl[[3]])
  grDevices::dev.off()
  
  
  R2HTML::HTMLhr()
  R2HTML::HTML("<h3>Step 1, Seasonal adjustment of Weekly Cycle, STL</h3>")
  R2HTML::HTML(paste("<p style='font-size:14px'>Results from STL for the adjustment of intra-weekly seasonality with <font color='blue'>s.window: ", as.integer(daily.object$stl[[1]]$win[1]), "</font></p>"))
  
  R2HTML::HTMLInsertGraph(paste(path, "/", "Graphics/", name, "/Graphics/", name, "_stl_1.png", sep=""), Align="left", WidthHTML=700)
  
  
  if (progressBar) {utils::setTxtProgressBar(pb, 13/21, label="RegARIMA Output")}
  R2HTML::HTML("<h3>Step 2, Calendar Adjustment and Forecasting</h3>")
  R2HTML::HTML("<h4>ARIMA Regression Results</h4>")
  R2HTML::HTML(paste("<p style='font-size:14px'>Results include Fourier terms, i.e. sine and cosine type regressors that model monthly and  <br> annual cycles. If they have been specified results for moving holidays are included as well.</p>"))
  reg <- suppressWarnings(data.frame(coefficient=round(daily.object[[3]]$coef, digits=5), s.e.=round(sqrt(diag(daily.object[[3]]$var.coef)), digits=5), t_=rep("", times=length(daily.object[[3]]$coef)),value=round(daily.object[[3]]$coef/sqrt(diag(daily.object[[3]]$var.coef)), digits=1))); rownames(reg) <- gsub("xregforecast::fouriers1_ts, whichminaicc400", "", gsub("^C", "cosine", gsub("^S", "sine", gsub("xregfouriers1_ts, whichminaicc400", "", gsub("\\(", "", gsub("\\)", "", gsub("\\.", "", rownames(reg))))))))
  R2HTML::HTML(reg, align="left")
  
  if (outlier) {
    if (progressBar) {utils::setTxtProgressBar(pb, 15/21, label="Outlier Output")}
  if (inherits(daily.object$outlier, "error")) {
    R2HTML::HTML("<b>Results from Outlier Adjustment</b>")
    R2HTML::HTML("<font color='red'>No outliers adjusted</font>")
  } else if (dim(daily.object$outlier$outliers)[1]==0) {
    R2HTML::HTML("<b>Results from Outlier Adjustment</b>")
    R2HTML::HTML("<font color='#395F39'>No outliers found</font>")
  } else {
    
    R2HTML::HTML("<b>Results from Outlier Adjustment</b>")

    s1_adj <- daily.object$outlier$orig
    
    outl_adj <- stats::ts(rep(NA, length(s1_adj)), start=xts::first(zoo::index(s1_adj)), frequency=stats::frequency(s1_adj))
    outl_adj[daily.object$outlier$outliers$ind] <- s1_adj[daily.object$outlier$outliers$ind]
    
    
    output <- xts::merge.xts(outlier=ts2xts(outl_adj), series=ts2xts(s1_adj), outlier_adjusted=ts2xts(daily.object$outlier$series_adj))

    
    outliergraphics <- output[,1:2] %>% dygraphs::dygraph()  %>% dygraphs::dySeries("outlier", drawPoints=TRUE, strokeWidth=0, pointSize=4, color="red") %>% dygraphs::dySeries("series", strokeWidth=1, color="black") %>% dygraphs::dyRangeSelector() %>% dygraphs::dyOptions(labelsKMB=T) %>% dygraphs::dyLegend(show="follow")
    
    htmlwidgets::saveWidget(outliergraphics, paste(path, "Graphics", name, "Graphics", "outlier.html", sep="/")) 
    R2HTML::HTML(paste("<iframe width=820 height=550 frameborder=0 src='", paste(path, "Graphics", name, "Graphics", "outlier.html", sep="/"), "' > </iframe>", sep=""))  
    
    rownames(daily.object$outlier$outliers) <- NULL
    outlier_data <- cbind(dates=zoo::index(output$outlier[!is.na(output$outlier)]), daily.object$outlier$outliers)
    R2HTML::HTML(outlier_data, align="left")
    
  } 
  }
  
  
  
  R2HTML::HTML("<h3>Step 3, Seasonal adjustment of monthly cycle, STL</h3>")
  R2HTML::HTML(paste("<p style='font-size:14px'>Results from STL for the adjustment of intra-monthly seasonality with <font color='blue'>s.window: ", as.integer(daily.object$stl[[2]]$win[1]), "</font></p>"))
  R2HTML::HTMLInsertGraph(paste(path, "/", "Graphics/", name, "/Graphics/", name, "_stl_2.png", sep=""), Align="left", WidthHTML=700)
  R2HTML::HTML("<h3>Step 4, Seasonal adjustment of annual cycle, STL</h3>")
  R2HTML::HTML(paste("<p style='font-size:14px'>Results from STL for the adjustment of intra-monthly seasonality with <font color='blue'>s.window: ", as.integer(daily.object$stl[[3]]$win[1]), "</font></p>"))
  R2HTML::HTMLInsertGraph(paste(path, "/", "Graphics/", name, "/Graphics/", name, "_stl_3.png", sep=""), Align="left", WidthHTML=700)
  


if (SI) {
  if (progressBar) {utils::setTxtProgressBar(pb, 16/21, label="Weekly SI")}
  sf <- day_split(daily.object$stl[[1]]$time.series[,1], use="ets")
  sf8 <- day_split(daily.object$stl[[1]]$time.series[,1]+daily.object$stl[[1]]$time.series[,3], use="ets")
  

  day_time <- as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+7*0:(length(sf$day1)-1);
  assign(weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1]))), T), xts::xts(sf$day1, order.by=day_time))
  assign(paste(weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1]))), T), "SI", sep=""), xts::xts(sf8$day1, order.by=day_time))

  day_time <- as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+1+7*0:(length(sf$day2)-1);
  assign(weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+1, T), xts::xts(sf$day2, order.by=day_time))
  assign(paste(weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+1, T), "SI", sep=""), xts::xts(sf8$day2, order.by=day_time))

  day_time <- as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+2+7*0:(length(sf$day3)-1);
  assign(weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+2, T), xts::xts(sf$day3, order.by=day_time))
  assign(paste(weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+2, T), "SI", sep=""), xts::xts(sf8$day3, order.by=day_time))

  day_time <- as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+3+7*0:(length(sf$day4)-1);
  assign(weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+3, T), xts::xts(sf$day4, order.by=day_time))
  assign(paste(weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+3, T), "SI", sep=""), xts::xts(sf8$day4, order.by=day_time))

  day_time <- as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+4+7*0:(length(sf$day5)-1);
  assign(weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+4, T), xts::xts(sf$day5, order.by=day_time))
  assign(paste(weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+4, T), "SI", sep=""), xts::xts(sf8$day5, order.by=day_time))

  day_time <- as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+5+7*0:(length(sf$day6)-1);
  assign(weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+5, T), xts::xts(sf$day6, order.by=day_time))
  assign(paste(weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+5, T), "SI", sep=""), xts::xts(sf8$day6, order.by=day_time))

  day_time <- as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+6+7*0:(length(sf$day7)-1);
  assign(weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+6, T), xts::xts(sf$day7, order.by=day_time))
  assign(paste(weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+6, T), "SI", sep=""), xts::xts(sf8$day7, order.by=day_time))

  
  a1 <- xts::merge.xts(eval(parse(text=weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1]))), T))), eval(parse(text=paste(weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1]))), T), "SI", sep=""))))
  a2 <- xts::merge.xts(eval(parse(text=weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+1, T))), eval(parse(text=paste(weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+1, T), "SI", sep=""))))
  a3 <- xts::merge.xts(eval(parse(text=weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+2, T))), eval(parse(text=paste(weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+2, T), "SI", sep=""))))
  a4 <- xts::merge.xts(eval(parse(text=weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+3, T))), eval(parse(text=paste(weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+3, T), "SI", sep=""))))
  a5 <- xts::merge.xts(eval(parse(text=weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+4, T))), eval(parse(text=paste(weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+4, T), "SI", sep=""))))
  a6 <- xts::merge.xts(eval(parse(text=weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+5, T))), eval(parse(text=paste(weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+5, T), "SI", sep=""))))
  a7 <- xts::merge.xts(eval(parse(text=weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+6, T))), eval(parse(text=paste(weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+6, T), "SI", sep=""))))
 
  names(a1) <- c(weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1]))), T), paste(weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1]))), T), "SI", sep=""))
  names(a2) <- c(weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+1, T), paste(weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+1, T), "SI", sep=""))
  names(a3) <- c(weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+2, T), paste(weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+2, T), "SI", sep=""))
  names(a4) <- c(weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+3, T), paste(weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+3, T), "SI", sep=""))
  names(a5) <- c(weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+4, T), paste(weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+4, T), "SI", sep=""))
  names(a6) <- c(weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+5, T), paste(weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+5, T), "SI", sep=""))
  names(a7) <- c(weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+6, T), paste(weekdays(as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+6, T), "SI", sep=""))
  
a_graph <-  xts::merge.xts(a1[,1], a2[,1], a3[,1], a4[,1],a5[,1], a6[,1],a7[,1])
  
  
  all_plot1 <- function(x=a_graph, main="") {
    Range = c(min(x, na.rm=T), max(x, na.rm=T))
    Range <- c(Range[1]-0.1*(Range[2]-Range[1]), Range[2]+0.1*(Range[2]-Range[1]))
    graphics::plot(zoo::index(x),zoo::na.locf(x[,1]),col=c("#D23310"), lwd=3.4, ylab="", xlab="", bty="l", cex.main=1.7, cex.axis=1.7, ylim=Range, main=main, type="l")
    Colors = c("","#F2961E", "#FFE400", "#0051FF", "#001645", "#D21D78", "#6A0888")
    for (j in 2:7) {
      graphics::lines(zoo::index(x),zoo::na.locf(x[,j]),col=Colors[j], lwd=3.4)
    }}
  
  give_plot1 <- function(x=a1, a=daily.object, main="", col="red") {
    use_dates <- paste0("/", as.Date(stats::end(a$output[,2]))-as.numeric(a$info[3]))
    graphics::plot(zoo::index(x[,2][use_dates]),as.numeric(x[,2][use_dates]), pch=21, type="p", ylab="", xlab="", cex=1.5, col=c("black"), bg="#166F07", bty="l", main=main, cex.main=1.7, cex.axis=1.7)
    graphics::lines(zoo::index(x[,2][use_dates]), as.numeric(x[,1][use_dates]), col=col, lwd=4) }
  
  
  grDevices::png(paste(path, "/", "Graphics/", name, "/Graphics/", "weekly_SI.png", sep=""), width=840, height=840, type="cairo-png")
  graphics::par(mfrow=c(4,2), mar=c(2.3,2.8,1.3,1))
 
  all_plot1(a_graph, main="Seasonal Factors")
  Which <- c("a1", "a2", "a3", "a4", "a5", "a6", "a7")
  Colors = c("#D23310","#F2961E", "#FFE400", "#0051FF", "#001645", "#D21D78", "#6A0888")
  for (k in 1:7) {
    give_plot1(get(Which[k]), daily.object, main=names(get(Which[k]))[1], col=Colors[k])
  }
  
  grDevices::dev.off()
 

  if (daily.object$info[2]>0) {textdiff <- " Series has been differenced for the STL adjustment. "} else {
    textdiff <- " Series has not been differenced for the STL adjustment. "}
  
  R2HTML::HTML("<h3>SI graphs weekly</h3>")
  R2HTML::HTML(paste("<p style='font-size:14px'>Intra-weekly seasonal factor with <font color='blue'>s.window: ", as.integer(daily.object$stl[[1]]$win[1]), ".", textdiff, "</font></p>", sep=""))
  R2HTML::HTMLInsertGraph(paste(path, "/", "Graphics/", name, "/Graphics/", "weekly_SI.png", sep=""), Align="left", WidthHTML=800) 
  


}  


if (SI) {
  if (progressBar) {utils::setTxtProgressBar(pb, 18/21, label="Monthly SI")}
    ss <- daily.object$stl[[2]]$time.series[,1]
    timer <- unique(round(zoo::index(ss)-floor(zoo::index(ss)), 3))
    timer <- base::sort(timer)
    
    monthly <- list()
    for (j in 1:length(timer)) {
      monthly[[j]] <- xts::xts(ss[(round(zoo::index(ss)-floor(zoo::index(ss)),3))==timer[j]], order.by=as.Date.numeric(1:length(ss[(round(zoo::index(ss)-floor(zoo::index(ss)),3))==timer[j]]), origin="1970-01-01") )
    }
    
    tt <- daily.object$stl[[2]]$time.series[,1] + daily.object$stl[[2]]$time.series[,3]
    timer <- unique(round(zoo::index(tt)-floor(zoo::index(tt)), 3))
    timer <- base::sort(timer)
    
    monthly2 <- list()
    for (j in 1:length(timer)) {
      monthly2[[j]] <- xts::xts(tt[(round(zoo::index(tt)-floor(zoo::index(tt)),3))==timer[j]], order.by=as.Date.numeric(1:length(tt[(round(zoo::index(tt)-floor(zoo::index(tt)),3))==timer[j]]), origin="1970-01-01") )
    }
    
    monthly_p <- monthly[[1]]
    for (k in 2:31) {
      monthly_p <-  xts::merge.xts(monthly_p, monthly[[k]])
    }
    
    monthly_q <- monthly2[[1]]
    for (l in 2:31) {
      monthly_q <-  xts::merge.xts(monthly_q, monthly2[[l]])
    }
    
    monthly_p <- monthly_p #* 10e9
    monthly_q <- monthly_q #* 10e9
    
    names(monthly_p) <- as.character(1:31)
    zoo::index(monthly_p) <- xts::first(zoo::index(daily.object[[1]][,1]))+0:(length(monthly_p[,1])-1)*30.4  
    zoo::index(monthly_q) <- xts::first(zoo::index(daily.object[[1]][,1]))+0:(length(monthly_q[,1])-1)*30.4
    names(monthly_q) <- as.character(1:31)
    
    month1 <- xts::merge.xts(monthly_p[,1], monthly_q[,1]); names(month1) <- c("s", "si")
    month7 <- xts::merge.xts(monthly_p[,7], monthly_q[,7]); names(month7) <- c("s", "si")
    month13 <- xts::merge.xts(monthly_p[,13], monthly_q[,13]); names(month13) <- c("s", "si")
    month19 <- xts::merge.xts(monthly_p[,19], monthly_q[,19]); names(month19) <- c("s", "si")
    month25 <- xts::merge.xts(monthly_p[,25], monthly_q[,25]); names(month25) <- c("s", "si")
    month31 <- xts::merge.xts(monthly_p[,31], monthly_q[,31]); names(month31) <- c("s", "si")

    
    all_plot2 <- function(x=monthly_p, main="") {
      Colors = grDevices::colorRampPalette(c("#5D5C5C", "#0848C0", "#C03708"))(31) 
      Range = c(min(x, na.rm=T), max(x, na.rm=T))
      Range <- c(Range[1]-0.1*(Range[2]-Range[1]), Range[2]+0.1*(Range[2]-Range[1]))
      graphics::plot(zoo::index(x),zoo::na.locf(x[,1]),col=Colors[1], lwd=3.4, ylab="", xlab="", bty="l", cex.main=1.7, cex.axis=1.7, ylim=Range, main=main, type="l")
            for (j in 2:31) {
              graphics::lines(zoo::index(x),zoo::na.locf(x[,j]),col=Colors[j], lwd=3.4)
      }}
    
    give_plot2 <- function(x=a1, a=daily.object, main="") {
      use_dates <- paste0("/", as.Date(stats::end(a$output[,2]))-as.numeric(a$info[3]))
      graphics::plot(zoo::index(x[,2][use_dates]),as.numeric(x[,2][use_dates]), pch=21, type="p", ylab="", xlab="", cex=1.5, col=c("black"), bg="#166F07", bty="l", main=main, cex.main=1.7, cex.axis=1.7)
      graphics::lines(zoo::index(x[,2][use_dates]), as.numeric(x[,1][use_dates]), col="#8b0000", lwd=4) }
    
    grDevices::png(paste(path, "/", "Graphics/", name, "/Graphics/", "monthly_SI.png", sep=""), width=840, height=940, type="cairo-png")
    layout_matrix <- matrix(c(1,2,4,6,1,3,5,7), ncol=2, nrow=4)
    graphics::layout(mat=layout_matrix, heights=c(1.2,1,1,1))
    graphics::par(mar=c(2.3,2.8,1.3,1))
    all_plot2(monthly_p, main="Seasonal Factors")
    Which <- c("month1", "month7", "month13", "month19", "month25", "month31")
    for (k in 1:6) {
      give_plot2(get(Which[k]), daily.object, main=Which[k])
    }
    
    grDevices::dev.off()
    
 
    R2HTML::HTML("<h3>SI graphs monthly</h3>")
    R2HTML::HTML(paste("<p style='font-size:14px'>Intra-monthly seasonal factor with <font color='blue'>s.window: ", as.integer(daily.object$stl[[2]]$win[1]), ".", textdiff, "</font></p>", sep=""))
    R2HTML::HTMLInsertGraph(paste(path, "/", "Graphics/", name, "/Graphics/", "monthly_SI.png", sep=""), Align="left", WidthHTML=800) 
    
  }

if (SI) {  
  if (progressBar) {utils::setTxtProgressBar(pb, 20/21, label="Annual SI")}
  ss <- daily.object$stl[[3]]$time.series[,1]
  timer <- unique(round(zoo::index(ss)-floor(zoo::index(ss)), 3))

  timer <- base::sort(timer)  

  annual <- list()
  for (j in 1:length(timer)) {
    annual[[j]] <- xts::xts(ss[(round(zoo::index(ss)-floor(zoo::index(ss)),3))==timer[j]], order.by=as.Date.numeric(1:length(ss[(round(zoo::index(ss)-floor(zoo::index(ss)),3))==timer[j]]), origin="1970-01-01") )
  }
  
  tt <- daily.object$stl[[3]]$time.series[,1] + daily.object$stl[[3]]$time.series[,3]
  timer <- unique(round(zoo::index(tt)-floor(zoo::index(tt)), 3))
  timer <- base::sort(timer)
  
  annual2 <- list()
  for (j in 1:length(timer)) {
    annual2[[j]] <- xts::xts(tt[(round(zoo::index(tt)-floor(zoo::index(tt)),3))==timer[j]], order.by=as.Date.numeric(1:length(tt[(round(zoo::index(tt)-floor(zoo::index(tt)),3))==timer[j]]), origin="1970-01-01") )
  }
  
  annual_p <- annual[[1]]
  for (k in 2:365) {
    annual_p <-  xts::merge.xts(annual_p, annual[[k]])
  }
  
  annual_q <- annual2[[1]]
  for (l in 2:365) {
    annual_q <-  xts::merge.xts(annual_q, annual2[[l]])
  }
  

  names(annual_p) <- as.character(1:365)
  names(annual_q) <- as.character(1:365)
  
  
  
  annual1 <- xts::merge.xts(annual_p[,1], annual_q[,1]); names(annual1) <- c("s", "si"); fac <- as.numeric(ifelse(timeDate::dayOfYear(timeDate::timeDate(xts::first(zoo::index(daily.object[[1]][,1]))))>1, 365, 0)); zoo::index(annual1) <- as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+fac+0:(length(annual1[,1])-1)*365 
 
  annual54 <- xts::merge.xts(annual_p[,54], annual_q[,54]); names(annual54) <- c("s", "si"); fac <- ifelse(timeDate::dayOfYear(timeDate::timeDate(xts::first(zoo::index(daily.object[[1]][,1]))))>54, 365, 0);zoo::index(annual54) <- as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+fac+0:(length(annual54[,1])-1)*365
 
  annual99 <- xts::merge.xts(annual_p[,99], annual_q[,99]); names(annual99) <- c("s", "si"); fac <- ifelse(timeDate::dayOfYear(timeDate::timeDate(xts::first(zoo::index(daily.object[[1]][,1]))))>99, 365, 0);zoo::index(annual99) <- as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+fac+0:(length(annual99[,1])-1)*365
 
  annual130 <- xts::merge.xts(annual_p[,130], annual_q[,130]); names(annual130) <- c("s", "si"); fac <- ifelse(timeDate::dayOfYear(timeDate::timeDate(xts::first(zoo::index(daily.object[[1]][,1]))))>130, 365, 0); zoo::index(annual130) <- as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+fac+0:(length(annual130[,1])-1)*365

  annual196 <- xts::merge.xts(annual_p[,196], annual_q[,196]); names(annual196) <- c("s", "si"); fac <- ifelse(timeDate::dayOfYear(timeDate::timeDate(xts::first(zoo::index(daily.object[[1]][,1]))))>196, 365, 0);zoo::index(annual196) <- as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+fac+0:(length(annual196[,1])-1)*365
 
  annual241 <- xts::merge.xts(annual_p[,241], annual_q[,241]); names(annual241) <- c("s", "si"); fac <- ifelse(timeDate::dayOfYear(timeDate::timeDate(xts::first(zoo::index(daily.object[[1]][,1]))))>241, 365, 0); zoo::index(annual241) <- as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+fac+0:(length(annual241[,1])-1)*365
 
  annual293 <- xts::merge.xts(annual_p[,293], annual_q[,293]); names(annual293) <- c("s", "si"); fac <- ifelse(timeDate::dayOfYear(timeDate::timeDate(xts::first(zoo::index(daily.object[[1]][,1]))))>293, 365, 0); zoo::index(annual293) <- as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+fac+0:(length(annual293[,1])-1)*365
 
  annual311 <- xts::merge.xts(annual_p[,311], annual_q[,311]); names(annual311) <- c("s", "si"); fac <- ifelse(timeDate::dayOfYear(timeDate::timeDate(xts::first(zoo::index(daily.object[[1]][,1]))))>311, 365, 0); zoo::index(annual311) <- as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+fac+0:(length(annual311[,1])-1)*365
 
  annual365 <- xts::merge.xts(annual_p[,365], annual_q[,365]); names(annual365) <- c("s", "si"); fac <- ifelse(timeDate::dayOfYear(timeDate::timeDate(xts::first(zoo::index(daily.object[[1]][,1]))))>365, 365, 0); zoo::index(annual365) <- as.Date(xts::first(zoo::index(daily.object[[1]][,1])))+fac+0:(length(annual365[,1])-1)*365

  
  set.seed(SI365.seed)  
  allannual <- annual_p[,sort(sample(365, 20))] 
  zoo::index(allannual) <- zoo::index(annual1)
  
  all_plot3 <- function(x=allannual, main="") {
    Colors = grDevices::colorRampPalette(c("#615B5B", "#07A513", "#6907A5"))(ncol(x)) 
    Range = c(min(x, na.rm=T), max(x, na.rm=T))
    Range <- c(Range[1]-0.1*(Range[2]-Range[1]), Range[2]+0.1*(Range[2]-Range[1]))
    graphics::plot(zoo::index(x),zoo::na.locf(x[,1]),col=Colors[1], lwd=3.4, ylab="", xlab="", bty="l", cex.main=1.7, cex.axis=1.7, ylim=Range, main=main, type="l")
    for (j in 2:ncol(x)) {
      graphics::lines(zoo::index(x),zoo::na.locf(x[,j]),col=Colors[j], lwd=3.4)
    }}
  
  give_plot3 <- function(x=a1, a=daily.object, main="") {
    use_dates <- paste0("/", as.Date(stats::end(a$output[,2]))-as.numeric(a$info[3]))
    graphics::plot(zoo::index(x[,2][use_dates]),as.numeric(x[,2][use_dates]), pch=21, type="p", ylab="", xlab="", cex=2.5, col=c("black"), bg="#07A513", bty="l", main=main, cex.main=1.7, cex.axis=1.7)
    graphics::lines(zoo::index(x[,2][use_dates]), as.numeric(x[,1][use_dates]), col="#8b0000", lwd=4) }
  
  grDevices::png(paste(path, "/", "Graphics/", name, "/Graphics/", "annual_SI.png", sep=""), width=840, height=920, type="cairo-png")
  layout_matrix <- matrix(c(1,2,5,8,1,3,6,9,1,4,7,10), ncol=3, nrow=4)
  graphics::layout(mat=layout_matrix, heights=c(1.2,1,1,1))
  graphics::par(mar=c(2.3,2.8,1.3,1))
  all_plot3(allannual, main="Seasonal Factors")
  Which <- c("annual1","annual54", "annual99", "annual130", "annual196", "annual241", "annual293", "annual311", "annual365")
  for (k in 1:9) {
    give_plot3(get(Which[k]), daily.object, main=Which[k])
  }
  
  grDevices::dev.off()
  
  
  R2HTML::HTML("<h3>SI graphs annual</h3>")
  R2HTML::HTML(paste("<p style='font-size:14px'>Intra-annual seasonal factor with <font color='blue'>s.window: ", as.integer(daily.object$stl[[3]]$win[1]), ".", textdiff, "</font></p>", sep=""))
 
  R2HTML::HTMLInsertGraph(paste(path, "/", "Graphics/", name, "/Graphics/", "annual_SI.png", sep=""), Align="left", WidthHTML=800)   
  
}
  
  if (spec) {
    if (progressBar) {utils::setTxtProgressBar(pb, 20.6/21, label="Spec")}

    grDevices::png(paste(path, "/", "Graphics/", name, "/Graphics/", name, "_specdiff.png", sep=""), width=720, height=550)
    df <- data.frame(spectrum=(stats::spec.pgram(diff(xts2ts(daily.object$output[,2][paste("/", (as.Date(stats::end(daily.object$output[,2])-365)), sep="")])), plot=F)$spec), freq=stats::spec.pgram(diff(xts2ts(daily.object$output[,2][paste("/", (as.Date(stats::end(daily.object$output[,2])-365)), sep="")])), plot=F)$freq)
    plot1 <- ggplot2::qplot(x=df$freq, y=df$spectrum, data=df, geom="line", log="y") +ggplot2::geom_vline(ggplot2::aes(xintercept=12), colour="#6F87B2", linetype="dotted") +ggplot2::geom_vline(ggplot2::aes(xintercept=24), colour="#6F87B2", linetype="dotted")  +ggplot2::geom_vline(ggplot2::aes(xintercept=365/7), colour="#6F87B2", linetype="dotted") +ggplot2::geom_vline(ggplot2::aes(xintercept=365/7*2), colour="#6F87B2", linetype="dotted") + ggplot2::geom_vline(ggplot2::aes(xintercept=365/7*3), colour="#6F87B2", linetype="dotted") + ggplot2::theme_bw() + ggplot2::theme(panel.grid = ggplot2::element_blank()) + ggplot2::ggtitle("Differenced original series")
    
    df2 <- data.frame(spectrum=(stats::spec.pgram(diff(xts2ts(daily.object$output[,1][paste("/", (as.Date(stats::end(daily.object$output[,1])-365)), sep="")])), plot=F)$spec), freq=stats::spec.pgram(diff(xts2ts(daily.object$output[,1][paste("/", (as.Date(stats::end(daily.object$output[,1])-365)), sep="")])), plot=F)$freq)
    plot2 <- ggplot2::qplot(x=df2$freq, y=df2$spectrum, data=df2, geom="line", log="y") +ggplot2::geom_vline(ggplot2::aes(xintercept=12), colour="#6F87B2", linetype="dotted") +ggplot2::geom_vline(ggplot2::aes(xintercept=24), colour="#6F87B2", linetype="dotted")  +ggplot2::geom_vline(ggplot2::aes(xintercept=365/7), colour="#6F87B2", linetype="dotted") +ggplot2::geom_vline(ggplot2::aes(xintercept=365/7*2), colour="#6F87B2", linetype="dotted") +ggplot2::geom_vline(ggplot2::aes(xintercept=365/7*3), colour="#6F87B2", linetype="dotted") + ggplot2::theme_bw() + ggplot2::theme(panel.grid = ggplot2::element_blank()) + ggplot2::ggtitle("Differenced final adjusted series")
    suppressWarnings(gridExtra::grid.arrange(plot1, plot2, nrow=2)) 
    grDevices::dev.off()
    
    grDevices::png(paste(path, "/", "Graphics/", name, "/Graphics/", name, "_specdiff_xlog.png", sep=""), width=720, height=550)
    df <- data.frame(spectrum=(stats::spec.pgram(diff(xts2ts(daily.object$output[,2][paste("/", (as.Date(stats::end(daily.object$output[,2])-365)), sep="")])), plot=F)$spec), freq=stats::spec.pgram(diff(xts2ts(daily.object$output[,2][paste("/", (as.Date(stats::end(daily.object$output[,2])-365)), sep="")])), plot=F)$freq); df <- df[df$freq>0.8,]
    plot1 <- ggplot2::qplot(x=df$freq, y=df$spectrum, data=df, geom="line", log="xy") +ggplot2::geom_vline(ggplot2::aes(xintercept=1), colour="#3AA625", linetype="dotted") +ggplot2::geom_vline(ggplot2::aes(xintercept=2), colour="#3AA625", linetype="dotted")  +ggplot2::geom_vline(ggplot2::aes(xintercept=3), colour="#3AA625", linetype="dotted") +ggplot2::geom_vline(ggplot2::aes(xintercept=4), colour="#3AA625", linetype="dotted") +ggplot2::geom_vline(ggplot2::aes(xintercept=5), colour="#3AA625", linetype="dotted") +ggplot2::geom_vline(ggplot2::aes(xintercept=6), colour="#3AA625", linetype="dotted") +ggplot2::geom_vline(ggplot2::aes(xintercept=7), colour="#3AA625", linetype="dotted") +ggplot2::geom_vline(ggplot2::aes(xintercept=8), colour="#3AA625", linetype="dotted") +ggplot2::geom_vline(ggplot2::aes(xintercept=9), colour="#3AA625", linetype="dotted") +ggplot2::geom_vline(ggplot2::aes(xintercept=10), colour="#3AA625", linetype="dotted") +ggplot2::geom_vline(ggplot2::aes(xintercept=12), colour="#FFBC00", linetype="dotted") +ggplot2::geom_vline(ggplot2::aes(xintercept=24), colour="#FFBC00", linetype="dotted")  +ggplot2::geom_vline(ggplot2::aes(xintercept=365/7), colour="#DB05D7", linetype="dotted") +ggplot2::geom_vline(ggplot2::aes(xintercept=365/7*2), colour="#DB05D7", linetype="dotted") +ggplot2::geom_vline(ggplot2::aes(xintercept=365/7*3), colour="#DB05D7", linetype="dotted") + ggplot2::theme_bw() + ggplot2::theme(panel.grid = ggplot2::element_blank()) + ggplot2::ggtitle("Differenced original series, x-axis=log")
    
    df2 <- data.frame(spectrum=(stats::spec.pgram(diff(xts2ts(daily.object$output[,1][paste("/", (as.Date(stats::end(daily.object$output[,1])-365)), sep="")])), plot=F)$spec), freq=stats::spec.pgram(diff(xts2ts(daily.object$output[,1][paste("/", (as.Date(stats::end(daily.object$output[,1])-365)), sep="")])), plot=F)$freq) ; df2 <- df[df$freq>0.8,]
    plot2 <- ggplot2::qplot(x=df2$freq, y=df2$spectrum, data=df2, geom="line", log="xy") + ggplot2::geom_vline(ggplot2::aes(xintercept=1), colour="#3AA625", linetype="dotted") + ggplot2::geom_vline(ggplot2::aes(xintercept=2), colour="#3AA625", linetype="dotted")  + ggplot2::geom_vline(ggplot2::aes(xintercept=3), colour="#3AA625", linetype="dotted") + ggplot2::geom_vline(ggplot2::aes(xintercept=4), colour="#3AA625", linetype="dotted") +ggplot2::geom_vline(ggplot2::aes(xintercept=5), colour="#3AA625", linetype="dotted") + ggplot2::geom_vline(ggplot2::aes(xintercept=6), colour="#3AA625", linetype="dotted") + ggplot2::geom_vline(ggplot2::aes(xintercept=7), colour="#3AA625", linetype="dotted") +ggplot2::geom_vline(ggplot2::aes(xintercept=8), colour="#3AA625", linetype="dotted") +ggplot2::geom_vline(ggplot2::aes(xintercept=9), colour="#3AA625", linetype="dotted") +ggplot2::geom_vline(ggplot2::aes(xintercept=10), colour="#3AA625", linetype="dotted") + ggplot2::geom_vline(ggplot2::aes(xintercept=12), colour="#FFBC00", linetype="dotted") + ggplot2::geom_vline(ggplot2::aes(xintercept=24), colour="#FFBC00", linetype="dotted")  +ggplot2::geom_vline(ggplot2::aes(xintercept=365/7), colour="#DB05D7", linetype="dotted") +ggplot2::geom_vline(ggplot2::aes(xintercept=365/7*2), colour="#DB05D7", linetype="dotted") + ggplot2::geom_vline(ggplot2::aes(xintercept=365/7*3), colour="#DB05D7", linetype="dotted") + ggplot2::theme_bw() + ggplot2::theme(panel.grid = ggplot2::element_blank()) + ggplot2::ggtitle("Differenced final adjusted series, x-axis=log")
    suppressWarnings(gridExtra::grid.arrange(plot1, plot2, nrow=2)) 
    grDevices::dev.off()
   
    R2HTML::HTML("<h3>Spectrums of the differenced series</h3>")
    R2HTML::HTMLInsertGraph(paste(path, "/", "Graphics/", name, "/Graphics/", name, "_specdiff.png", sep=""), Align="left", WidthHTML=700) 
    R2HTML::HTMLInsertGraph(paste(path, "/", "Graphics/", name, "/Graphics/", name, "_specdiff_xlog.png", sep=""), Align="left", WidthHTML=700) 
  }
  }
  
  if (progressBar) {utils::setTxtProgressBar(pb, 21/21, label="Done")}
  MyReportEnd() 
  
}