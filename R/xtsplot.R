#' Create a plot for xts series
#' 
#' Creates a plot using an xts series
#' @param xts one or many series
#' @param transform one of "none","diff", "change" (can be abbreviated)
#' @param type either "bar", "bar2" or "line"
#' @param years number of years to include
#' @param names change names of series
#' @param scale by what factor should data be scaled.
#' @param color color of the series
#' @param main title of the plot
#' @param submain subtitle of the plot
#' @param legend alignment of legend. "horizontal" or "vertical"
#' @param textsize scale the size of all the text
#' @param textsize_x scale size of x-axis labels
#' @param textsize_y scale size of y-axis labels
#' @param textsize_legend scale size of legend text
#' @param textsize_title scale size of title
#' @param linesize scale the size of the lines
#' @param WeekOfYear should x axis be week of year
#' @param date_breaks distance between labels (see examples)
#' @param date_labels format of the date label for x-axis
#' @param font font to be used
#' @author Daniel Ollech 
#' @details This function uses the ggplot2 package. 
#' The difference between type="bar" and type="bar2" is that the former produces barcharts with bars of the second series in front of the bars of the first series (and accordingly for more than two series), while "bar2" creates side-by-side barcharts.
#' If a scale is supplied, the data will be divided by this number.
#' @examples x <- xts::xts(rnorm(100), seq.Date(as.Date("2010-01-01"), length.out=100, by="months"))
#' y <- xts::xts(runif(100), seq.Date(as.Date("2010-01-01"), length.out=100, by="months"))
#' xtsplot(x, font="sans")
#' xtsplot(y, transform="diff", type="bar", font="sans")
#' xtsplot(y, font="sans")
#' xtsplot(y, transform="diff", type="bar", date_breaks="24 months", font="sans")
#' xtsplot(merge(x,y), names=c("Gaussian", "Uniform"), main="Simulated series", font="sans")
#' @export


xtsplot <- function(xts, transform="none", type="line", years=NA, scale=1, names=NA, color=NA, main="", legend=NA, textsize=1, textsize_x=NA, textsize_y=NA, textsize_legend=NA, textsize_title=NA, linesize=1.1, WeekOfYear=F, date_breaks=NA, date_labels=NA, submain=NULL, font=NA) {
  if (all(is.na(color))) {
  color <- c("#169aaf", "#f08927", "#44a347", "#e53138", "#0062a1", "#fbbd1a", "#9f5e1c", "#803689")
  if (length(color) < ncol(xts)) {color <- rep(color, ncol(xts)/length(color)*2)}
  color <- color[seq(ncol(xts))]
  }
    
  if (ncol(xts) > 1 & length(color)==1) {color=rep(color, times=ncol(xts))} 
  if (scale==0) {scale <- 1; warning("scale=0 is not possible. Scale was set to 1 instead")}
  
  if (substr(transform,1,1)=="d") {
    data_comp <- diff(xts)[2:nrow(xts),] } 
  if (substr(transform,1,1)=="c" | substr(transform,1,1)=="m" | substr(transform,1,2)=="vm") {
    data_comp <- diff(xts)/stats::lag(xts)[2:nrow(xts),] }
  if (substr(transform,1,1)=="n") {
    data_comp <- xts }
  
  TheDate <- xts::last(as.Date(zoo::index(xts), format="%m/%d/%Y", origin=as.Date("1970-01-01 00:00.00 UTC")))
  if (!is.na(years)) {data_comp <- data_comp[paste(TheDate-years*365.25,"/", TheDate, sep="")]} else {data_comp <- data_comp[paste("/", TheDate, sep="")]}
  
  
  aligner="horizontal"
  if (is.na(legend)){if (any(nchar(names(xts))>40)) {aligner ="vertical"} else {aligner ="horizontal"}} else if (legend=="vertical") {aligner ="vertical"} else if (legend=="horizontal") {aligner ="horizontal"}
  
  if (!is.na(names[1])) {
    if (length(names)!=ncol(xts)) {stop("Number of names provided should correspond to number of time series.")}
    names(xts) <- names
  }
  
  
  if(WeekOfYear) {
    if (dim(table(format(zoo::index(data_comp), "%Y")))==1) {LeFormat <- "%W"} else {LeFormat <- "%Y-%W"}
    time_n <- format(zoo::index(data_comp), LeFormat)
    if (is.na(date_breaks)) {date_labels <- "%Y-%W"}
    if (is.na(date_breaks)) {date_breaks <- "weeks"}
    } else {
      time=as.POSIXlt(zoo::index(data_comp), format="%m/%d/%Y", origin=as.Date("1970-01-01 00:00.00 UTC"))
      time_n <- 1900+time$year+time$yday/366}
  
  gg1 <-reshape2::melt(data.frame(time=time, data_comp), id.var="time")
  l1 <- xts::last(gg1$time) 
  m1 <- min(gg1$value)
  

  if (is.na(font)) {
    if(.Platform$OS.type=="windows") {extrafont::loadfonts(device="win", quiet=T)}
    extrafont::loadfonts(device="pdf", quiet=T)
  thefont <- extrafont::choose_font(c("Segoe UI","Cambria", "Times New Roman", "Arial", "sans"))} else {
    thefont <- font
  }
  
  if(thefont=="") {message("Try using extrafont::font_import() to load all fonts")}
  
  if (is.na(date_breaks)) {
    timedifference <- as.numeric(difftime(xts::last(time), xts::first(time), units="days"))
    if (timedifference > 735) {date_breaks = paste(round(timedifference/(4*365)), "years")
    if (is.na(date_labels)) {date_labels <- "%Y"}}
    if (timedifference < 735 & timedifference > 122) {date_breaks = paste(round(timedifference/(4*30.4)), "months")
    if (is.na(date_labels)) {date_labels <- "%Y-%b"}}
    if (timedifference <= 122 & timedifference > 14) {date_breaks = paste(round(timedifference/(4*7)), "weeks")
    if (is.na(date_labels)) {date_labels <- "%Y-%m-%d"}}
    if (timedifference <= 14 & timedifference > 2) {date_breaks = paste(round(timedifference/(4)), "days")
    if (is.na(date_labels)) {date_labels <- "%Y-%m-%d"}}
    if (timedifference <= 2) {date_breaks = paste(round(timedifference/4*24), "hours")
    if (is.na(date_labels)) {date_labels <- "%Y-%m-%d %H:%M"}}
  }
  
  if (is.na(date_labels)) {
    timedifference <- as.numeric(difftime(xts::last(time), xts::first(time), units="days"))
    if (timedifference > 735) {date_labels <- "%Y"}
    if (timedifference < 735 & timedifference > 122) {date_labels <- "%Y-%b"}
    if (timedifference <= 122 & timedifference > 14) {date_labels <- "%Y-%m-%d"}
    if (timedifference <= 14 & timedifference > 2) {date_labels <- "%Y-%m-%d"}
    if (timedifference <= 2) {date_labels <- "%Y-%m-%d %H:%M"}
  }
  
  if (is.na(textsize_legend)) textsize_legend <- textsize
  if (is.na(textsize_x)) textsize_x <- textsize
  if (is.na(textsize_y)) textsize_y <- textsize
  if (is.na(textsize_title)) textsize_title <- textsize

  
  if (type=="line") {
    gg1_pic <- suppressMessages(ggplot2::ggplot(data=gg1, ggplot2::aes(x=time, y=gg1$value/scale, group=gg1$variable, colour=gg1$variable), environment = environment()) + ggplot2::geom_line(size=linesize) + ggplot2::scale_x_datetime(date_breaks=date_breaks, date_labels=date_labels) + ggplot2::scale_color_manual(values=color, labels=c(names(xts)))  +   ggplot2::theme(axis.title.x=ggplot2::element_blank(), axis.title.y=ggplot2::element_blank(), axis.text.y=ggplot2::element_text(size=9.5*textsize_y, family=thefont), axis.text.x=ggplot2::element_text(size=9.5*textsize_x, family=thefont), plot.title=ggplot2::element_text(size=13*textsize_title, family=thefont, hjust=0), axis.ticks=ggplot2::element_line(colour="white") , legend.title=ggplot2::element_blank(), legend.position="bottom", panel.spacing=grid::unit(0.2, "cm"), panel.background = ggplot2::element_rect(fill = "#DBDBDB"), panel.grid.minor = ggplot2::element_line(colour = "#DBDBDB"), panel.grid.major = ggplot2::element_line(colour = "white", size=0.8), legend.key=ggplot2::element_blank(),legend.background=ggplot2::element_rect(fill="transparent"), plot.background=ggplot2::element_rect(fill="#F0F0F0", color=NA), legend.direction=aligner, legend.text=ggplot2::element_text(size=9.5*textsize_legend), strip.text.x = ggplot2::element_text(size=12*textsize_x, colour="black", family=thefont)) + ggplot2::ggtitle(main)   ) } 
  
  if (type=="bar") {
    gg1_pic <- suppressWarnings(ggplot2::ggplot(data=gg1, ggplot2::aes(x=time, y=gg1$value/scale, group=gg1$variable, fill=gg1$variable), environment = environment()) + ggplot2::geom_bar(stat="identity", position="identity") + ggplot2::scale_x_datetime(date_breaks=date_breaks, date_labels=date_labels)+ ggplot2::scale_fill_manual(values=color, labels=c(names(xts)))  +   ggplot2::theme(axis.title.x=ggplot2::element_blank(), axis.title.y=ggplot2::element_blank(), axis.text=ggplot2::element_text(size=9.5*textsize_y, family=thefont), plot.title=ggplot2::element_text(size=13*textsize_title, family=thefont, hjust=0),  legend.title=ggplot2::element_blank(), legend.position="bottom", panel.spacing=grid::unit(0.2, "cm"), panel.background = ggplot2::element_rect(fill = "#DBDBDB"), axis.ticks=ggplot2::element_line(colour="white") , panel.grid.minor = ggplot2::element_line(colour = "#DBDBDB"), panel.grid.major = ggplot2::element_line(colour = "white", size=0.8), legend.key=ggplot2::element_blank(),legend.background=ggplot2::element_rect(fill="transparent"), plot.background=ggplot2::element_rect(fill="#F0F0F0", color=NA), legend.direction=aligner, legend.text=ggplot2::element_text(size=9.5*textsize_legend)) + ggplot2::ggtitle(main)   ) } 
  
  if (type=="bar2") {
    gg1_pic <- suppressWarnings(ggplot2::ggplot(data=gg1, ggplot2::aes(x=time, y=gg1$value/scale, group=gg1$variable, fill=gg1$variable), environment = environment()) + ggplot2::geom_bar(stat="identity", position="dodge") + ggplot2::scale_x_datetime(date_breaks=date_breaks, date_labels=date_labels) + ggplot2::scale_fill_manual(values=color, labels=c(names(xts)))  +   ggplot2::theme(axis.title.x=ggplot2::element_blank(), axis.title.y=ggplot2::element_blank(), axis.text=ggplot2::element_text(size=9.5*textsize_y, family=thefont), plot.title=ggplot2::element_text(size=13*textsize_title, family=thefont, hjust=0), axis.ticks=ggplot2::element_line(colour="white") ,  legend.title=ggplot2::element_blank(), legend.position="bottom", panel.spacing=grid::unit(0.2, "cm"), panel.background = ggplot2::element_rect(fill = "#DBDBDB"), panel.grid.minor = ggplot2::element_line(colour = "#DBDBDB"), panel.grid.major = ggplot2::element_line(colour = "white", size=0.8), legend.key=ggplot2::element_blank(),legend.background=ggplot2::element_rect(fill="transparent"), plot.background=ggplot2::element_rect(fill="#F0F0F0", color=NA), legend.direction=aligner, legend.text=ggplot2::element_text(size=9.5*textsize_legend)) + ggplot2::ggtitle(main)   ) }  
  
  if (!is.null(submain)) {gg1_pic <- gg1_pic + ggplot2::labs(subtitle=submain) + ggplot2::theme(plot.subtitle=ggplot2::element_text(size=9.5*textsize_title, family=thefont, hjust=0))}
  
  return(gg1_pic)
}






