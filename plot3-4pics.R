library("ggplot2")
library("data.table")

NEI_df <- readRDS("summarySCC_PM25.rds")
SCC_df <- readRDS("Source_Classification_Code.rds")

NEI_dt = data.table(NEI_df)

plot_dt <- NEI_dt[ fips == "24510", sum(Emissions)/1000, by=list(year,type)]
setnames(plot_dt, "V1", "pollutants")
setnames(plot_dt, "type", "source")

plot      <- NEI_dt[ fips == "24510", sum(Emissions)/1000, by=list(year,type)]
setnames(plot, "V1", "pollutants")
setnames(plot, "type", "source")

change_txt <- function(number) {
  if (number > 0) { 
    answer <- "INCREASING " 
  } else if (number < 0) {
    answer <- "decreasing "
  } else {
    answer <- "not changing "
  }
  answer
}

text_label <- function( data_dt, match ) {
  data_dt <- data_dt[ source==match ]
  fit <- lm(pollutants ~ year, data=data_dt)
  slope = round(fit$coef[2], digits=3)
  text = paste(c(match,"\n",change_txt(slope),abs(slope)," Kilotons/year" ),collapse = "")
}

point_dt <- plot[ source=="POINT" ]
point_dt$change <- text_label(plot, "POINT")
nonpoint_dt <- plot[ source=="NONPOINT" ]
nonpoint_dt$change <- text_label(plot, "NONPOINT")
onroad_dt <- plot[ source=="ON-ROAD" ]
onroad_dt$change <- text_label(plot, "ON-ROAD")
nonroad_dt <- plot[ source=="NON-ROAD" ]
nonroad_dt$change <- text_label(plot, "NON-ROAD")

plot_dt <- rbind(point_dt, nonpoint_dt)
plot_dt <- rbind(plot_dt, onroad_dt)
plot_dt <- rbind(plot_dt, nonroad_dt)
plot_dt

graph_change <- function(data_dt) {

  g <- ggplot( data_dt, aes(year, pollutants) )
  p <- g + geom_point()
  p <- p + geom_smooth(method ="lm", se=FALSE) 
  #p <- p + facet_wrap( ~ change, scales="free" ) 
  p <- p + facet_wrap( ~ change ) 
  p <- p + ylab("PM2.5 Emissions (Kilotons)") 
  p <- p + xlab("Year Pollutants Measured")  
  p <- p + ggtitle( "Baltimore Emissions of PM2.5 Pollutants by Source over Time\n") 
  #p <- p + opt( title = change))
  print(p)
}
graph_change(plot_dt)


change_txt <- function(number) {
  if (number > 0) { 
    answer <- "INCREASING " 
  } else if (number < 0) {
    answer <- "decreasing "
  } else {
    answer <- "not changing "
  }
  answer
}

src_change <- function( data_dt, match ) {
  data_dt <- data_dt[ source==match ]
  fit <- lm(pollutants ~ year, data=data_dt)
  slope = round(fit$coef[2], digits=3)
  text = paste(c(match,"\n",change_txt(slope),abs(slope)," Kilotons/year" ),collapse = "")
}

plot_dt <- plot
# if it matches POINT replace with src_change text, etc
plot_dt$source <- sub("^POINT$", src_change(plot_dt,"POINT"), plot_dt$source)
plot_dt$source <- sub("^NONPOINT$", src_change(plot_dt,"NONPOINT"), plot_dt$source)
plot_dt$source <- sub("^ON-ROAD$", src_change(plot_dt,"ON-ROAD"), plot_dt$source)
plot_dt$source <- sub("^NON-ROAD$", src_change(plot_dt,"NON-ROAD"), plot_dt$source)

graph <- function(data_dt) {

  g <- ggplot( data_dt, aes(year, pollutants) )
  p <- g + geom_point()
  p <- p + geom_smooth(method ="lm", se=FALSE) 
  #p <- p + facet_wrap( ~ source, scales="free" ) 
  p <- p + facet_wrap( ~ source ) 
  p <- p + ylab("PM2.5 Emissions (Kilotons)") 
  p <- p + xlab("Year Pollutants Measured")  
  p <- p + ggtitle( "Baltimore Emissions of PM2.5 Pollutants by Source over Time\n") 
  print(p)
}
graph(plot_dt)

png(filename = "plot3.png", width = 480, height = 480)
graph(plot_dt)
dev.off()
