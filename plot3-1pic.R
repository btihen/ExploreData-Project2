library("ggplot2")
library("data.table")

NEI_df <- readRDS("summarySCC_PM25.rds")
SCC_df <- readRDS("Source_Classification_Code.rds")

NEI_dt = data.table(NEI_df)

plot      <- NEI_dt[ fips == "24510", sum(Emissions)/1000, by=list(year,type)]
setnames(plot, "V1", "pollutants")
setnames(plot, "type", "source")

change_txt <- function(number) {
  if (number > 0) { 
    answer <- "increasing" 
  } else if (number < 0) {
    answer <- "decreasing"
  } else {
    answer <- "not changing"
  }
  answer
}

text_label <- function( data_dt, match ) {
  data_dt <- data_dt[ source==match ]
  fit <- lm(pollutants ~ year, data=data_dt)
  slope = round(fit$coef[2], digits=3)
  text = paste(c(match, change_txt(slope), abs(slope), "Kilotons/year" ), collapse = " ")
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

graph <- function(data_dt) {

  g <- ggplot( data_dt, aes(year, pollutants, color=change) )
  p <- g + geom_point(size=3.5, alpha=4/5)
  #p <- g + geom_point( aes(color = change), size=6.5, alpha=4/5)   
  p <- p + geom_smooth(method ="lm", se=FALSE, size=1.2) 
  p <- p + scale_colour_discrete(name  ="Source")
  p <- p + ylab("PM2.5 Emissions (Kilotons)") 
  p <- p + xlab("Year Pollutants Measured")  
  p <- p + ggtitle( "Baltimore PM2.5 Pollutants by Source over Time\n") 
  #p <- p + geom_text(x=2005,y=2100, label=text_label(data_dt,"POINT"), size=4, family="serif", fontface="plain", color=factor("POINT") )
  #p <- p + opts(

  print(p)
}
graph(plot_dt)

png(filename = "plot3_1pic.png", width = 480, height = 480)
graph(plot_dt)
dev.off()
