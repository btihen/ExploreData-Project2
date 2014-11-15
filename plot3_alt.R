library("ggplot2")
library("data.table")

NEI_df <- readRDS("summarySCC_PM25.rds")
SCC_df <- readRDS("Source_Classification_Code.rds")

NEI_dt = data.table(NEI_df)

plot      <- NEI_dt[ fips == "24510", sum(Emissions)/1000, by=list(year,type)])
setnames(plot, "V1", "pollutants")
setnames(plot, "type", "source")

text_label <- function( data_dt, match ) {
  data_dt <- data_dt[ source==match ]
  fit <- lm(pollutants ~ year, data=data_dt)
  slope = round(fit$coef[2], digits=3)
  text = paste(c("Change = ", slope, "Tons/year" ), collapse = " ")
}

graph <- function(data_dt) {

  g <- ggplot( data_dt, aes(year, pollutants, color=source) )
  p <- g + geom_point(size=2.5)
  #p <- g + geom_point( aes(color = source), size=6.5, alpha=4/5)   
  p <- p + geom_smooth(method ="lm", se=FALSE, size=1.2) 
  p <- p + scale_colour_discrete(name  ="Source")
  p <- p + ylab("PM2.5 Emissions (Kilotons)") 
  p <- p + xlab("Year Pollutants Measured")  
  p <- p + ggtitle( "Baltimore Emissions of PM2.5 Pollutants by Source over Time\n") 
  #p <- p + geom_text(x=2005,y=2100, label=text_label(data_dt,"POINT"), size=4, family="serif", fontface="plain", color=factor("POINT") )
  #p <- p + geom_text(x=2005,y=2000, label=text_label(data_dt,"NONPOINT"), size=4, family="serif", fontface="plain", color="green" )

  print(p)
}
graph(plot)

png(filename = "plot3_alt.png", width = 480, height = 480)
graph(plot)
dev.off()
