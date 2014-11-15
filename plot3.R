library("ggplot2")
library("data.table")

NEI_df <- readRDS("summarySCC_PM25.rds")
SCC_df <- readRDS("Source_Classification_Code.rds")

NEI_dt = data.table(NEI_df)

plot_dt <- NEI_dt[ fips == 24510, sum(Emissions)/1000, by=list(year,type)]
setnames(plot_dt, "V1", "pollutants")
setnames(plot_dt, "type", "source")

graph <- function(data_dt) {

  g <- ggplot( data_dt, aes(year, pollutants) )
  p <- g + geom_point()
  p <- p + geom_smooth(method ="lm", se=FALSE) 
  #p <- p + facet_wrap( ~ source, scales="free" ) 
  p <- p + facet_wrap( ~ source ) 
  p <- p + ylab("PM2.5 Emissions (Kilotons)") 
  p <- p + xlab("Years Pollutants Measured")  
  p <- p + ggtitle( "Baltimore Emissions of PM2.5 Pollutants by Source over Time\n") 

  print(p)
}

graph(plot_dt)

png(filename = "plot3.png", width = 480, height = 480)
graph(plot_dt)
dev.off()
