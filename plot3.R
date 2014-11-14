library("ggplot2")
library("data.table")

NEI_df <- readRDS("summarySCC_PM25.rds")
SCC_df <- readRDS("Source_Classification_Code.rds")

NEI_dt = data.table(NEI_df)

plot_dt <- NEI_dt[ fips == 24510, sum(Emissions)/1000, by=list(year,type)]
setnames(plot_dt, "V1", "pollutants")
setnames(plot_dt, "type", "source")

graph <- function(data_dt) {

  # qplot(year, pollutants, data=data_dt, facets = . ~type,  # color=type
  #   geom=c("point", "smooth"), method = "lm")
    
  g <- ggplot( data_dt, aes(year, pollutants) )
  # multiple colors are not needed with multiple graphs
  # p <- g + geom_point( aes(color = source), size=2.5, alpha=3/4) 
  # p <- p + scale_colour_discrete(name  ="Source Type")
  p <- g + geom_point()
  p <- p + geom_smooth(method ="lm") 
  # use a wrap grid for readability 
  # p <- p + facet_grid( . ~source ) 
  p <- p + facet_wrap( ~ source, scales="free" ) 
  p <- p + ylab("PM2.5 Polluntants (Kilotons)") 
  p <- p + xlab("Sample Years")  
  p <- p + ggtitle( "Baltimore Emissions of PM2.5 Pollutants by Source Type\n") 

  print(p)
}

graph(plot_dt)

png(filename = "plot3.png", width = 480, height = 480)
graph(plot_dt)
dev.off()
