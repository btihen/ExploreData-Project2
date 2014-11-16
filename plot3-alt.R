library("ggplot2")
library("data.table")

NEI_df <- readRDS("summarySCC_PM25.rds")
SCC_df <- readRDS("Source_Classification_Code.rds")

NEI_dt = data.table(NEI_df)

plot_dt <- NEI_dt[ fips == "24510", sum(Emissions)/1000, by=list(year,type)]
# relabel calcuated names for improved labeling on graphs
setnames(plot_dt, "V1", "pollutants")
setnames(plot_dt, "type", "source")

# calculate if direction of change and turn into english
change_txt <- function(slope) {
  if (slope > 0) { 
    answer <- paste(c("SOURCE -- INCREASING\n(change rate =",slope,"Kilotons/year)"))
  } else if (slope < 0) {
    answer <- paste(c("SOURCE -- Decreasing\n(change rate =",slope,"Kilotons/year)"))
  } else {
    answer <- "SOURCE\nNOT Changing"
  }
  answer
}

# calculate linear model for a source & call change text to add descriptive text
src_change <- function( data_dt, match ) {
  data_dt <- data_dt[ source==match ]
  fit <- lm(pollutants ~ year, data=data_dt)
  slope = round(fit$coef[2], digits=3)
  text = paste(c(match,change_txt(slope)), collapse = " ")
}

# map values in dataframe to new calculations
mf_labeller <- function(value){ 
  value[value=="POINT"]    <- src_change(plot_dt,"POINT")
  value[value=="NONPOINT"] <- src_change(plot_dt,"NONPOINT")
  value[value=="ON-ROAD"]  <- src_change(plot_dt,"ON-ROAD")
  value[value=="NON-ROAD"] <- src_change(plot_dt,"NON-ROAD")
  return(value)
}

# add the newly calculated descriptive info to a column
plot_dt$src_info <- mf_labeller(plot_dt$source)

# create the graphs
graph <- function(data_dt) {
  g <- ggplot( data_dt, aes(year, pollutants, color=src_info) )
  p <- g + geom_point( aes(color = src_info), size=3.9, alpha=4/5)   
  p <- p + geom_smooth(method ="lm", se=FALSE, size=1.3) 
  p <- p + facet_wrap( ~ src_info, ncol=2 ) 
  #p <- p + theme(strip.text.x = element_text(size=8, color="blue"))
  p <- p + theme(legend.position="none")
  p <- p + ylab("PM2.5 Emissions (Kilotons)") 
  p <- p + xlab("Year Pollutants Measured")  
  p <- p + ggtitle( "Baltimore Emissions of PM2.5 Pollutants by Source over Time\n") 
  print(p)
}
graph(plot_dt)

png(filename = "plot3-alt.png", width = 480, height = 480)
graph(plot_dt)
dev.off()
