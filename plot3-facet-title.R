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

# create text for new source information
src_change <- function( data_dt, match ) {
  data_dt <- data_dt[ source==match ]
  fit <- lm(pollutants ~ year, data=data_dt)
  slope = round(fit$coef[2], digits=3)
  text = paste(c(match," SOURCE\n",change_txt(slope),"-- change = ",slope," Kilotons/year"), collapse = "")
}

# recplace old source information with source and change info
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
  p <- p + facet_wrap( ~ source, ncol=2 ) 
  p <- p + ylab("PM2.5 Emissions (Kilotons)") 
  p <- p + xlab("Year Pollutants Measured")  
  p <- p + ggtitle( "Baltimore Emissions of PM2.5 Pollutants by Source over Time\n") 
  print(p)
}


plot_dt <- NEI_dt[ fips == "24510", sum(Emissions)/1000, by=list(year,type)]
# relabel calcuated names for improved labeling on graphs
setnames(plot_dt, "V1", "pollutants")
setnames(plot_dt, "type", "source")

src_change <- function( data_dt, match ) {
  data_dt <- data_dt[ source==match ]
  fit <- lm(pollutants ~ year, data=data_dt)
  slope = round(fit$coef[2], digits=3)
  text = paste(c(match," SOURCE\n",change_txt(slope),"-- change = ",slope," Kilotons/year"), collapse = "")
}


# idea from http://www.cookbook-r.com/Graphs/Facets_%28ggplot2%29/
facet_label <- function(value) {
  value <- as.character(value)
  if (var=="source") { 
    #value_ret <- sub(value, src_change(plot_dt,value), plot_dt$source)
    value[value=="POINT"]   <- sub("^POINT$", src_change(plot_dt,"POINT"), plot_dt$source)
    value[value=="NONPOINT"]<- sub("^NONPOINT$", src_change(plot_dt,"NONPOINT"), plot_dt$source)
    value[value=="ON-ROAD"] <- sub("^ON-ROAD$", src_change(plot_dt,"ON-ROAD"), plot_dt$source)
    value[value=="NON-ROAD"]<- sub("^NON-ROAD$", src_change(plot_dt,"NON-ROAD"), plot_dt$source)
  }
  return(value)
}

# http://stackoverflow.com/questions/3472980/ggplot-how-to-change-facet-labels
src_names <- list(
    "POINT"    = src_change(plot_dt,"POINT"),
    "NONPOINT" = src_change(plot_dt,"NONPOINT"),
    "ON-ROAD"  = src_change(plot_dt,"ON-ROAD"),
    "NON-ROAD" = src_change(plot_dt,"NON-ROAD")
)

facet_label <- function(variable,value){
  return(source_names[value])
}


# idea from http://www.cookbook-r.com/Graphs/Facets_%28ggplot2%29/
facet_label <- function(value) {
  value <- as.character(value)
  if (var=="source") { 
    #value_ret <- sub(value, src_change(plot_dt,value), plot_dt$source)
    value[value=="POINT"]   <- sub("^POINT$", src_change(plot_dt,"POINT"), plot_dt$source)
    value[value=="NONPOINT"]<- sub("^NONPOINT$", src_change(plot_dt,"NONPOINT"), plot_dt$source)
    value[value=="ON-ROAD"] <- sub("^ON-ROAD$", src_change(plot_dt,"ON-ROAD"), plot_dt$source)
    value[value=="NON-ROAD"]<- sub("^NON-ROAD$", src_change(plot_dt,"NON-ROAD"), plot_dt$source)
  }
  return(value)
}


graph <- function(data_dt) {
  g <- ggplot( data_dt, aes(year, pollutants) )
  p <- g + geom_point()
  p <- p + geom_smooth(method ="lm", se=FALSE) 
  #p <- p + facet_wrap( ~ source, ncol=2, labeller=facet_label ) 
  p <- p + facet_grid( ~ source, labeller=facet_label ) 
  p <- p + ylab("PM2.5 Emissions (Kilotons)") 
  p <- p + xlab("Year Pollutants Measured")  
  p <- p + ggtitle( "Baltimore Emissions of PM2.5 Pollutants by Source over Time\n") 
  print(p)
}
graph(plot_dt)

png(filename = "plot3-facet-title.png", width = 480, height = 480)
graph(plot_dt)
dev.off()

graph(plot_dt)

png(filename = "plot3-facet-title.png", width = 480, height = 480)
graph(plot_dt)
dev.off()

