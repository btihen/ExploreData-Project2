library("data.table")

NEI_df  <- readRDS("summarySCC_PM25.rds")
SCC_df  <- readRDS("Source_Classification_Code.rds")
mergedf <- merge( NEI_df, SCC_df, by.x="SCC", by.y="SCC", all=FALSE )
mergedf <- mergedf[ order( mergedf$fips ), ]
mergedt = data.table(mergedf)

plot_dt <- mergedt[ fips==24510 & Short.Name %like% "Highway", sum(Emissions), by=year ]
setnames(plot_dt, "V1", "pollutants")

graph <- function(data_dt) {

  fit <- lm(pollutants ~ year, data=data_dt)
  slope = round(fit$coef[2], digits=3)

  plot(data_dt, main="Baltimore Vehicle Emissions of PM2.5 Pollutants over Time",
    ylab="PM2.5 Emissions (Tons)",
    xlim=c(1998,2009), ylim=c(0.0,1000.0),
    pch=20)
  lines(data_dt$year, data_dt$pollutants)
  abline(fit, col="blue")
  
  lines(data_dt$year, data_dt$pollutants)
  abline(fit, col="blue")
    legend("topright", 
    legend = paste(c("Change = ", slope, "Tons/year" ), collapse = " "),
    lty=1, # gives the legend appropriate symbols (lines)
    col="blue")
}

graph(plot_dt)

png(filename = "plot5.png", width = 480, height = 480)
graph(plot_dt)
dev.off()

