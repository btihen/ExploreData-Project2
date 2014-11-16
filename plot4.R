library("data.table")

NEI_df  <- readRDS("summarySCC_PM25.rds")
SCC_df  <- readRDS("Source_Classification_Code.rds")

mergedf <- merge( NEI_df, SCC_df, by.x="SCC", by.y="SCC", all=FALSE )
mergedf <- mergedf[ order( mergedf$fips ), ]
mergedt = data.table(mergedf)

plot_dt <- mergedt[ Short.Name %like% "Coal" | EI.Sector %like% "Coal" , sum(Emissions)/1000, by=year ]
setnames(plot_dt, "V1", "pollutants")

graph <- function(data_dt) {

  fit         <- lm(pollutants ~ year, data=data_dt)
  slope       <- round(fit$coef[2], digits=3)
  legend_text <-  paste(c("Change =", slope, "Kilotons/year" ), collapse = " ")

  plot(data_dt, main="US Coal Combustion Emissions of PM2.5 Pollutants over Time",
    xlab="Year Pollutants Measured",
    ylab="PM2.5 Emissions (Kilotons)",
    xlim=c(1998,2010), ylim=c(0.0,1000.0),
    pch=20)
  lines(data_dt$year, data_dt$pollutants)
  abline(fit, col="blue")
  
  lines(data_dt$year, data_dt$pollutants)
  abline(fit, col="blue")
    legend("topright", 
    legend = legend_text,
    lty=1, # gives the legend appropriate symbols (lines)
    col="blue")
}
par(mfrow=c(1,1))
graph(plot_dt)

png(filename = "plot4.png", width = 480, height = 480)
par(mfrow=c(1,1))
graph(plot_dt)
dev.off()


