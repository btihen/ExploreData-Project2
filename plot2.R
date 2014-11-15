library("data.table")

NEI_df <- readRDS("summarySCC_PM25.rds")
SCC_df <- readRDS("Source_Classification_Code.rds")

NEI_dt = data.table(NEI_df)

plot_dt <- NEI_dt[ fips == "24510", sum(Emissions)/1000, by=year]
setnames(plot_dt, "V1", "pollutants")

graph <- function(data_dt) {
  fit <- lm(pollutants ~ year, data=data_dt)
  slope = round(fit$coef[2], digits=3)

  plot(data_dt, main="Baltimore Emissions of PM2.5 Pollutants over Time", 
    ylab="PM2.5 Emissions (Kilotons)",
    xlab="Year Pollutants Measured",
    ylim=c(0.0,5.0),xlim=c(1998,2010), 
    pch=20)
  lines(data_dt$year, data_dt$pollutants)
  abline(fit, col="blue")
  legend("topright", 
    legend = paste(c("Change =", slope, "Kilotons/year" ), collapse = " "),
    lty=1, col="blue")
}
par(mfrow=c(1,1))
graph(plot_dt)

png(filename = "plot2.png", width = 480, height = 480)
par(mfrow=c(1,1))
graph(plot_dt)
dev.off()

