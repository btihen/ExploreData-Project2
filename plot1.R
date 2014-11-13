library("data.table")

NEI_df <- readRDS("summarySCC_PM25.rds")
SCC_df <- readRDS("Source_Classification_Code.rds")

NEI_dt = data.table(NEI_df)

plot_dt <- NEI_dt[,sum(Emissions)/1000000, by=year]
setnames(plot_dt, "V1", "pollutants")

graph <- function(data) {
  plot(data, main="Total US Emissions of PM2.5 Pollutants over Time", ylab="PM2.5 Emissions (Megatons)",ylim=c(3.0,8.0),xlim=c(1998,2009), pch=20)
  lines(data$year, data$pollutants)
}

png(filename = "plot1.png", width = 480, height = 480)
graph(plot_dt)
dev.off()

