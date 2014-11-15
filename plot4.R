library("data.table")

NEI_df  <- readRDS("summarySCC_PM25.rds")
SCC_df  <- readRDS("Source_Classification_Code.rds")
mergedf <- merge( NEI_df, SCC_df, by.x="SCC", by.y="SCC", all=FALSE )
mergedf <- mergedf[ order( fips ), ]
mergedt = data.table(mergedf)

# plot_dt <- mergedt[ Short.Name %like% "Coal" , sum(Emissions)/1000, by=year ]
# setnames(plot_dt, "V1", "pollutants")
plot_dt
#    year       V1
# 1: 2008 355.9677
# 2: 2005 566.7009
# 3: 2002 561.1747
# 4: 1999 596.6879
# 
# plot_dt <- mergedt[ Short.Name %like% "Coal" | EI.Sector %like% "Coal" , sum(Emissions)/1000, by=year ]
# setnames(plot_dt, "V1", "pollutants")
# > plot_dt
#    year pollutants
# 1: 2008   356.1031
# 2: 2005   568.8139
# 3: 2002   563.3646
# 4: 1999   598.7546
# 
# plot_dt <- mergedt[ Short.Name %like% "Coal" | EI.Sector %like% "Coal" | SCC.Level.One %like% "Coal" | SCC.Level.Two %like% "Coal" | SCC.Level.Three %like% "Coal" | SCC.Level.Four %like% "Coal", sum(Emissions)/1000, by=year ]
# setnames(plot_dt, "V1", "pollutants")
# > plot_dt
#    year pollutants
# 1: 2008   356.1031
# 2: 2005   568.8139
# 3: 2002   563.3646
# 4: 1999   598.7546

plot_dt <- mergedt[ Short.Name %like% "Coal" | EI.Sector %like% "Coal" , sum(Emissions)/1000, by=year ]
setnames(plot_dt, "V1", "pollutants")

graph <- function(data_dt) {

  fit <- lm(pollutants ~ year, data=data_dt)
  slope = round(fit$coef[2], digits=3)

  plot(data_dt, main="US Coal Combustion Emissions of PM2.5 Pollutants over Time",
    ylab="PM2.5 Emissions (Kilotons)",
    xlim=c(1998,2009), ylim=c(0.0,1000.0),
    pch=20)
  lines(data_dt$year, data_dt$pollutants)
  abline(fit, col="blue")
  
  lines(data_dt$year, data_dt$pollutants)
  abline(fit, col="blue")
    legend("topright", 
    legend = paste(c("Change = ", slope, "Kilotons/year" ), collapse = " "),
    lty=1, # gives the legend appropriate symbols (lines)
    col="blue")
}

graph(plot_dt)

png(filename = "plot4.png", width = 480, height = 480)
graph(plot_dt)
dev.off()


