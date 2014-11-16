library("data.table")

NEI_df  <- readRDS("summarySCC_PM25.rds")
SCC_df  <- readRDS("Source_Classification_Code.rds")
mergedf <- merge( NEI_df, SCC_df, by.x="SCC", by.y="SCC", all=FALSE )
mergedf <- mergedf[ order( mergedf$fips ), ]
mergedt = data.table(mergedf)

balt_cars <- mergedt[ fips=="24510" & Short.Name %like% "Highway", sum(Emissions)/1000, by=year ]
setnames(balt_cars, "V1", "pollutants")
balt_cars$area <- "Baltimore City"
balt_cars <- balt_cars[ order( balt_cars$year ), ]

la_cars <- mergedt[ fips=="06037" & Short.Name %like% "Highway", sum(Emissions)/1000, by=year ]
setnames(la_cars, "V1", "pollutants")
la_cars$area <- "Los Angles County"
la_cars <- la_cars[ order( la_cars$year ), ]

graph <- function(data_dt) {

  fit         <- lm(pollutants ~ year, data=data_dt)
  slope       <- round(fit$coef[2], digits=3)
  legend_text <- paste( c("Change =", slope, "Kilotons/year" ),  collapse = " ")
  title_text  <- paste( c(data_dt$area[1], "Vehicle Emissions"), collapse = "\n")

  plot(data_dt$pollutants ~ data_dt$year, 
    main=title_text,
    xlab="Year Pollutants Measured", 
    ylab="PM2.5 Emissions (Kilotons)",
    xlim=c(1998,2010), ylim=c(0.0,6.0),
    pch=20)
  lines(data_dt$year, data_dt$pollutants)
  abline(fit, col="blue")
  
  lines(data_dt$year, data_dt$pollutants)
  abline(fit, col="blue")
    legend("topright", 
    legend = legend_text,
    cex=0.8, pch=1, pt.cex = 0.5,
    lty=1, # gives the legend appropriate symbols (lines)
    col="blue")
}

par(mfrow=c(1,2))
par(mar=c(4.1,4.1,4.1,1.1))
graph(balt_cars)
graph(la_cars)

png(filename = "plot6.png", width = 480, height = 480)
par(mfrow=c(1,2))
par(mar=c(4.1,4.1,4.1,1.1))
graph(balt_cars)
graph(la_cars)
dev.off()

