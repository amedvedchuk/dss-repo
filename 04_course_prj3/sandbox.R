# data saving parameters
dataUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
dataDir <- "data"
destArchive <- paste(dataDir, "/incoming.zip", sep="")
destUnzipFolder <- paste(dataDir, "/FNEI_data", sep="")

downloadRawData <- function(){
  
  if(!file.exists(dataDir)){
    print(paste("download data from:", dataUrl))
    dir.create(dataDir)
    download.file(dataUrl, destArchive, method = "wget")
    print(paste("unzip files to:", destUnzipFolder))
    unzip(zipfile = destArchive, exdir = destUnzipFolder)
  } else {
    print("data already downloaded!")
  }
  
}


scc <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")

pm25data <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")

#---------- Question 1 ----------------

totalEmissionByYear <- rowsum(pm25data$Emissions, pm25data$year)/1000

with(pm25data, {
  years <- unique(year)
  plot(x = years, totalEmissionByYear, ylab = "Total emission (kilotons)", type = "b", 
       main = "Total emission by years")
  model <- lm(totalEmissionByYear ~ years)
  abline(model, lwd = 2, col = "blue")
})

#--------------------------------------


#---------- Question 2 ----------------

baltimore <- pm25data[pm25data$fips=="24510", ]

totalEmBaltimoreByYear <- rowsum(baltimore$Emissions, baltimore$year)

with(pm25data, {
  years <- unique(year)
  plot(x = years, totalEmBaltimoreByYear, ylab = "Total emission (tons)", type = "b", 
       main = "Total emission in Baltimore by years")
  model <- lm(totalEmBaltimoreByYear ~ years)
  abline(model, lwd = 2, col = "blue")
})
#--------------------------------------


#---------- Question 3 ----------------

library(ggplot2)
baltimore <- pm25data[pm25data$fips=="24510", ]
aggregated <- aggregate(x=baltimore$Emissions, by = list(baltimore$year, baltimore$type), FUN = sum)
names(aggregated) <- c("year", "type", "Emissions")
#aggregated1 <- aggregate(type ~ year, data = baltimore, FUN = sum)



qplot(data=aggregated, x = year, y = Emissions, facets = .~type, 
      geom =	c("point",	"smooth"), method = "lm", 
      main = "Emissions in Baltimore by year and type")

g <- ggplot(data = aggregated, aes(year, Emissions))
p <- g + geom_point() + geom_smooth(method = "lm") + facet_grid(.~type)
p


#totalEmBaltimoreByYear <- rowsum(baltimore$Emissions, c(baltimore$year, baltimore$type))


#--------------------------------------