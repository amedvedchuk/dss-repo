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

totalEmissionByYear <- rowsum(pm25data$Emissions, pm25data$year)

with(pm25data, {
years <- unique(year)
plot(x = years, totalEmissionByYear)
model <- lm(totalEmissionByYear ~ years, airquality)
abline(model, lwd = 2)
})
