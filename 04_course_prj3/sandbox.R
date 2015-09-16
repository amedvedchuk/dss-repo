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




#---------- Question 1 ----------------
#1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission from all sources 
# for each of the years 1999, 2002, 2005, and 2008.

scc <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")
pm25data <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")

# divide emissions by 1000 to scale to kilotonns
totalEmissionByYear <- rowsum(pm25data$Emissions, pm25data$year)/1000
totalEmissionByYear <- data.frame(as.numeric(rownames(totalEmissionByYear)), totalEmissionByYear[,1])
names(totalEmissionByYear) <- c("year", "Emissions")

png("plot1.png")
with(totalEmissionByYear, {
    plot(x = year, Emissions, ylab = "Emissions (kilotons)", type = "b", 
         main = "Total emissions in United States by years")
    model <- lm(Emissions ~ year)
    abline(model, lwd = 2, col = "blue")
})

dev.off()


# with(pm25data, {
#   years <- unique(year)
#   plot(x = years, totalEmissionByYear, ylab = "Total emission (kilotons)", type = "b", 
#        main = "Total emission by years")
#   model <- lm(totalEmissionByYear ~ years)
#   abline(model, lwd = 2, col = "blue")
# })

#--------------------------------------


#---------- Question 2 ----------------
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? 
# Use the base plotting system to make a plot answering this question.

scc <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")
pm25data <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")

baltimore <- pm25data[pm25data$fips=="24510", ]

totalEmBaltimoreByYear <- rowsum(baltimore$Emissions, baltimore$year)
totalEmBaltimoreByYear <- data.frame(as.numeric(rownames(totalEmBaltimoreByYear)), totalEmBaltimoreByYear[,1])
names(totalEmBaltimoreByYear) <- c("year", "Emissions")

png("plot2.png")
with(totalEmBaltimoreByYear, {
  plot(x = year, y = Emissions, ylab = "Emissions (tons)", type = "b", 
       main = "Total emissions in Baltimore by years")
  model <- lm(Emissions ~ year)
  abline(model, lwd = 2, col = "blue")
})
dev.off()
#--------------------------------------


#---------- Question 3 ----------------
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
# Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make 
#a plot answer this question.


library(ggplot2)
scc <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")
pm25data <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")

baltimore <- pm25data[pm25data$fips=="24510", ]
aggregated <- aggregate(x=baltimore$Emissions, by = list(baltimore$year, baltimore$type), FUN = sum)
names(aggregated) <- c("year", "type", "Emissions")

png("plot3.png", height = 400, width = 800)
g <- ggplot(data = aggregated, aes(year, Emissions))
p <- g + geom_point() + geom_smooth(method = "lm") + 
    facet_grid(.~type) + 
    labs(title = "Emissions in Baltimore by year and type") + 
    ylab("Emissions (tons)")
p
dev.off()

# Alternative way to plot the same graph with qplot function:
# qplot(data=aggregated, x = year, y = Emissions, facets = .~type, 
#       geom =    c("point",	"smooth"), method = "lm", 
#       main = "Emissions in Baltimore by year and type", ylab = "Emissions (tons)")

#totalEmBaltimoreByYear <- rowsum(baltimore$Emissions, c(baltimore$year, baltimore$type))


#--------------------------------------

#---------- Question 4 ----------------
# Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

library(ggplot2)
scc <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")
pm25data <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")

## Another way to select the coalSCC is based on scc$EI.Sector column. Result plot is mostly same.
# coalCombSelector <- grep("coal", unique(scc$EI.Sector), ignore.case = T, value = T)
# coalSCC <- scc[scc$EI.Sector %in% coalCombSelector, ]
## But i decided to use scc$Short.Name as more ditailed descriptor

coalCombSelector <- grep("coal", unique(scc$Short.Name), ignore.case = T, value = T)
coalSCC <- scc[scc$Short.Name %in% coalCombSelector, ]

pm25fromCoal <- pm25data[pm25data$SCC %in% coalSCC$SCC,]
aggregated <- aggregate(x=pm25fromCoal$Emissions, by = list(pm25fromCoal$year), FUN = sum)
names(aggregated) <- c("year", "Emissions")

png("plot4.png")
g <- ggplot(data = aggregated, aes(year, Emissions))
p <- g + geom_point() + geom_smooth(method = "lm") +
        labs(title = "Emissions in US from coal combustion-related sources") + 
        ylab("Emissions (tons)")
p
dev.off()

#--------------------------------------

#---------- Question 5 ----------------
# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

library(ggplot2)
scc <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")
pm25data <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")

# motorVehicleSelector1 <- grep("vehicle", unique(scc$EI.Sector), ignore.case = T, value = T)
# motorVehicleSCC1 <- scc[scc$EI.Sector %in% motorVehicleSelector1, ]

motorVehicleSelector <- grep("motor|vehicle|veh", unique(scc$Short.Name), ignore.case = T, value = T)
motorVehicleSCC <- scc[scc$Short.Name %in% motorVehicleSelector, ]

baltimoreVehicles <- pm25data[pm25data$SCC %in% motorVehicleSCC$SCC & pm25data$fips=="24510",]

aggregated <- aggregate(x=baltimoreVehicles$Emissions, by = list(baltimoreVehicles$year), FUN = sum)
names(aggregated) <- c("year", "Emissions")

png("plot5.png")
g <- ggplot(data = aggregated, aes(year, Emissions))
p <- g + geom_point() + geom_smooth(method = "lm") + 
         labs(title = "Emissions in Baltimore from motor vehicle sources") + 
         ylab("Emissions (tons)")
p
dev.off()
#--------------------------------------


#---------- Question 6 ----------------
# Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources 
#in Los Angeles County, California (fips == "06037"). 
#Which city has seen greater changes over time in motor vehicle emissions?

library(ggplot2)

scc <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")
pm25data <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")

motorVehicleSelector <- grep("motor|vehicle|veh", unique(scc$Short.Name), ignore.case = T, value = T)
motorVehicleSCC <- scc[scc$Short.Name %in% motorVehicleSelector, ]

balAndlaVehicles <- pm25data[pm25data$SCC %in% motorVehicleSCC$SCC & pm25data$fips %in% c("06037", "24510"),]
laVehEmissionByYear <- aggregate(x=balAndlaVehicles$Emissions, by = list(balAndlaVehicles$fips, balAndlaVehicles$year), FUN = sum)
names(laVehEmissionByYear) <- c("Place", "year", "Emissions")
laVehEmissionByYear[laVehEmissionByYear$Place=="24510", 1] <- "Baltimore"
laVehEmissionByYear[laVehEmissionByYear$Place=="06037", 1] <- "LosAngelesCounty"

png("plot6.png", width = 800)
g <- ggplot(data = laVehEmissionByYear, aes(year, Emissions, col = Place))
p <- g + geom_point() + geom_smooth( method = "lm") +
         labs(title = "Emissions in Baltimore vs Los Angeles County from motor vehicle sources") + 
         ylab("Emissions (tons)")
p
dev.off()

#--------------------------------------

plot2 <- function(){
    
    library(dplyr)
    
    ## A) Parameters
    ## This script has no parameters
    
    ## B) Objetive
    ## To produce a line trend plot of total PM2.5 Emissions on Baltimore city and save it to a png file
    ## The data is supposed to be on the working directory, but if the file is missing a message is printed
    ## Also, to run properly, the following libraries should be installed and loaded
    ##      - library(dplyr)
    ## See Readme for more details
    
    ## C) Procedure
    
    ## c.1 Initial default values
    fileName <- "summarySCC_PM25.rds"
    
    ## c.2 Checks if the file exists in the specified directory
    if(file.exists(fileName)){                
        # c.2.1 Reads the data, filters it for Baltimore city, and summarizes it using the dplyr library
        NEI <- readRDS(fileName)
        byyear <- NEI %>% filter(fips=="24510") %>% group_by(year) %>% summarize(total=sum(Emissions)/1000)
        
        ## c.2.2 Creates the graph and saves it to a file
        png(filename = "plot2.png", width = 480, height = 480, units = "px")
        with(byyear, plot(byyear$year,byyear$total, type="b", col="red", xaxt="n", xlab="", ylab=""))
        with(byyear, axis(1, byyear$year))
        with(byyear, title(main="Baltimore City - Total PM2.5 Emissions", xlab="Year", ylab="Tousands of Tons"))
        with(byyear, text(byyear$year, byyear$total, labels=round(byyear$total,1), pos=c(4,2,3,2)))
        dev.off()
    } else {
        print("Download the file and put it into the working directory.")
        print(paste("Download link:", "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"))
    } 
}

plot3 <- function(){
    ## A) Parameters
    ## This script has no parameters
    
    ## B) Objetive
    ## To produce a line trend plot of total PM2.5 Emissions on Baltimore City, separated by source type, and save it to a png file
    ## The data is supposed to be on the working directory, but if the file is missing a message is printed
    ## Also, to run properly, the following libraries should be installed and loaded
    ##      - library(dplyr)
    ##      - library(ggplot2)
    ## See Readme for more details
    
    ## C) Procedure
    
    ## c.1 Initial default values
    fileName <- "summarySCC_PM25.rds"
    
    ## c.2 Checks if the file exists in the specified directory
    if(file.exists(fileName)){                
        # c.2.1 Reads the data, filters it for Baltimore city, and summarizes it using the dplyr library
        NEI <- readRDS(fileName)
        byyeartype <- NEI %>% filter(fips=="24510") %>% group_by(year, type) %>% summarize(total=sum(Emissions)/1000)
        
        ## c.2.2 Creates the graph and saves it to a file
        gplot <- ggplot(byyeartype, aes(x=as.factor(year), y=total))
        gplot + geom_point(alpha = 0.8, size =5) + facet_wrap(~ type) + aes(color = type) + labs(title="Baltimore City - PM2.5 Emissions by Source and Year\n") + labs(x="Year", y="Tousands of Tons\n")
        ggsave("plot3.png", dpi=100)
        
    } else {
        print("Download the file and put it into the working directory.")
        print(paste("Download link:", "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"))
    } 
}

plot4 <- function(){
    ## A) Parameters
    ## This script has no parameters
    
    ## B) Objetive
    ## To produce a line trend plot of total PM2.5 Emissions on USA, related to coal combustion, and save it to a png file
    ## The data is supposed to be on the working directory, but if the file is missing a message is printed
    ## Also, to run properly, the following libraries should be installed and loaded
    ##      - library(dplyr)
    ##      - library(ggplot2)
    ## See Readme for more details
    
    ## C) Procedure
    
    ## c.1 Initial default values
    fileName <- "summarySCC_PM25.rds"
    
    ## c.2 Checks if the file exists in the specified directory
    if(file.exists(fileName)){                
        # c.2.1 Reads the data from the two files and merges it
        NEI <- readRDS(fileName)
        SCC <- readRDS("Source_Classification_Code.rds")                
        coal <- merge(NEI[NEI$SCC %in% SCC[grep("^fuel comb -(.*)- coal$", SCC$EI.Sector, ignore.case=TRUE), 1], ], SCC[,c(1,4)], by.x = "SCC", by.y = "SCC")                
        
        # c.2.2 Calculates the total by all sectors and appends the total to the data by sector
        totalbyyear <- coal %>% group_by(year) %>% summarize(total=sum(Emissions)/1000)
        csec <- data.frame(EI.Sector=rep("Overall Coal Combustion", 4))
        totalbyyear <- cbind(csec, totalbyyear)
        bysectoryear <- coal %>% group_by(EI.Sector, year) %>% summarize(total=sum(Emissions)/1000)
        bysectoryear <- rbind(bysectoryear, totalbyyear)
        
        ## c.2.3 Creates the graph and saves it to a file
        gplot <- ggplot(bysectoryear, aes(x = as.factor(year), y = total))
        gplot + geom_point(aes(color = EI.Sector), size = 5, alpha = 0.8) + facet_grid(. ~ EI.Sector)+
            labs(title="USA - PM2.5 Emissions Related to Coal Combustion\n")+
            labs(x="Year", y="Tousands of Tons\n")+
            geom_text(size=3, hjust=c(0,0,0,1), vjust=-2, aes(label=round(total,1)))
        ggsave("plot4.png", dpi=100, width=12)
        
    } else {
        print("Download the file and put it into the working directory.")
        print(paste("Download link:", "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"))
    } 
}


# ================================================

## Questions and tasks: Have total emissions from PM2.5 decreased in the United States
## from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission
## from all sources for each of the years 1999, 2002, 2005, and 2008.

## Loading files
NEI <- readRDS("summarySCC_PM25.rds")

## Calculating total PM2.5 emission per year
Total.Emission<-tapply(NEI$Emissions,NEI$year,sum)

## Plotting
png(filename="plot1.png")
barplot(Total.Emission,xlab="Year"
        ,ylab="Amount of PM2.5 emitted (tons)"
        ,main="Total PM2.5 emission from all sources")
dev.off()



## Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable,
## which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City?
## Which have seen increases in emissions from 1999–2008?
## Use the ggplot2 plotting system to make a plot answer this question.

## Loading files
NEI <- readRDS("summarySCC_PM25.rds")

## Subsetting Baltimore
Subset<-NEI[NEI$fips=="24510",]

## Calculating total PM2.5 emission per type and year
library(plyr)
x<-ddply(Subset, c("year","type"), summarise, Total.Emission=sum(Emissions))

## Ordering the labels for the facets
x$type <- factor(x$type, levels=c("POINT", "NONPOINT", "ON-ROAD", "NON-ROAD"))

## Plotting
library(ggplot2)
png(filename="plot3.png", width = 600, height = 600) ## I make it bigger so the x-ticks labels fit well
qplot(year,Total.Emission,data=x,
      facets=.~type,geom = c("point", "line"),
      ylab="Amount of PM2.5 emitted (tons)",
      main="Total PM2.5 emission by type in the Baltimore City, Maryland")
dev.off()


#================================================================


# Load the NEI & SCC data frames. The rds files must be available in the working directory

# Load the NEI & SCC data frames.
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Subset NEI data by Baltimore's fip.
baltimoreNEI <- NEI[NEI$fips=="24510",]

# Aggregate using sum the Baltimore emissions data by year
aggTotalsBaltimore <- aggregate(Emissions ~ year, baltimoreNEI,sum)

png("plot2.png",width=480,height=480,units="px",bg="transparent")

barplot(
    aggTotalsBaltimore$Emissions,
    names.arg=aggTotalsBaltimore$year,
    xlab="Year",
    ylab="PM2.5 Emissions (Tons)",
    main="Total PM2.5 Emissions From all Baltimore City Sources"
)

dev.off()
