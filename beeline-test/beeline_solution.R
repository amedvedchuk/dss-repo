# data saving parameters
# dataUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
dataDir <- ""
destArchive <- paste(dataDir, "beeline_bigdata.zip", sep="")
destUnzipFolder <- paste(dataDir, "unpacked", sep="")

getRawData <- function(){
    
#     if(!file.exists(dataDir)){
#         print(paste("download data from:", dataUrl))
#         dir.create(dataDir)
#         download.file(dataUrl, destArchive, method = "wget")
        print(paste("unzip files to:", destUnzipFolder))
        unzip(zipfile = destArchive, exdir = destUnzipFolder)
#     } else {
#         print("data already downloaded!")
#     }
    
}


readData <- function(){
    
    train <- read.csv("unpacked/train.csv")
    
} 

analyzeData <- function(){
    
    library(caret)
    modelFit <- train(y~., data=train, method="glm")
} 
