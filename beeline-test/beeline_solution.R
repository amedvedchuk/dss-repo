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
    
    trainData <- read.csv("d:/projects-git/dss-repo/beeline-test/unpacked/train.csv")
    trainData$y <- as.factor((trainData$y))
    
} 

analyzeData <- function(){
    
    library(caret)
    tr <- trainData[1:100,]
    tr$y <- as.factor(tr$y)
    str(tr)
    fitControl <- trainControl(## 10-fold CV
      method = "none" )
      # number = 10,
      # repeats = 3
      
      
      #method = "repeatedcv",
      # number = 10,
      ## repeated ten times
      # repeats = 3
      # )
  
    fitControl
    
    modelFit1 <- train(y~., data=tr, method="rf")
    modelFit1
    predictions1 <- predict(modelFit1, newdata = tr)
    confusionMatrix(predictions1, tr$y)
    unique(predictions1)
    
    
    inTrain <- createDataPartition(y=trainData$y, p=.75, list = F)
    training <- trainData[inTrain, ]
    testing <- trainData[-inTrain, ]
    
    pairs(training[,23:62])
    
    modelFit2 <- train(y~., data=training, method="gbm", trControl = trainControl(method = "LOOCV"))
    modelFit2
    
    predictions <- predict(modelFit2, newdata = trainData[1:100,])
    length(predictions)
    
    confusionMatrix(predictions, tr[1:79,]$y)
    
} 


spamPredict <- function(){
  
  library(kernlab); 
  data(spam)
  inTrain <- createDataPartition(y=spam$type,
                                 p=0.75, list=FALSE)
  training <- spam[inTrain,]
  testing <- spam[-inTrain,]
  set.seed(32343)
  modelFit <- train(type ~.,data=training, method="glm")
  #install.packages('e1071', dependencies=TRUE)
  
}
