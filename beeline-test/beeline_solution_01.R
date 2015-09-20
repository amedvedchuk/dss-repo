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
    
    print("read data...")
    d <- read.csv("unpacked/train.csv")
    print("read done!")
    d
} 

exploreData <- function(){
    
    unique(beeData$x7)
    
    largeFactors <- c(11, 19, 22)
    str(beeData[largeFactors])
    beeNoLFactors <- beeData[-largeFactors]
    
    
    numsOnly <- sapply(beeData, is.numeric)
    beeNumsOnly <- beeData[,numsOnly]
    
    
    
}

# analyzeData(beeNoLFactors)

# Naive Bayes    nb - 0.40
# Linear Discriminant Analysis lda
# CART    rpart - 0.66
#         rpart2 (maxdepth 4, data w/o large factors) - 0.703
# Boosted Classification Trees    ada
# Support Vector Machines with Linear Kernel    svmLinear
# Least Squares Support Vector Machine    lssvmLinear
# Random Forest    rf

analyzeData <- function(){
    
    library(caret)
    
    set.seed(21121)
         data <- beeNoLFactors
#     data <- beeNumsOnly
    
    
    data$y <- as.factor(data$y)
#     table(data[,c(1:2,44)])

    # for nb:
#      data <- data[,-c(1,2)]

    inTrain = createDataPartition(y=data$y, p = 0.7, list=F)
    training = data[ inTrain,]
    testing = data[-inTrain,]
    dim(training)

    rm(modelFitAsIs)

    modelFitAsIs <- train(y ~ ., method = "rpart2", data = training, 
                          trControl = trainControl(method = "cv", verboseIter = T, number = 5)
#                           ,preProcess = c("center", "scale")
#                           ,preProcess = "pca"
#                           ,tuneGrid = data.frame(fL = 1, usekernel = T)
                          ,tuneGrid = data.frame(maxdepth = 3:7)
    )

    modelFitAsIs
    plot(modelFitAsIs)
    varImp(modelFitAsIs)
  
    predictions <- predict(modelFitAsIs, newdata = na.omit(testing))
    confusionMatrix(predictions, na.omit(testing)$y)


    ?predict

    table(training$y)
    table(testing$y)    

    preInt <- as.integer(predictions)
    preInt[preInt<0] <- 0
    summary(preInt)    
    hist(preInt)

    summary(predictions)
    hist(predictions)
    hist(testing$y)
    hist(training$y)

    summary(testing$y)    

    confusionMatrix(preInt, na.omit(testing)$y)

#     modelFitAsIs$finalModel
    
    11907/14998

} 

getRawData()
beeData <- readData()
