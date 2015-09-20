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
# Linear Discriminant Analysis lda - 0.51
# CART    rpart - 0.66
#         rpart2 (maxdepth 6, data w/o large factors) - 0.7197
# Boosted Classification Trees    ada   ---- Currently this procedure can not directly handle > 2 class response
# AdaBag - .754
# Support Vector Machines with Linear Kernel    svmLinear
# Least Squares Support Vector Machine    lssvmLinear
# Random Forest  (mtry = 16, p=0.7, beeAlnum)   rf - 0.7553


analyzeData <- function(){
    
    library(caret)
    
    set.seed(21121)
#          data <- beeNoLFactors
    data <- beeNumsOnly
    
    
    data$y <- as.factor(data$y)
#     table(data[,c(1:2,44)])

    # for nb:
#      data <- data[,-c(1,2)]

    inTrain = createDataPartition(y=data$y, p = 0.7, list=F)
    training = data[ inTrain,]
    testing = data[-inTrain,]
    dim(training)


    rm(modelFitAsIs)

# for lda   (just for beeNumOnly)
#     training <- training[,-2]

    modelFitAsIs <- train(y ~ ., method = "AdaBoost.M1", data = training, 
                          trControl = trainControl(method = "cv", verboseIter = T, number = 3)
#                           ,preProcess = c("center", "scale")
#                           ,preProcess = "pca"
#                           ,tuneGrid = data.frame(fL = 1, usekernel = T)     # for nb
#                           ,tuneGrid = data.frame(maxdepth = 3:7)            # for rpart2
#                           ,tuneGrid = data.frame(mtry=16)     # for rf
#                           ,tuneGrid = data.frame(maxdepth=7, mfinal = c(50,100,150))     # for adaBag
    )

    

    modelFitAsIs
    plot(modelFitAsIs)
    varImp(modelFitAsIs)
  
    predictions <- predict(modelFitAsIs, newdata = na.omit(testing))
    confusionMatrix(predictions, na.omit(testing)$y)
    length(predictions)
    dim(testing)


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

test <- function(){
    final_test <- read.csv("unpacked/test.csv")
    
    final_test <- final_test[!(final_test$x14 %in% c("94f7a0566f", "c82fb3b2f7")),]
    final_test <- final_test[!(final_test$x17 %in% c("ab6738e02f")),]
    final_test <- final_test[!(final_test$x20 %in% c("d000d40d38")),]
    
    
    final_pred <- predict(modelFitAsIs, newdata = na.omit(final_test))
    
    result_df <- data.frame(na.omit(final_test)$ID, final_pred)
    colnames (result_df)<- c("ID","y")
    head(result_df, 10)
    class(result_df$y)
    
    write.table(result_df, file = "result.csv", quote = F, col.names = c("ID", "y"), row.names = F)
}

getRawData()
beeData <- readData()
