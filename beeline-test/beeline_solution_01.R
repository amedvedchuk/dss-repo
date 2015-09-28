

library(gtools)    
library(caret)


getRawData <- function(){
    
    # data saving parameters
    # dataUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
    dataDir <- ""
    destArchive <- paste(dataDir, "beeline_bigdata.zip", sep="")
    destUnzipFolder <- paste(dataDir, "unpacked", sep="")
    
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
    
    summary(beeData)
    beeNA <- beeData[!complete.cases(beeData),]
    
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
# amdai - 0.5012
trainModel <- function(){
    
    
    set.seed(21121)
    
    imputeMethod <- "medianImpute"
    #     imputeMethod <- "knnImpute"
    
    #     beeData[!complete.cases(beeData),]
    
    # data <- beeNoLFactors
    # data <- beeNumsOnly
    data<- beeData
    data <- preprocessColumns(beeData, delCol = c())  #"x7"
    data <- impute_NA(data, excludeCol = "y")
    data$y <- as.factor(data$y)
    
    #         
    #     dataNA <- data[!complete.cases(data),]
    #     dataOnly <- data[,-length(data)]
    #     preObj <- preProcess(dataOnly, method = imputeMethod)
    #     data <- predict(preObj, data)
    
    
    library(doParallel)
    cl <- makeCluster(2, type='PSOCK')
    registerDoParallel(cl)
    
    registerDoSEQ()
    
    
    
    #     table(data[,c(1:2,44)])
    
    # for nb:
    #      data <- data[,-c(1,2)]
    
    inTrain = createDataPartition(y=data$y, p = 0.1, list=F)
    training = data[ inTrain,]
    testing = data[-inTrain,]
    dim(training)
    dim(testing)
    
    
#     library(snow)
#     cl<-makeCluster(4,type="SOCK")
#     stopCluster(cl)    
    
    
    

    rm(modelFitAsIs)
    
    # for lda   (just for beeNumOnly)
    #     training <- training[,-2]
    

    modelFitAsIs <<- train(y ~ ., method = "ORFsvm", data = training, 
                          trControl = trainControl(method = "cv", verboseIter = T
                                                   , number = 5
                                                   )
                          # ,preProcess = c("center", "scale")
                          # ,preProcess = imputeMethod
                          # ,preProcess = "pca"
                          # ,tuneGrid = data.frame(fL = 1, usekernel = T)     # for nb
                          # ,tuneGrid = data.frame(maxdepth = 20)            # for rpart2
#                           ,tuneGrid = data.frame(mtry=18)     # for rf
                           # ,tuneGrid = data.frame(mtry=18)     # for rf
 # ,prox = T
#                           ,tuneGrid = data.frame(maxdepth=9, mfinal = 150)     # for adaBag
    )
    
    
    modelFitAsIs
    modelFitAsIs$finalModel
    plot(modelFitAsIs)
    vi <- varImp(modelFitAsIs)

# idea 0.
#     training <- data.frame(training[, order(vi$importance$Overall, decreasing = T)[1:20]], y=training$y)
    
        

    predictions <- predict(modelFitAsIs, newdata = na.omit(testing))
    confusionMatrix(predictions, na.omit(testing)$y)
    length(predictions)
    dim(testing)
    
    
#     table(training$y)
#     table(testing$y)    
#     
#     preInt <- as.integer(predictions)
#     preInt[preInt<0] <- 0
#     summary(preInt)    
#     hist(preInt)
#     
#     summary(predictions)
#     hist(predictions)
#     hist(testing$y)
#     hist(training$y)
#     
#     summary(testing$y)    
    
} 

test <- function(){
    
    print("start testing !!!")
    
    final_test <- read.csv("unpacked/test.csv")
    
    final_test <- preprocessColumns(final_test, delCol = c()) #"x7"
    final_test <- impute_NA(final_test, excludeCol = "ID")
    
    #     final_test <- final_test[!(final_test$x14 %in% c("94f7a0566f", "c82fb3b2f7")),]
    #     final_test <- final_test[!(final_test$x17 %in% c("ab6738e02f")),]
    #     final_test <- final_test[!(final_test$x20 %in% c("d000d40d38")),]
    
    print(sprintf("prediction on model [%s] with Estimated Accuracy = %f ..."
                  , modelFitAsIs$modelInfo$label
                  , modelFitAsIs$results$Accuracy))
    
    final_pred <- predict(modelFitAsIs, newdata = final_test)
    print(head(final_pred))
    print(length(final_pred))
    
    result_df <- data.frame(ID=na.omit(final_test)$ID, y=final_pred)
    head(result_df, 10)
    
    #write predictions
    file_prefix <- paste("result",format(Sys.time(), "%y%m%d_%H%M"),"_EA", round(modelFitAsIs$results$Accuracy, 5), sep="")
    print(paste("write result with file prefix:",file_prefix))
    
    write.table(result_df, 
                file = paste(file_prefix, ".csv", sep=""), 
                quote = F, row.names = F, sep = ",")
    #write desciption
    desc <- capture.output(cat(sep="\n", 
                "dim(training): ", capture.output(dim(training)), 
                "dim(testing): ", capture.output(dim(testing)),
                "dim(final_test): ", capture.output(dim(final_test)),
                "imputeMethod: ",  imputeMethod,
                "FIT call: ", capture.output(modelFitAsIs$call),
                "TRAINING vars: ", capture.output(names(training)),
                "\n\nMODEL SETTINGS: ", capture.output(modelFitAsIs),
                "\n\n predicted df head: ", capture.output( head(result_df, 10))
                ))
    write(desc, file=paste(file_prefix, ".desc", sep=""))
}

impute_NA <- function(dtaset, excludeCol = c()){
    
    print(sprintf("impute_NA: start preprocess with predict(method = %s)", imputeMethod))
    
    if (length(excludeCol) > 0){
        preprocData <- dtaset[,-which(names(dtaset) %in% excludeCol)]
        restData <- data.frame(dtaset[,which(names(dtaset) %in% excludeCol)])
        if(length(excludeCol) == 1){
            names(restData) <- excludeCol
        }
        result <- predict(preProcess(preprocData, method = imputeMethod), preprocData)
        result <- cbind(result, restData)
        print(sprintf("exclude cols: %s", capture.output(excludeCol)))
    } else{
        result <- predict(preProcess(dtaset, method = imputeMethod), dtaset)
    }
    
    NAs <- result[!complete.cases(result),]
    print("Nas dataset dim:")
    print(dim(NAs))
    print("impute_NA finish!")
    result
    
}

preprocessColumns <- function(dataset, numonly=TRUE, delCol=c()){
    
    print("preprocessColumns start")
    result <- dataset
    
    if(numonly){
        numsOnlyFinal <- sapply(result, is.numeric)
        result <- result[,numsOnlyFinal]
    }
    if(length(delCol) > 0){
        result <- result[,-which(names(result) %in% delCol)]
    }
    print(sprintf("preprocessColumns finish! - result dim: %s", capture.output(dim(result))))
    result
}

# getRawData()
# beeData <- readData()
# 22.09.2015 - 76.36
# 23.09.2015 - 76.38