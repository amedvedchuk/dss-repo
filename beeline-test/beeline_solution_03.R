

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

init <- function(){
  beeData <<- readData()
  
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

runParallel <- function(){
  library(doParallel)
  cl <- makeCluster(4, type='PSOCK')
  registerDoParallel(cl)
  
  
}

delLargeFactors <- function(){
  largeFactors <- c(11, 19, 22)
  str(beeData[largeFactors])
  beeNoLFactors <- beeData[-largeFactors]
  beeNoLFactors
}

runSeq <- function(){
  registerDoSEQ()
}

splitData <- function(beeData, 
                      ptrain=0.7, 
                      ptest=0.7, 
                      delCol=c(), 
                      trainImpMethod="bagImpute", 
                      testImpMethod=trainImpMethod,
                      validateImpMethod=testImpMethod
){
  
  res <- MModel$new()
  
  data <- preprocessColumns(beeData, delCol = delCol)  #"x7"
  data$y <- as.factor(data$y)
  
  inTrain <- createDataPartition(y=data$y, p = ptrain, list=F)
  training <- data[inTrain,]
  
  inTest <- createDataPartition(y=data[-inTrain,]$y, p = ptest, list=F)
  testing <- testing[inTest,]
  validation <- testing[-inTest,]
  
  training <- impute_NA(res, training, excludeCol = "y", imputeMethod = trainImpMethod, imputeSetName = "training")$result
  testing <- impute_NA(res, testing, excludeCol = "y", imputeMethod = testImpMethod, imputeSetName = "testing")$result
  validation <- impute_NA(res, validation, excludeCol = "y", imputeMethod = validateImpMethod, imputeSetName = "validation")$result
  
  
  res$datasets <- list(
    beeData = beeData,
    training = training,
    testing = testing,
    validation = validation
  )
  
  print("Data partitioning result: ")
  print(lapply(res$datasets, dim))
  
  res
  
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
  
  
  mmod <- splitData(beeData, trainImpMethod="medianImpute")
  mmod$ensemble <- list(modelFitAsIs, fit2, fit3)
  pr1<- mmod$calcComb()
  str(pr1)
  pr1
  
  data.frame(pr1[[1]])
  
  mmod$combFits[[1]]$method
  
  dd1 <- pr1[[1]]
  str(dd1[,-grep("y",names(dd1))])
  lapply(dd1[],length)
  
  
  dd<-lapply(pr1, data.frame, y=data.frame(na.omit(mmod$datasets$testing)$y))
  dd<-lapply(pr1, data.frame)
  
  cf <- list()
  cf <- list(unlist(cf), "sds")
  
  cf <- c(cf, "aaaa")
  cf <- c(cf, "bbbb")
  cf <- c(cf, fit2)
  cf[length(cf)+1] <- list(fit2)
  
  str(cf)
  
  cf
  
  
  dd<-data.frame(pr1[[1]])
  colnames(dd)<-seq_along(dd)
  
  str(pr1)
  
  lapply(pr1, length)
  
  write(mmod$getDescription(), "test.desc")
  
  
  modelFitAsIs <<- train(y ~ ., method = "nnet", data = training, 
                         trControl = trainControl(method = "cv", verboseIter = T
                                                  , number = 5
                         )
                         # ,preProcess = c("center", "scale")
                         # ,preProcess = imputeMethod
                         # ,preProcess = "pca"
                         # ,tuneGrid = data.frame(fL = 1, usekernel = T)     # for nb
                         # ,tuneGrid = data.frame(maxdepth = 5)            # for rpart2
                         # ,tuneGrid = data.frame(mtry=18)     # for rf, Boruta
                         # ,prox = T
                         # ,tuneGrid = data.frame(maxdepth=9, mfinal = 150)     # for adaBag
                         # ,tuneGrid = data.frame(size=1:10, decay = 0.17)     # for nnet
  )
  
  modelFitAsIs
  
  
  fit2 <- train(y ~ ., method = "rf", data = training, 
                trControl = trainControl(method = "cv", verboseIter = T, number = 5))
  
  fit2
  
  fit3 <<- train(y ~ ., method = "rpart2", data = training, 
                         trControl = trainControl(method = "cv", verboseIter = T
                                                  , number = 5
                         )
                         ,tuneGrid = data.frame(maxdepth = 5)            # for rpart2
  )
  
  fit3
  
  pr1<- mmod$calcComb()
  pr1
  
  class(unlist(pr1[1,]))
  
  mmod$ensemble[unlist(pr1[3,])]
  
  if(any(c(F,F,F))){
    print("da")
  }
  
  pred1 <- predict(modelFitAsIs, newdata = na.omit(testing))
  pred2 <- predict(fit2, newdata = testing)
  predDF <- data.frame(pred1, pred2, y=na.omit(testing)$y)
  
  combFit <- train(y~., data=predDF, method="rpart2",
                   trControl = trainControl(method = "cv", verboseIter = T, number = 5))
  combFit
  
  
  
  combPred <- predict(combFit,predDF)
  
  pred1V <- predict(modelFitAsIs,validation); 
  pred2V <- predict(fit2,validation)
  predVDF <- data.frame(pred1=pred1V,pred2=pred2V)
  combPredV <- predict(combFit,predVDF)
  
  
  
  
  
  res1 <- data.frame(
    confusionMatrix(pred1, na.omit(testing)$y)$overall[1]
    ,confusionMatrix(pred2, na.omit(testing)$y)$overall[1]
    ,confusionMatrix(combPred, na.omit(testing)$y)$overall[1]
  )
  names(res1)<-c(modelFitAsIs$method, fit2$method,"combPredV")
  res1
  
  res2 <- data.frame(
    confusionMatrix(pred1V, na.omit(validation)$y)$overall[1]
    ,confusionMatrix(pred2V, na.omit(validation)$y)$overall[1]
    ,confusionMatrix(combPredV, na.omit(validation)$y)$overall[1], row.names = "AccuracyV"
  )
  names(res2)<-c(modelFitAsIs$method, fit2$method,"combPredV")
  
  rbind(res1, res2)
  
  
  
} 

test <- function(imputeMethod = "medianImpute", dataTrain, modelData){
  
  print("start testing !!!")
  
  final_test <- read.csv("unpacked/test.csv")
  
  final_test <- preprocessColumns(final_test, delCol = c()) #"x7"
  final_test <- impute_NA(final_test, excludeCol = "ID", 
                          imputeMethod = imputeMethod, imputeSetName = "final_test")$result
  
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
                             "dim(dataTrain): ", capture.output(lapply(dataTrain, dim)), 
                             "dim(final_test): ", capture.output(dim(final_test)),
                             "\nimputingInfo: ",  capture.output(imputingInfo),
                             "\nFIT call: ", capture.output(modelFitAsIs$call),
                             "\nTRAINING vars: ", capture.output(names(dataTrain$training)),
                             "\n\nMODEL SETTINGS: ", capture.output(modelFitAsIs),
                             "\n\npredicted df head: ", capture.output( head(result_df, 10))
  ))
  rm(imputingInfo, inherits = T)
  write(desc, file=paste(file_prefix, ".desc", sep=""))
}

impute_NA <- function(mmodel, dtaset, excludeCol = c(), imputeSetName, imputeMethod){
  
  print(sprintf("impute_NA: start preprocess with predict(method = %s)", imputeMethod))
  
  preProc <- NULL
  
  if(imputeMethod == "na_omit"){
    result <- na.omit(dtaset)
  }else{
    if (length(excludeCol) > 0){
      preprocData <- dtaset[,-which(names(dtaset) %in% excludeCol)]
      restData <- data.frame(dtaset[,which(names(dtaset) %in% excludeCol)])
      if(length(excludeCol) == 1){
        names(restData) <- excludeCol
      }
      preProc <- preProcess(preprocData, method = imputeMethod)
      result <- predict(preProc, preprocData)
      result <- cbind(result, restData)
      print(sprintf("exclude cols: %s", capture.output(excludeCol)))
    } else{
      preProc <- preProcess(dtaset, method = imputeMethod)
      result <- predict(preProc, dtaset)
    }
  }
  
  mmodel$addImputingInfo(data.frame(ds=imputeSetName,imMetod = imputeMethod))
  
  NAs <- result[!complete.cases(result),]
  print("Nas dataset dim:")
  print(dim(NAs))
  print("impute_NA finish!")
  list(result=result, preProc = preProc)
  #     result
  
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