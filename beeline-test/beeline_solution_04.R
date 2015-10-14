

library(gtools)    
library(caret)
source(file = "model_trainer.R")

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

runParallel <- function(){
  library(doParallel)
  cl <- makeCluster(4, type='PSOCK')
  registerDoParallel(cl)
  
  
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
  
  otherData <- data[-inTrain,]
  
  inTest <- createDataPartition(y=otherData$y, p = ptest, list=F)
  testing <- otherData[inTest,]
  validation <- otherData[-inTest,]
  
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
trainModel <- function(){
  
  
  set.seed(21121)
  
  init()
  
#   runParallel()
  
  mmod <<- splitData(beeData, trainImpMethod="bagImpute")
  
#   fit0 <<- readRDS("model_Boruta_7579.mod")
  
  fit1 <<- train(y ~ ., method = "AdaBag", data = mmod$datasets$training, 
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
                 ,tuneGrid = data.frame(maxdepth=9, mfinal = 150)     # for adaBag
                 #                          ,tuneGrid = data.frame(size=1:10, decay = 0.17)     # for nnet
  )
  
  
  fit2 <<- train(y ~ ., method = "rf", data = mmod$datasets$training, 
                 trControl = trainControl(method = "cv", verboseIter = T, number = 5)
                 ,tuneGrid = data.frame(mtry=18)     # for rf, Boruta
  )
  
  fit3 <<- train(y ~ ., method = "rpart2", data = mmod$datasets$training, 
                 trControl = trainControl(method = "cv", verboseIter = T
                                          , number = 5
                 )
                 ,tuneGrid = data.frame(maxdepth = 3:8)            # for rpart2
  )
  
fit4 <<- train(y ~ ., method = "svmLinear", data = mmod$datasets$training, 
               trControl = trainControl(method = "cv", verboseIter = T
                                        , number = 5
               )
#                ,tuneGrid = data.frame(maxdepth = 3:8)            # for rpart2
)

# Sys.setenv(JAVA_HOME='C:\\ProgramFiles\\Java\\jdk1.7.0_40\\jre')
# Sys.getenv("JAVA_HOME")

fit5 <<- train(y ~ ., method = "gbm", data = mmod$datasets$training, 
               trControl = trainControl(method = "cv", verboseIter = T
                                        , number = 3
               )
               ,tuneGrid = expand.grid(interaction.depth = 2, n.trees = 150, shrinkage = 0.1, n.minobsinnode = 10)
               
)

saveRDS(fit5, "model_gbm_0756.mod")

fit6 <<- train(y ~ ., method = "bagFDA", data = mmod$datasets$training[1:5000,], 
               trControl = trainControl(method = "cv", verboseIter = T
                                        , number = 3
               )
               ,tuneGrid = expand.grid(nprune = c(35,45,25), degree = 1:3)
               
)

plot(fit6)

# seq(from = 0.00001, to = 0.01, by = 0.001)
  
  mmod$ensemble <- list(fit1, fit2, fit3)
  
  mmod$calcComb()
  
  mmod$calcValidation()
  
  write(mmod$getDescription(), "test.desc")
  
#   runSeq()
  
  mmod
} 

#dataTrain, modelData
test <- function(imputeMethod = "bagImpute", mmodel, combfitIndex = 1){
  
  print("start testing !!!")
  
  if(length(mmodel$datasets$final_test) == 0){
    final_test <- read.csv("unpacked/test.csv")
    
    final_test <- preprocessColumns(final_test, delCol = c()) #"x7"
    final_test <- impute_NA(mmodel, final_test, excludeCol = "ID", 
                            imputeMethod = imputeMethod, imputeSetName = "final_test")$result
    
    mmodel$addDataset("final_test", final_test)
  } else {
    final_test <- mmodel$datasets$final_test
  }

  final_pred <- mmod$predictComb( combFitIndex = combfitIndex, newdata = final_test)
    
  str(final_pred)
  print(head(unlist(final_pred)))
  print(length(final_pred))
  
  result_df <- data.frame(ID=na.omit(final_test)$ID, y=unlist(final_pred))
  head(result_df, 10)
  
  mmodel$result_df <- result_df
  
  #write predictions
  file_prefix <- paste("result",format(Sys.time(), "%y%m%d_%H%M"),"_EA", round(max(mmod$combFits[[combfitIndex]]$fit$results$Accuracy), 5), sep="")
  print(paste("write result with file prefix:",file_prefix))
  
  write.table(result_df, 
              file = paste(file_prefix, ".csv", sep=""), 
              quote = F, row.names = F, sep = ",")
  #write desciption
  desc <- mmodel$getDescription()
  #   rm(imputingInfo, inherits = T)
  write(desc, file=paste(file_prefix, ".desc", sep=""))
}

impute_NA <- function(mmodel, dtaset, excludeCol = c(), imputeSetName, imputeMethod){
  
  print(sprintf("impute_NA: start preprocess with predict(method = %s)", imputeMethod))
  
  preProc <- NULL
  
  if(imputeMethod == "na_omit"){
    result <- na.omit(dtaset)
  } else if (imputeMethod == "none"){
    result <- dtaset
  } else {
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