

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

runEnsemble <- function(){
  
  mmod <<- readRDS("mmod_v01.env")
  
  m_gbm <<- readRDS("model_gbm_fit5_7554782.mod")
  m_boruta <<- readRDS("model_Boruta_750907.mod")
  m_rf <<- readRDS("model_rf_fit12_7505931.mod")
  m_ranger <<- readRDS("model_ranger_fit8_7504503.mod")
  m_wsrf <<- readRDS("model_wsrf_fit11_7495070.mod")
  m_gcvEarth <<- readRDS("model_gcvEarth_fit7_7484227.mod")
  m_bagFDA <<- readRDS("model_bagFDA_fit6_73818.mod")
  
#   m_earth <<- readRDS("model_earth_fit9_7483647.mod")
#   m_AdaBag <<- readRDS("model_AdaBag_7480217.mod")
  # m_fda <<- readRDS("model_fda_fit10_7359654.mod")
#   m_rpart2 <<- readRDS("model_rpart2_7104534.mod")
  
  dim(expand.grid(data.frame(matrix(rep(c(T,F),length(mmod$ensemble)),nrow = 2,ncol=length(mmod$ensemble)))))
  
  saveRDS(mmod, "mmod_v02.mod")
  
  mmod$ensemble <- list(m_gbm, m_boruta, m_rf, m_ranger, m_wsrf, m_gcvEarth, m_bagFDA)
  mmod$calcComb()
  mmod$calcValidation()
  
  write(mmod$getDescription(), "test.desc")
  
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 1)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 2)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 3)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 4)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 5)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 6)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 7)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 8)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 9)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 10)
  
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 11)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 12)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 13)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 14)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 15)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 16)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 17)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 18)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 19)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 20)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 21)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 22)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 23)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 24)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 25)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 26)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 27)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 28)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 29)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 30)
  
  
  
  
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 31)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 32)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 33)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 34)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 35)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 36)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 37)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 38)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 39)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 40)
  
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 51)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 52)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 53)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 54)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 55)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 56)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 57)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 58)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 59)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 60)
  
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 61)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 62)
  test(imputeMethod = "bagImpute", mmodel = mmod, combfitIndex = 63)
    
}

# analyzeData(beeNoLFactors)
trainModel <- function(){
  
  
  set.seed(21121)
  
  init()
  
  #   runParallel()
  
  mmod <<- splitData(beeData, trainImpMethod="bagImpute")
  
  
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
  
  
  # Sys.setenv(JAVA_HOME='C:\\ProgramFiles\\Java\\jdk1.7.0_40\\jre')
  # Sys.getenv("JAVA_HOME")
  
  fit6 <<- train(y ~ ., method = "bagFDA", data = mmod$datasets$training, 
                 trControl = trainControl(method = "cv", verboseIter = T
                                          , number = 5
                 )
                 ,tuneGrid = expand.grid(nprune = c(45,55), degree = 1:3)
                 
  )
  
  saveRDS(fit6, "model_bagFDA_fit6.mod")
  
  
  
  fit5 <<- train(y ~ ., method = "gbm", data = mmod$datasets$training, 
                 trControl = trainControl(method = "cv", verboseIter = T
                                          , number = 3
                 )
                 ,tuneGrid = expand.grid(interaction.depth = 2, n.trees = 150, shrinkage = 0.1, n.minobsinnode = 10)
                 
  )
  
  saveRDS(fit5, "model_gbm_fit5.mod")
  
  
  fit7 <<- train(y ~ ., method = "gcvEarth", data = mmod$datasets$training, 
                 trControl = trainControl(method = "cv", verboseIter = T
                                          , number = 5
                 )
                 ,tuneGrid = expand.grid(degree = 1:3)
                 
  )
  saveRDS(fit7, "model_gcvEarth_fit7.mod")
  
  
  fit8 <<- train(y ~ ., method = "ranger", data = mmod$datasets$training, 
                 trControl = trainControl(method = "cv", verboseIter = T
                                          , number = 5
                 )
                 # ,tuneGrid = expand.grid(mtry = c(22,30,43))
                 
  )
  saveRDS(fit8, "model_ranger_fit8.mod")
  
  
  fit9 <<- train(y ~ ., method = "earth", data = mmod$datasets$training, 
                 trControl = trainControl(method = "cv", verboseIter = T
                                          , number = 5
                 )
                 ,tuneGrid = expand.grid(degree = 1:3, nprune = c(2, 26,51))
                 
  )
  saveRDS(fit9, "model_earth_fit9.mod")
  
  
  
  fit10 <<- train(y ~ ., method = "fda", data = mmod$datasets$training, 
                  trControl = trainControl(method = "cv", verboseIter = T
                                           , number = 5
                  )
                  ,tuneGrid = expand.grid(degree = 1:3, nprune = c(2, 26,51))
                  
  )
  saveRDS(fit10, "model_fda_fit10.mod")
  
  
  fit11 <<- train(y ~ ., method = "wsrf", data = mmod$datasets$training, 
                  trControl = trainControl(method = "cv", verboseIter = T
                                           , number = 5
                  )
                  ,tuneGrid = expand.grid(mtry = c(2,22,18,43))
                  
  )
  
  saveRDS(fit11, "model_wsrf_fit11.mod")
  
  
  fit12 <<- train(y ~ ., method = "rf", data = mmod$datasets$training, 
                  trControl = trainControl(method = "cv", verboseIter = T
                                           , number = 5
                  )
                  ,tuneGrid = expand.grid(mtry = c(22,18,43))
                  
  )
  saveRDS(fit12, "model_rf_fit12.mod")
  
  fit13 <<- train(y ~ ., method = "AdaBag", data = mmod$datasets$training, 
                  trControl = trainControl(method = "cv", verboseIter = T
                                           , number = 5
                                           ,tuneGrid = data.frame(maxdepth=2:11, mfinal = 150)     # for adaBag
                  )
  )
  saveRDS(fit13, "model_AdaBag_fit13.mod")
  
  
  
  
  plot(fit6)
  
  # seq(from = 0.00001, to = 0.01, by = 0.001)
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
  
  print("start prediction...")
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