library(caret)
# library(dplyr)
library(plyr)
library(R.utils)

if(Sys.getenv("JAVA_HOME")==""){
  Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jdk1.8.0_65")
}

readData <- function(){
  print("readData: start")
  data <- read.csv("data/export_2015-11-20_M201510.dsv", sep = ";", dec = ",", header = T)
  print(dim(data))
  print("readData: stop")
  data
}

preBasic <- function(data){
  print("preBasic: start")
  data$MSISDN <- as.character(data$MSISDN)
  data$SEX <- as.factor(tolower(as.character(data$SEX)))
  data$TARRIF_ID <- as.factor(data$TARRIF_ID)
  data <- data[,-which(names(data) %in% c("SUBS_NAME", "MSISDN"))]
  data <- data[,-which(names(data) %in% c("TARRIF_ID", "DUAL_SIM_PROBABILITY", "SIM_PRIORITY"))]
  data[is.na(data)] <- 0
  print(dim(data))
  print("preBasic: stop")
  data
}

preLog <- function(data){
  print("preLog: start")
  data <- data.frame(lapply(data, function(x){if(is.numeric(x) && min(x)>=0) log(x+1) else x}))
  print("preLog: stop")
  data
}

makeParts_006 <- function(data) {
  print("makeParts_006: start")
  inTrain <- createDataPartition(y=data$SEX, p = 0.06, list=F)
  # inTrain <- as.numeric(rownames(rf_0.6$trainingData))
  
  training <- data[inTrain,]
  otherData <- data[-inTrain,]
  inTest <- createDataPartition(y=otherData$SEX, p = 0.7, list=F)
  testing <- otherData[inTest,]
  validation <- otherData[-inTest,]
  dsets = list(
    training = training,
    testing = testing,
    validation = validation
  )
  laply(dsets, function(set){print(dim(set))})
  print("makeParts_006: stop")
  dsets
}

reduceTrain_rf006_log_20 <- function(dsets){
  print("reduceTrain_rf006_log_20: start")
  vimp <- varImp(readRDS("rf_0.06_log.rds"))
  reduced_rt <- dsets$training[,rownames(vimp$importance)[order(vimp$importance$Overall, decreasing=TRUE)][1:20]]
  reduced_rt$SEX <- dsets$training$SEX
  dsets$training <- reduced_rt
  str(reduced_rt)
  print("reduceTrain_rf006_log_20: stop")
  dsets
}

preProcess <- function(...){
  
  funList <- list(...)
  
  lastRes <- NULL
  
  for (i in seq_along(funList)){
    if(i==1){
      lastRes <- funList[[i]]()
    } else {
      lastRes <- funList[[i]](lastRes)
    }
  }
  lastRes
  
  # lapply(list(...), function(FUN){FUN(data)})
}

testMethods <- function(dsets, methods){
  
  res <- data.frame(stringsAsFactors = F)
  
  file_name <- paste("model_result_",format(Sys.time(), "%Y-%m-%d_%H_%M"), ".log", sep = "")
  first <- TRUE
  print(file_name)
  
  laply(methods, function(mt){
    start <- currentTimeMillis.System()
    fit <- train(SEX ~ ., method = mt, data = dsets$training, 
                 trControl = trainControl(method = "cv", verboseIter = T
                                          , number = 3
                 )
    )
    predictions <- predict(fit, newdata = dsets$testing)
    cfres<- data.frame(time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                       method = mt, 
                       train = max(fit$results$Accuracy), 
                       testing = confusionMatrix(predictions, dsets$testing$SEX)$overall[1], 
                       stringsAsFactors = F, row.names = NULL)
    predictions <- predict(fit, newdata = dsets$validation)
    cfres$validation <- confusionMatrix(predictions, dsets$validation$SEX)$overall[1]
    cfres$runTime <- round(as.numeric((currentTimeMillis.System()-start)/1000), 1)
    cfres$train_cols <- ncol(dsets$training)
    cfres$train_rows <- nrow(dsets$training)
    cfres$test_rows <- nrow(dsets$testing)
    cfres$val_rows <- nrow(dsets$validation)
    print(cfres)
    write.table(cfres, file_name, append = T, row.names = F, col.names = first, sep = "\t")
    if(first){
      first <<- FALSE
    }
    print("------------------------------------------------")
    res <- rbind(res, cfres)
    res
  })
}
