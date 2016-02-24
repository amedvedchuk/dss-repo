library(caret)
library(plyr)
library(dplyr)
library(R.utils)
library(car)

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

preBasic_noImpute <- function(data){
  print("preBasic_noImpute: start")
  data$MSISDN <- as.character(data$MSISDN)
  data$SEX <- as.factor(tolower(as.character(data$SEX)))
  data$TARRIF_ID <- as.factor(data$TARRIF_ID)
  data <- data[,-which(names(data) %in% c("SUBS_NAME", "MSISDN"))]
  data <- data[,-which(names(data) %in% c("TARRIF_ID", "DUAL_SIM_PROBABILITY", "SIM_PRIORITY"))]
  print(dim(data))
  print("preBasic_noImpute: stop")
  data
}

imputeNA_as0_DIFF <- function(data){
  print("--- imputeNA_as0_DIFF: start ---")
  print(missInfo(data))
  tmp <- data %>% select(contains("DIFF")) %>% 
    apply(., 2, function(column) recode(column, "NA = 0")) %>%  
    as.data.frame()
  
  tmp <- data.frame(select(data, -contains("DIFF")), tmp)
  print("--- imputeNA_as0_DIFF: after imputing:")
  print(missInfo(tmp))
  print("--- imputeNA_as0_DIFF: stop ---")
  tmp
}

reduce_corrPredictors <- function(data){
  print("--- reduce_corrPredictors: start ---")
  ## TODO: try to use=pairwise.complete.obs  in cor function.
  fc <- findCorrelation(cor(select(data, -SEX), use = "complete.obs"), cutoff = 0.85)
  fc <- fc[!is.na(fc)] 
  data_uncor <- data[,-fc]
  print("reduce correlated variables:")
  print(names(data)[fc])
  print("--- reduce_corrPredictors: stop ---")
  data_uncor
}

imputeNA_Bag_after_DIFF <- function(dsets){
  print("--- imputeNA_Bag_after_DIFF: start ---")
  print(missInfo(dsets$training))
  print(missInfo(dsets$testing))
  print(missInfo(dsets$validation))
  #     select(AVG_BALANCE_BEFORE_REF_1M, AVG_REFILL_AMOUNT_1M, REFILL_FREQ_1M, 
  #            REF_SHARE_SPEND_75_3M, REF_SHARE_SPEND_95_3M, AVG_BALANCE_BEFORE_REF_6M, 
  #            AVG_DAYS_BETW_REF_6M, AVG_REFILL_AMOUNT_6M, REFILL_FREQ_6M, REFILL_RATIO, 
  #            REF_SPEND_SPEED_95_3M, REF_SHARE_SPEND_75_12M, REF_SHARE_SPEND_95_12M, 
  #            REF_SPEND_SPEED_95_12M, DAYS_SINCE_LAST_REF, REF_SPEND_SPEED_75_3M, 
  #            REF_SPEND_SPEED_75_12M, DAYS_INACT_MAX, AVG_DAYS_INACT_SUCC_6) %>%   
  
  # preProcData <- dset$training %>% select(-SEX, -contains("DIFF"))
  
  excludeCols <- function(dataset){
    dataset %>% select(-SEX, -contains("DIFF"))
  }
  
  imputeForDataset <- function(dataset){
    tmp <- predict(preProc, excludeCols(dataset))
    tmp <- cbind(SEX = dataset$SEX, tmp, select(dataset, contains("DIFF")))
    tmp
  }
  
  preProc <- preProcess(excludeCols(dsets$training), method = "bagImpute")
  dsets$training <- imputeForDataset(dsets$training)
  dsets$testing <- imputeForDataset(dsets$testing)
  dsets$validation <- imputeForDataset(dsets$validation)
  
  
  print("--- imputeNA_Bag_after_DIFF: after imputing:")
  print(missInfo(dsets$training))
  print(missInfo(dsets$testing))
  print(missInfo(dsets$validation))
  print("--- imputeNA_Bag_after_DIFF: stop ---")
  dsets
}

missInfo <- function(data){
  missed <- colSums(is.na(data))
  missed <- data.frame(cols=names(missed), missed)
  missed <- missed %>% filter(missed > 0) %>% arrange(desc(missed))
  missed
}

preLog <- function(data){
  print("preLog: start")
  data <- data.frame(lapply(data, function(x){if(is.numeric(x) && min(x)>=0) log(x+1) else x}))
  print("preLog: stop")
  data
}

preLog_dset <- function(dsets){
  print("preLog_dset: start")
  dsets$training <- preLog(dsets$training)
  dsets$training <- preLog(dsets$testing)
  dsets$training <- preLog(dsets$validation)
  print("preLog_dset: stop")
  dsets
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

runScenario <- function(...){
  
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

testMethods <- function(dsets, saveModels = F, methods){
  
  # res <- data.frame(stringsAsFactors = F)
  # res <- list()
  
  file_name <- paste("model_result_",format(Sys.time(), "%Y-%m-%d_%H_%M"), ".log", sep = "")
  first <- TRUE
  print(file_name)
  
  finalRes <- laply(methods, function(mt){
    start <- currentTimeMillis.System()
    
    fit <- NULL
    
    cfres <- tryCatch({
      
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
      cfres$result <- "SUCCESS"
      cfres
    }, error = function(err){
      print(paste("ERROR occurred in cycle: ",err))
      cfres<- data.frame(time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                         method = mt, 
                         train = NA, 
                         testing = NA, 
                         stringsAsFactors = F, row.names = NULL)
      cfres$validation <- NA
      cfres$runTime <- round(as.numeric((currentTimeMillis.System()-start)/1000), 1)
      cfres$train_cols <- ncol(dsets$training)
      cfres$train_rows <- nrow(dsets$training)
      cfres$test_rows <- nrow(dsets$testing)
      cfres$val_rows <- nrow(dsets$validation)
      cfres$result <- gsub("\n", "|", as.character(err))
      cfres
    })
    
    print(cfres)
    write.table(cfres, file_name, append = T, row.names = F, col.names = first, sep = "\t")
    if(first){
      first <<- FALSE
    }
    print("------------------------------------------------")
    if(saveModels){
      cfres$model$fit <- fit
    }
    # res <- rbind(res, cfres)
    # res[[mt]] <- cfres
    cfres
    
    # res
    
  })
  
  dimnames(finalRes)[[1]] <- methods
  finalRes  
  # res
}
