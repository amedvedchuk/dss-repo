
runParallel <- function(){
  library(doParallel)
  cl <- makeCluster(4, type='PSOCK')
  registerDoParallel(cl)
  
  
}

runSeq <- function(){
  registerDoSEQ()
}


library(caret)
library(dplyr)
library(plyr)

set.seed(1221)

data <- read.csv("data/export_2015-11-20_M201510.dsv", sep = ";", dec = ",", header = T)
data$MSISDN <- as.character(data$MSISDN)
data$SEX <- as.factor(tolower(as.character(data$SEX)))
data$TARRIF_ID <- as.factor(data$TARRIF_ID)
data <- data[,-which(names(data) %in% c("SUBS_NAME", "MSISDN"))]
data <- data[,-which(names(data) %in% c("TARRIF_ID", "DUAL_SIM_PROBABILITY", "SIM_PRIORITY"))]
data[is.na(data)] <- 0

#  log(x>0 && is.numeric)
data <- data.frame(lapply(data, function(x){if(is.numeric(x) && min(x)>=0) log(x+1) else x}))



str(data[,1:60])
str(data[,60:118])

summary(data)


dim(res)

str(rf_0.6$trainingData)

inTrain <- createDataPartition(y=data$SEX, p = 0.06, list=F)
# inTrain <- as.numeric(rownames(rf_0.6$trainingData))

training <- data[inTrain,]
otherData <- data[-inTrain,]
inTest <- createDataPartition(y=otherData$SEX, p = 0.7, list=F)
testing <- otherData[inTest,]
validation <- otherData[-inTest,]

rf_0.06 <- fit1
rf_0.6 <- fit
rf_0.06_log <- fit1
saveRDS(rf_0.6, "rf_0.6.rds")
saveRDS(rf_0.06, "rf_0.06.rds")
saveRDS(rf_0.06_log, "rf_0.06_log.rds")


rf_0.6 <- readRDS("rf_0.6.rds")
rf_0.06 <- readRDS("rf_0.06.rds")


fit1 <<- train(SEX ~ ., method = "rf", data = training, 
               trControl = trainControl(method = "cv", verboseIter = T
                                        , number = 3
               )
              # ,tuneGrid = data.frame(mtry = 15)            # for fr
               # ,tuneGrid = data.frame(maxdepth = 3:8)            # for rpart2
)
predictions <- predict(fit1, newdata = testing)
cfres<- data.frame(train = max(fit1$results$Accuracy), testing = confusionMatrix(predictions, testing$SEX)$overall[1])
predictions <- predict(fit1, newdata = validation)
cfres$validation <- confusionMatrix(predictions, validation$SEX)$overall[1]
cfres


fit1
str(training)
summary(fit1)
fit$finalModel
varImp(fit1)
plot(fit)




vimp <- varImp(rf_0.6)
vimp <- varImp(rf_0.06_log)

vimp
# 2 ----------------- reduced dataset
reduced_rt <- training[,rownames(vimp$importance)[order(vimp$importance$Overall, decreasing=TRUE)][1:20]]
reduced_rt$SEX <- training$SEX

# reduced_rt <- testing[,rownames(vimp$importance)[order(vimp$importance$Overall, decreasing=TRUE)][1:20]]
# reduced_rt$SEX <- testing$SEX


# reduced_rt <- data.frame(lapply(reduced_rt, function(x){if(is.numeric(x) && min(x)>=0) log(x+1) else x}))


# reduced_rt$TARRIF_ID <- training$TARRIF_ID
# reduced_rt$DUAL_SIM_PROBABILITY <- training$DUAL_SIM_PROBABILITY

dim(reduced_rt)
str(reduced_rt)
summary(reduced_rt)

fit2 <<- train(SEX ~ ., method = "rf", data = reduced_rt, 
              trControl = trainControl(method = "cv", verboseIter = T
                                       , number = 3
              )
              # ,tuneGrid = data.frame(mtry = 15)            # for fr
              # ,tuneGrid = data.frame(maxdepth = 7:15)            # for rpart2
              # ,tuneGrid = data.frame(cp = 0.01)            # for rpart2
)
predictions <- predict(fit2, newdata = testing)
cfres<- data.frame(train = max(fit2$results$Accuracy), testing = confusionMatrix(predictions, testing$SEX)$overall[1])
predictions <- predict(fit2, newdata = validation)
cfres$validation <- confusionMatrix(predictions, validation$SEX)$overall[1]
cfres



fit2

fit$finalModel
varImp(fit2)
plot(fit2)

predictions <- predict(fit, newdata = testing)
confusionMatrix(predictions, testing$SEX)


# ===================== functions
testMethods(reduced_rt, testing, validation, c("glm","rpart"))

# FAILED: "wsrf"
# DONE: "gbm","bagFDA", "gcvEarth", "ranger", "earth"

testMethods(reduced_rt, testing, validation, c( "fda", "xgbTree", "xgbLinear", "C5.0", "RRFglobal", "rfRules", "treebag"
                                               ,"evtree", "AdaBoost.M1", "J48", "OneR", "JRip", "extraTrees", "RRF", "LMT", "C5.0Tree", "C5.0Rules", "ctree2"))
 
# blackboost, gamboost, glmboost rotationForest rotationForestCp bartMachine rFerns dwdLinear rmda dwdPoly dwdRadial binda logicBag LogitBoost logreg plr polr

source("a_ml_functions.R")

dt <- preProcess(readData, preBasic, preLog, makeParts_006, reduceTrain_rf006_log_20)
str(dt)

preTest1 <- function(data){
  print(data)
}
preTest2 <- function(res){
  r <- paste(res, "res2")
  r
}

preProcess(preTest1, preTest2)
preTest1()
preTest2("sdsd")


t <- Sys.time()
dif <- t - Sys.time()
as.numeric(dif)

format(dif, "%Y-%m-%d %H:%M:%S")
cfres$date <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
write.table(cfres, "test.log", append = T, row.names = F, col.names = F)

# ===================== Exploratory


res <- head(reduced_rt)
select(res, -SEX)

# res[,unlist(colwise(function(x){is.numeric(x) && min(x)>0})(res))]
# 
# filter(res, colwise(mean, data=res))
# filter(res, colwise(min)(select(res, -SEX))>0)

# res2 <- data.frame(apply(res[,unlist(colwise(function(x){is.numeric(x) && min(x)>0})(res))],2, log))
res2 <- data.frame(lapply(res, function(x){if(is.numeric(x) && min(x)>=0) log(x+1) else x}))

str(res2)
str(res)

summary(res2)

qplot(log(AVG_ONNET_INC_MOU_6M+1), log(AVG_ONNET_INC_COUNT_CALLS_6M+1), data = reduced_rt, facets = ~SEX)
qplot(AVG_ONNET_INC_MOU_6M, AVG_ONNET_INC_COUNT_CALLS_6M, data = reduced_rt, facets = ~SEX)


qplot(SEX, REF_SPEND_SPEED_95_12M, data = reduced_rt, geom = "boxplot")
qplot(SEX, log(AVG_ONNET_INC_MOU_6M+1), data = reduced_rt, geom = "boxplot")


qplot(AVG_BALANCE_BEFORE_REF_6M, data=reduced_rt, color = SEX)
qplot(log(AVG_BALANCE_BEFORE_REF_6M+1), data=reduced_rt, color = SEX)

qplot((reduced_rt$AVG_ONNET_INC_MOU_6M-mean(reduced_rt$AVG_ONNET_INC_MOU_6M))/sd(reduced_rt$AVG_ONNET_INC_MOU_6M), data=reduced_rt)


# ------------------------------------------

levels(data$DUAL_SIM_PROBABILITY)
levels(data$SIM_PRIORITY)
levels(data$TARRIF_ID)

res <- data.frame(lapply(names(first), function(x){
  print(x)
  first[is.na(first[[x]]),][[x]]<-0
}))

first[is.na(first)] <- 0

first[is.na(first$KYIVSTAR_DIFF_A_INC_1M),]$KYIVSTAR_DIFF_A_INC_1M

res <- is.na(first$SUBS_NAME)
