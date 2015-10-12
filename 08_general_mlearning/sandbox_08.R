

#Quizz2 Q2 =====================================================

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

hist(log(training$Superplasticizer)+1)

hist(training$Superplasticizer)
?log


#Quizz2 Q3 =====================================================

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

# IL names
grep(names(adData), pattern = "^IL.*", value = T)
nameIdx <- grep(names(adData), pattern = "^IL.*")

aDataReduced <- adData[,nameIdx]
preProc <- preProcess(aDataReduced, method = "pca", thresh = 0.9)
preProc

# Call:
#     preProcess.default(x = aDataReduced, method = "pca", thresh = 0.9)
# 
# Created from 333 samples and 12 variables
# Pre-processing: principal component signal extraction, scaled, centered 
# 
# PCA needed 9 components to capture 90 percent of the variance


#Quizz2 Q4 =====================================================

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

nameIdx <- grep(names(adData), pattern = "^IL.*")
trainIL <- training[,c(1,nameIdx)]

modelFitAsIs <- train(diagnosis ~ ., method = "glm", data = trainIL, trControl = trainControl(verboseIter = T, number = 5))
modelFitAsIs
?trainControl



preProc <- preProcess(trainIL[-1], method = "pca", thresh = 0.8)
trainIL_PC <- predict(preProc, trainIL[-1])
modelFitAsIs <- train(trainIL$diagnosis ~ ., method = "glm", data = trainIL_PC)
modelFitAsIs

# ============================================================== 
#Quizz3 Q1 =====================================================

library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
library(rattle)

# intrain <- createDataPartition(y = segmentationOriginal$Case, list = F)

training <- segmentationOriginal[segmentationOriginal$Case == "Train",]
testing <- segmentationOriginal[segmentationOriginal$Case == "Test",]

training<-training[,-2]
testing<-testing[,-2]


set.seed(125)

fit1 <- train(Class ~ ., method = "rpart", data = training)

# plot(fit1$finalModel)
fancyRpartPlot(fit1$finalModel)

fit1$finalModel
fit1
varImp(fit1)


#Quizz3 Q3 =====================================================

library(pgmm)
data(olive)
olive = olive[,-1]
fit2 <- train(Area ~ ., method = "rpart", data = olive)
newdata = as.data.frame(t(colMeans(olive)))
newdata
predict(fit2, newdata = newdata)

fit2$finalModel




#Quizz4 Q3 =====================================================

library(ElemStatLearn)
library(caret)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)

fit4 <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, method = "glm", family = binomial, data = trainSA)
fit4
varImp(fit4)


missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

missClass(values = testSA$chd, predict(fit4, testSA))
missClass(values = trainSA$chd, predict(fit4, trainSA))



#Quizz3 Q5 =====================================================

library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)

fit5 <- train(y ~ ., method = "rf", data = vowel.train, trControl = 
                  trainControl(verboseIter = T, number =1), allowParallel=TRUE)

varImp(fit5)
fit5

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Quizz4 Q1 =====================================================

library(caret)
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 

set.seed(33833)
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

fit1 <- train(y~., data = vowel.train, method = "rf")
fit2 <- train(y~., data = vowel.train, method = "gbm")

fit1
fit2

pred1 <- predict(fit1, vowel.test)
pred2 <- predict(fit2, vowel.test)

pred1

qplot(pred1, pred2, color = y, data = vowel.test)

predDF <- data.frame(pred1, pred2, y = vowel.test$y)
acc1 = sum(pred1 == vowel.test$y) / length(pred1)
acc2 = sum(pred2 == vowel.test$y) / length(pred2)

acc1
acc2

confusionMatrix(pred1, vowel.test$y)
confusionMatrix(pred2, vowel.test$y)


agreeSub = vowel.test[pred1 == pred2,]
pred_comb = predict(fit1, agreeSub)
comb_accuracy = sum(pred_comb == agreeSub$y) / length(pred_comb)
comb_accuracy



comb1 <- train(y~., data = predDF, method = "gbm")
comb1
confusionMatrix(pred1, pred2)

#Quizz4 Q2 =====================================================

library(caret)
# library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]


set.seed(62433)


fit1 <- train(diagnosis~., data = training, method = "rf", trControl = trainControl(number = 3, verboseIter = T))
fit2 <- train(diagnosis~., data = training, method = "gbm", trControl = trainControl(number = 3, verboseIter = T))
fit3 <- train(diagnosis~., data = training, method = "lda", trControl = trainControl(number = 3, verboseIter = T))

pred1 <- predict(fit1, training)
pred2 <- predict(fit2, training)
pred3 <- predict(fit3, training)

# confusionMatrix(pred1, training$diagnosis)$overall[1]
# confusionMatrix(pred2, testing$diagnosis)$overall[1]
# confusionMatrix(pred3, testing$diagnosis)$overall[1]

combDF <- data.frame(pred1, pred2, pred3, diagnosis = training$diagnosis)

combFit <- train(diagnosis~., data = combDF, method = "rf", trControl = trainControl(number = 3, verboseIter = T))
combFit

test_pred1 <- predict(fit1, testing)
test_pred2 <- predict(fit2, testing)
test_pred3 <- predict(fit3, testing)
test_combDF <- data.frame(pred1=test_pred1, pred2=test_pred2, pred3=test_pred3, diagnosis = testing$diagnosis)


predComb_test <- predict(combFit, test_combDF)
length(predComb_test)
confusionMatrix(predComb_test, test_combDF$diagnosis)$overall[1]


confusionMatrix(test_pred1, testing$diagnosis)$overall[1]
confusionMatrix(test_pred2, testing$diagnosis)$overall[1]
confusionMatrix(test_pred3, testing$diagnosis)$overall[1]


# WARN: is there overfitting??? may be we need validation set



#Quizz4 Q3 =====================================================

set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(233)

fit1 <- train(CompressiveStrength~., data = training, method = "lasso")
fit1$finalModel
plot.enet(fit1$finalModel, xvar='penalty')
plot.enet(fit1$finalModel, xvar='fraction')
plot.enet(fit1$finalModel, xvar='step')
plot.enet(fit1$finalModel, xvar='L1norm')


#Quizz4 Q4 =====================================================
library(lubridate)  # For year() function below

download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv", destfile = "gaData.csv")

dat <- read.csv("gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
tstest = ts(testing$visitsTumblr, start=366)


library(forecast)
?bats
fit <- bats(tstrain)
plot(forecast(fit, level = 95))
fc <- forecast(fit, h=235, level = 95)
plot(fc)
lines(tstest,col="red")

inConfint <- tstest[(tstest>=fc$lower & tstest<=fc$upper)]

length(inConfint)
length(tstest)

#answer
length(inConfint)/length(tstest)


#Quizz4 Q5 =====================================================
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(325)

str(training)

fit1 <- train(CompressiveStrength~., data=training, method = "svmLinear2")
fit2 <- svm(CompressiveStrength~., data=training)
fit2

pred <- predict(fit2, testing)
# Function that returns Root Mean Squared Error
rmse <- function(error){
    sqrt(mean(error^2))
}

error <- pred - testing$CompressiveStrength
error

rmse(error)
