

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



#Quizz5 Q3 =====================================================

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
