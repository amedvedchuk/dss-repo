

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


