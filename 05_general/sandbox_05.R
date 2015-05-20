
library(kernlab)
data(spam)

# Perform the subsampling
set.seed(3435)
trainIndicator = rbinom(4601, size = 1, prob = 0.5)
table(trainIndicator)

trainSpam = spam[trainIndicator == 1, ]
testSpam = spam[trainIndicator == 0, ]

plot(trainSpam$capitalAve ~ trainSpam$type)

plot(log10(trainSpam$capitalAve + 1) ~ trainSpam$type)

#Relationships between predictors
plot(log10(trainSpam[, 1:4] + 1))

#Clustering Clustering
hCluster = hclust(dist(t(trainSpam[, 1:57])))
plot(hCluster)

#New clustering
hClusterUpdated = hclust(dist(t(log10(trainSpam[, 1:55] + 1))))
plot(hClusterUpdated)

################ Statistical prediction/modeling Statistical prediction/modeling
trainSpam$numType = as.numeric(trainSpam$type) - 1
costFunction = function(x, y) sum(x != (y > 0.5))
cvError = rep(NA, 55)
library(boot)

##########
#lmFormula = reformulate(names(trainSpam)[1], response = "numType")
#glmFit = glm(lmFormula, family = "binomial", data = trainSpam)
#cvError[1] = cv.glm(trainSpam, glmFit, costFunction, 2)$delta[2]
##########

for (i in 1:55) {
  lmFormula = reformulate(names(trainSpam)[i], response = "numType")
  glmFit = glm(lmFormula, family = "binomial", data = trainSpam)
  cvError[i] = cv.glm(trainSpam, glmFit, costFunction, 2)$delta[2]
}
## Which predictor has minimum cross-validated error?
names(trainSpam)[which.min(cvError)]

################ Get a measure of uncertainty Get a measure of uncertainty
## Use the best model from the group
predictionModel = glm(numType ~ charDollar, family = "binomial", data = trainSpam)
## Get predictions on the test set
predictionTest = predict(predictionModel, testSpam)
predictedSpam = rep("nonspam", dim(testSpam)[1])
## Classify as `spam' for those with prob > 0.5
predictedSpam[predictionModel$fitted > 0.5] = "spam"


############ Get a measure of uncertainty Get a measure of uncertainty
## Classification table
table(predictedSpam, testSpam$type)

## Error rate
(61 + 458)/(1346 + 458 + 61 + 449)


########################  FROM ASSIGNEMENT 1 #######################
activity <- read.csv(file = "data/activity.csv")
meanStepsByInterval <- aggregate(steps ~ interval, activity, FUN = mean)

convert interval variable to time for interpreting x axis as time scale
meanStepsByInterval$time <-  strptime(sprintf("%04d",meanStepsByInterval$interval), format = "%H%M")

plot(x=meanStepsByInterval$time, y=meanStepsByInterval$steps, type="l",
     main = "Average number of steps by 5-minute interval",
     xlab = "time of day", 
     ylab="average steps count")

# g <- ggplot(data = meanStepsByInterval, aes(x = interval, y = steps))
# p <- g + geom_line() +
#     labs(title = "Average number of steps by 5-minute interval") + 
#     xlab("time interval")
# p

# set locale to English to print weekday names in English.
Sys.setlocale("LC_ALL","English")


##################################################################
