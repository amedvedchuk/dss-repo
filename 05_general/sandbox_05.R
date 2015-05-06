
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
