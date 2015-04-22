
set.seed(12345)

dataMatrix <- matrix(rnorm(400), nrow=40)

set.seed(678910)

for (i in 1:40){
    coinFlip <- rbinom(1, size = 1, prob = 0.5)
    if(coinFlip){
        dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0,3), each = 5)
    }
}

# par(mfrow = c(1,2))

image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1])
# heatmap(dataMatrix, )

hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order, ]
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(rowMeans(dataMatrixOrdered), 40:1, , xlab = "Row Mean", ylab = "Row", pch = 19)
plot(colMeans(dataMatrixOrdered), xlab = "Column", ylab = "Column Mean", pch = 19)

scaledDataOrdered <- scale(dataMatrixOrdered)

svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(svd1$u[, 1], 40:1, xlab = "Row", ylab = "First left singular vector", pch = 19)
plot(svd1$v[, 1], xlab = "Column", ylab = "First right singular vector", pch = 19)

#---- try to see all left and right vercotors vecotros -----
par(mfrow = c(2, 5))
#ll<-lapply(seq(1,10), function(i) qplot(svd1$u[, i],40:1))
lapply(seq_along(svd1$u[1,]), function(i) plot(svd1$u[, i], 40:1, main = paste("U_col", i), 
                                               xlab = "Value", ylab = "Row", pch = 19))
lapply(seq_along(svd1$v[1,]), function(i) plot(svd1$v[, i], main = paste("V_col", i), 
                                               xlab = "Column", ylab = "Value", pch = 19))
# --------------------------------------

plot(svd1$d, ylab = "SVD_D", xlab = "cols", pch = 19)

#library(plyr)
#library(ggplot2)

#Components of the SVD - Variance explained
par(mfrow = c(1, 2))
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Prop. of variance explained",
     pch = 19)

#Relationship to principal components Relationship to principal components
svd1 <- svd(scale(dataMatrixOrdered))
pca1 <- prcomp(dataMatrixOrdered, scale = TRUE)
plot(pca1$rotation[, 1], svd1$v[, 1], pch = 19, xlab = "Principal Component 1",
     ylab = "Right Singular Vector 1")
abline(c(0, 1))

#Components of the SVD - variance explained Components
constantMatrix <- dataMatrixOrdered*0
for(i in 1:dim(dataMatrixOrdered)[1]){
  constantMatrix[i,] <- rep(c(0,1),each=5)
}
svd1 <- svd(constantMatrix)
par(mfrow=c(1,3))
image(t(constantMatrix)[,nrow(constantMatrix):1])
plot(svd1$d,xlab="Column",ylab="Singular value",pch=19)
plot(svd1$d^2/sum(svd1$d^2),xlab="Column",ylab="Prop. of variance explained",pch=19)

#What if we add a second pattern?
set.seed(678910)
for (i in 1:40) {
  # flip a coin
  coinFlip1 <- rbinom(1, size = 1, prob = 0.5)
  coinFlip2 <- rbinom(1, size = 1, prob = 0.5)
  # if coin is heads add a common pattern to that row
  if (coinFlip1) {
    dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0, 5), each = 5)
  }
  if (coinFlip2) {
    dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0, 5), 5)
  }
}
hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order, ]
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
heatmap(dataMatrixOrdered)



#Singular value decomposition - true patterns Singular value decomposition - true patterns
svd2 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(rep(c(0, 1), each = 5), pch = 19, xlab = "Column", ylab = "Pattern 1")
plot(rep(c(0, 1), 5), pch = 19, xlab = "Column", ylab = "Pattern 2")

#v and patterns of variance in rows and patterns of variance in rows
svd2 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(svd2$v[, 1], pch = 19, xlab = "Column", ylab = "First right singular vector")
plot(svd2$v[, 2], pch = 19, xlab = "Column", ylab = "Second right singular vector")

#--------- print all vectors:
par(mfrow = c(2, 5))
lapply(seq_along(svd2$v[1,]), function(i) plot(svd2$v[, i], main = paste("V_col", i), 
                                               xlab = "Column", ylab = "Value", pch = 19))
#----------------------------

#d and variance explained and variance explained
svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 2))
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Percent of variance explained",
     pch = 19)