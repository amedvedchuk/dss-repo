
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


