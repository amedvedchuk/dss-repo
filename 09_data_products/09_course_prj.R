

data(iris)
str(iris)

library(caret)

index <- createDataPartition(iris$Species, p=0.7, list = F)
training <- iris[index,]
testing <- iris[-index,]



# fit <- train(Species ~ Petal.Width + Petal.Length + Sepal.Length, method = "rpart", data = training, tuneGrid = data.frame(cp=0.1))
fit <- train(Species ~ .,
             method = "rpart2", data = iris, tuneGrid = data.frame(maxdepth=2))

fit <- train(Species ~ Petal.Width + Petal.Length,
             method = "rpart2", data = iris, tuneGrid = data.frame(maxdepth=2))


fit
fit$finalModel
varImp(fit)

pred <- predict(fit, testing)
confusionMatrix(pred, testing$Species)
summary(iris)

plot(Sepal.Width ~ Species, data = iris)

predict(fit, data.frame(Petal.Width = 1.6, Petal.Length=1.6, Sepal.Length = 5))

 g <- ggplot(data = iris, aes(Species, Petal.Width)) + geom_boxplot()
g <- g + geom_hline(y = 1, color="red")
g
