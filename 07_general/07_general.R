
#Quizz1 Q2 =====================================================

x <- c(0.18, -1.54, 0.42, 0.95)
w <- c(2, 1, 3, 1)
weighted.mean(x,w)

#Quizz1 Q2 =====================================================

x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

lm(y~x-1, data=data.frame(x,y))

g <- ggplot(data.frame(x,y), aes(x = x, y = y))
g = g + geom_point(size = 6, colour = "black", alpha = 0.5)
g <- g + geom_smooth(method="lm", formula=y~x)
g

#Quizz1 Q2 =====================================================

library(mtcars)
lm(mpg~wt, mtcars)

# plot for ilustrate
g <- ggplot(mtcars, aes(x = wt, y = mpg))
g = g + geom_point(size = 6, colour = "black", alpha = 0.5)
g <- g + geom_smooth(method="lm", formula=y~x)
g

#Quizz1 Q6 =====================================================
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
xn <- (x-mean(x))/sd(x)
xn[1]




#Quizz1 Q7 =====================================================
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)

lm(y~x, data=data.frame(x,y))

g <- ggplot(data.frame(x,y), aes(x = x, y = y))
g = g + geom_point(size = 6, colour = "black", alpha = 0.5)
g <- g + geom_smooth(method="lm", formula=y~x)
g

#Quizz1 Q9 =====================================================
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
mean(x)



# ======================================================================== 
# ========================================================================


#Quizz2 Q1 =====================================================

x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)

fit <- lm(y~x)
summary(fit)
str(summary(fit))
summary(fit)$coefficients[2,4]


#Quizz2 Q2 =====================================================
summary(fit)$sigma
# or
sqrt((1/(9-2))*sum(fit$residuals^2))


#Quizz2 Q3 =====================================================
data(mtcars)
str(mtcars)

fit <- lm(mpg ~ wt, mtcars)

# WRONG ANSWER!!! :
#confint(fit)
#confint(fit)[2,1]
## OR
#summary(fit)$coeff[2,1] + c(-1, 1)*qt(0.975, df=fit$df)*summary(fit)$coeff[2,2]
# because of conf interval for mpg is required!
predict(fit, newdata = data.frame(wt=mean(mtcars$wt)), interval="confidence")



#Quizz2 Q5 ====================================================

predict(fit, newdata = data.frame(wt=3), interval="prediction")

#Quizz2 Q6 ====================================================

confint(lm(mpg ~I(wt/2), mtcars))

#Quizz2 Q8 ====================================================
lm(mpg ~I(wt+0), mtcars)
lm(mpg ~I(wt+1), mtcars)



#Quizz2 Q9 ====================================================
fitsl <- lm(mpg~wt, mtcars)
fititc <- lm(mpg~1, mtcars) # intercept only
mean(mtcars$mpg) # for intercept onle - there is just mean fro MPG

sum(resid(fitsl)^2) / sum(resid(fititc)^2)
1
#Quizz2 Q10 ====================================================
sum(resid(lm(mpg~wt, mtcars)))
sum(resid(lm(mpg~wt-1, mtcars)))

