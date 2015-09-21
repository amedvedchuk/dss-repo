
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


#===============================================================
#===============================================================

#Quizz3 Q1 ====================================================
data(mtcars)
summary(lm(mpg~factor(cyl)+wt, mtcars))
# ans factor(cyl)8  -6.0709 

#Quizz3 Q2 ====================================================
summary(lm(mpg~factor(cyl)+wt, mtcars))
summary(lm(mpg~factor(cyl), mtcars))

#Quizz3 Q3 ====================================================
lm32_1 <- lm(mpg~factor(cyl)+wt, mtcars)
lm32_2 <- lm(mpg~factor(cyl)+wt+factor(cyl)*wt, mtcars)
# lm32_2 <- lm(mpg~factor(cyl)+wt+cyl*wt, mtcars)
summary(lm32_1)
summary(lm32_2)
anova(lm32_1,lm32_2)
#ans: The P-value is larger than 0.05. So, according to our criterion, we would fail to reject, 
#    which suggests that the interaction terms may not be necessary. 

#Quizz3 Q4 ====================================================
lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
lm(mpg ~ factor(cyl) + I(wt * 0.5), data = mtcars)
# ans: The estimated expected change in MPG per  ton increase in weight for 
    #a specific number of cylinders (4, 6, 8). 

#Quizz3 Q5 ====================================================

x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
lm35 <- lm(y~x)
hatvalues(lm35)
?hatvalues
# ans - 0.9945734


#Quizz3 Q6 ====================================================
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
lm36 <- lm(y~x)
hatvalues(lm36)
?dfbeta
influence.measures(lm36)


#===============================================================
#===============================================================

#Quizz4 Q1 ====================================================

library(MASS)
?shuttle
shuttle$useNum[shuttle$use == 'auto'] <- 1
shuttle$useNum[shuttle$use != 'auto'] <- 0
tail(shuttle, 100)

logShut <- glm(useNum~wind, shuttle, family = "binomial")
logShut
summary(logShut)
exp(logShut$coef)
1/exp(logShut$coef)[2]

#Quizz4 Q2 ====================================================
logShut2 <- glm(useNum~wind+magn, shuttle, family = "binomial")
logShut2
summary(logShut2)
exp(logShut2$coef)
1/exp(logShut2$coef)[2]

#Quizz4 Q3 ====================================================

glm(useNum~wind, shuttle, family = "binomial")
glm(1-useNum~wind, shuttle, family = "binomial")

#Quizz4 Q4 ====================================================

data(InsectSprays)
str(InsectSprays)

poisStray <- glm(count~spray, InsectSprays, family = 'poisson')
poisStray
summary(poisStray)
exp(poisStray$coef)
1/exp(poisStray$coef)[2]


#Quizz4 Q5 ====================================================
glm(count~spray, InsectSprays, family = 'poisson', offset=log(seq_along(InsectSprays[,1])))$coef
glm(count~spray, InsectSprays, family = 'poisson', offset=(log(10) + log(seq_along(InsectSprays[,1]))))$coef

seq_along(InsectSprays[,1])
0.802 - log(10)

#Quizz4 Q5 ====================================================

x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
plot(x,y)
knots = c(0);
splineTerms <- sapply(knots, function(knot) (x > knot)*(x-knot) )
xMat <- cbind(1, x, splineTerms)
yhat <- predict(lm(y ~ xMat))
plot(x, y, frame = FALSE, pch = 21, bg = "lightblue", cex = 2)
lines(x, yhat, col = "red", lwd = 2)

lm(y ~ xMat)

http://rstudio-pubs-static.s3.amazonaws.com/31791_aed7e217745248f2b174e26a3e774c8c.html

