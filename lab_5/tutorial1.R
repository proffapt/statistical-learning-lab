library(ISLR)
library(ggplot2)
library(boot)

set.seed(2)

# Randomly sample 196 obs out of 392 obs
train = sample(392, 196)
train[1:5]
head(Auto)
dim(Auto)

Auto.tr <- Auto[train,]
dim(Auto.tr)
head(Auto.tr)
View(Auto)
?Auto

# Fit Linear Model on training data
lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)
summary(lm.fit)

# Calculate MSE for validation set
attach(Auto)
mean((mpg - predict(lm.fit, Auto))[-train] ^ 2)
## Same thing but step-by-step
Auto.test <- Auto[-train,]
dim(Auto.test)
Auto.new <- data.frame(horsepower = Auto.test[,4])
dim(Auto.new)
mpg_pred <- predict(lm.fit, newdata = Auto.new)
mse <- mean((Auto.test$mpg - mpg_pred)^2)
mse

# We can check the residuals
mpg - predict(lm.fit, Auto)[-train]

lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
summary(lm.fit2)
mean((mpg - predict(lm.fit2, Auto))[-train] ^ 2)

lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
summary(lm.fit3)
mean((mpg - predict(lm.fit3, Auto))[-train] ^ 2)

# GLM with no family argument is same as lm
glm.fit <- glm(mpg ~ horsepower, data = Auto)
summary(glm.fit)
cv.err <- cv.glm(Auto, glm.fit)
cv.err$delta

# Fit Linear Models of order 1-5
cv.error = rep(0,5)
cv.error
for(i in 1:5) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[2]
}
cv.error
plot(seq(1,5), cv.error, xlab = "Order", ylab = "CV Error", type = "o")
boxplot(mpg ~ factor(cylinders))

# K-Fold Validation
for(i in 1:5) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit, K = 5)$delta[2]
}
cv.error
plot(seq(1,5), cv.error, xlab = "Order", ylab = "CV Error", type = "o")

# K fold CV
set.seed(17)
cv.error10 = rep(0,10)
cv.error10
for(i in 1:10) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error10[i] <- cv.glm(Auto, glm.fit, K = 5)$delta[2]
}
cv.error10
plot(seq(1,10), cv.error10, xlab = "Order", ylab = "CV Error", type = "o")
dim(Portfolio)

# Bootstrapping
alpha.fn = function(data, index){
  X = data$X[index]
  Y = data$Y[index]
  return ((var(Y) - cov(X,Y))/(var(X)+var(Y) - 2*cov(X,Y)))
}
alpha.fn(Portfolio, 1:100)
boot(Portfolio ,alpha.fn,R=1000)  

head(Portfolio)
dim(Portfolio)

boot.fn = function(data, index){
  lm.fit <- lm(mpg ~ horsepower, data = data)
  return (coef(lm.fit))
}
boot.fn(Auto, 1:196)
boot(Auto, boot.fn, R = 1000)

boot.fn = function(data, index){
  lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = index)
  return (coef(lm.fit))
}
boot.fn(Auto, 1:196)
boot(Auto, boot.fn, R = 1000)

# Assignment
# library(MASS)
# Boston Dataset

# Fit "medv" with number of rooms "rm",
# Train 75%, validation 25%
# for linear, poly 2-5
# Compare MSE with LOOVC, K-fold
# for k = 5
# Bootstrap with index [1:400] for R=100
# and determine parametric uncertainity