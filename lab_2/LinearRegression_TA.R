library(MASS)
library(ggplot2)
library(car)
library(ISLR)
library(stats)
library(dplyr)

data(Boston)
summary(Boston)
head(Boston)


# Univariate Normality using QQ plot

qqnorm(Boston$lstat)
qqline(Boston$lstat)

# Correlation Matrix
cor(Boston)


# Linear Regression
plot(Boston$lstat,Boston$medv)
model <- lm(medv~lstat, data=Boston)
abline(model,col="red")
summary(model)

# Multiple linear regression model
model1<-lm(medv~.,data=Boston)
summary(model1)

anova(model1)
#variance influence factor =1/(R^2)
vif(model1)

model2<-lm(medv~. -rad-tax,data=Boston)
summary(model2)

anova(model2)
vif(model2)

# Confidence interval
confint(model2,level=0.95)

# Prediction Interval
model3 <- lm(medv~lstat,data=Boston)
new_pt <- data.frame(lstat=c(4,5,6))
predict(model3,newdata=new_pt)

predict(model3,newdata=new_pt,interval="predict",level=0.95)


# Residual 
residuals(model3)
fitted(model3)
deviance(model3)

# Normal probability plot for residuals
lm.std <- rstandard(model3)
qqnorm(lm.stdylab="Standardzed ")

#Normal probability plot for residuals
lm.std <- rstandard(model3)
qqnorm(lm.std,ylab="Standardized Residuals ", xlab="Normal Scores", main="NOrmal Probabilty plot of residuals" )
qqline(lm.std)

#Backward stepwise Regression

model4<- lm(medv~.,data=Boston)
summary(model4)
model41 <- stepAIC(model,direction="backward", trace=FALSE)
summary(41)

#Cook's Distance
model5<-lm(medv~.,data=Boston)
dist<- cooks.distance(model5)
cooks.distance(model5)[which.max(cooks.distance(model5))]