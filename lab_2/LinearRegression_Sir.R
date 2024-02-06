x <- rnorm(100, 5, 1)
y <- 100 + 2*x + rnorm(100, 2, 0.5)

model = lm(y ~ x)
summary(model)
anova(model)

plot(x, y)
abline(model, col = "red")

residuals_v <- residuals(model)
qqnorm(residuals_v)

yhat <- predict(model)
manual_residuals_v <- y-yhat
plot(yhat, manual_residuals_v, xlab="Predicted Y", ylab="Residuals")

xnew <- data.frame(x = rnorm(25, 5, 1)) # variable name should be same as before
ynew <- predict(model, newdata = xnew)

data(mtcars)
head(mtcars)
library(dplyr)
data <- mtcars %>% select(mpg, disp,hp, drat, wt, qsec)
pairs(data)
cor_data <- cor(data)
heatmap(cor_data)
library(corrplot)
corrplot(cor_data)

model_2 <- lm(mpg~., data = data)
summary(model_2)
library(car)
vif(model_2)

model_3 <- lm(mpg~ hp + drat + wt + qsec, data = data)
summary(model_3)
vif(model_3)

model_4 <- lm(mpg~ .-drat - hp, data = data)
summary(model_4)
vif(model_4)

anova(model_4)