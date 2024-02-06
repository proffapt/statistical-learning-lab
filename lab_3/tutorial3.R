library(MASS)
library(ggplot2)
library(car)
library(ISLR)
library(stats)
library(dplyr)

# Load the airquality
data("airquality")
View(airquality)
dim(airquality)
head(airquality)
tail(airquality)
colnames(airquality)
colSums(is.na(airquality))
str(airquality)
summary(airquality)

# Handling the missing values
library(mice)
# Create an imputation object using the mice function with predictive mean matching method
impute_object <- mice(airquality, method = c("pmm", "pmm","pmm","pmm","pmm","pmm"), m = 1)

# Replace missing values in the original dataset with imputed values
imputed_data <- complete(impute_object, action = "long")
airquality$Solar.R <- imputed_data$Solar.R
airquality$Ozone <- imputed_data$Ozone

# Check for missing values after imputation
colSums(is.na(airquality))

qqnorm(airquality$Ozone)
qqline(airquality$Ozone)
cor(airquality)

aq1 <- airquality %>% select(Ozone,Solar.R, Wind, Temp, Month, Day)
pairs(aq1)
aq1 <- cor(aq1)
heatmap(cor(aq1))

library(corrplot)
corrplot(aq1)


plot(airquality$Solar.R, airquality$Ozone)
model <- lm(Ozone ~ Solar.R, data = airquality)
abline(model, col = "red")
summary(model)

model1 <- lm(Ozone ~ ., data = airquality)
summary(model1)
anova(model1)

vif(model1)

model2 <- lm(Ozone ~ . - Wind, data = airquality)
summary(model2)
anova(model2)
vif(model2)
confint(model2, level = 0.95)

model3 <- lm(Ozone ~ Solar.R, data = airquality)
new_pt <- data.frame(Solar.R = c(40, 50, 60))
predict(model3, newdata = new_pt)
predict(model3, newdata = new_pt, interval = "predict", level = 0.95)
residuals(model3)
fitted(model3)
deviance(model3)
lm.std <- rstandard(model3)
qqnorm(lm.std, ylab = "Standardized Residuals ", xlab = "Normal Scores", main = "Normal Probability Plot of Residuals")
qqline(lm.std)

model4 <- lm(Ozone ~ ., data = airquality)
summary(model4)
model41 <- stepAIC(model4, direction = "backward", trace = FALSE)
summary(model41)

model5 <- lm(Ozone ~ ., data = airquality)
dist <- cooks.distance(model5)
cooks.distance(model5)[which.max(cooks.distance(model5))]

# Influential Data Points
influential <- dist[(dist > 3 * mean(dist))]
names_of_influential <- names(influential)
names_of_influential
length(names_of_influential)

# Outliers (Influential Data Points)
outliers <- airquality[names_of_influential, ]
outliers

data_wo_out <- airquality %>% anti_join(outliers)
data_wo_out

# Model without outliers
model6 <- lm(Ozone ~ ., data = data_wo_out)
summary(model6)

