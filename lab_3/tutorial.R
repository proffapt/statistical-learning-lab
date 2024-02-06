library(ISLR2)
library(ggplot2)
library(car)
library(stats)
library(dplyr)
library(MASS)
library(boot)

head(Default)
dim(Default)

sample(100, 5)

# Randomly divide in training and test data
trn <- sample(dim(Default)[1], 8000)
trn
Default_train <- Default[trn,]
Default_test <- Default[-trn,]
Default_test <- Default_test[, -1]
head(Default_test)

dim(Default_train)
dim(Default_test)

glm.logit <- glm(default ~balance + income, data = Default_train, family = binomial(link= "logit"))
summary(glm.logit)

glm.probit <- glm(default ~balance + income, data = Default_train, family = binomial(link= "probit"))
summary(glm.probit)

glm.logit1 <- glm(default ~balance, data = Default_train, family = binomial(link= "logit"))
summary(glm.logit1)

l1 <- logLik(glm.logit)
l1[1]
l0 <- logLik(glm.logit1)
l0[1]

glm.logit2 <- glm(default ~balance + income + student, data = Default_train, family = binomial(link= "logit"))
summary(glm.logit2)

glm.logit3 <- glm(default ~balance + student, data = Default_train, family = binomial(link= "logit"))
summary(glm.logit3)

# Classify using test data
pred <- predict(glm.logit3, Default_test, type="response")
pred[1:5]

# Classify the prediction in default = "Yes" or "No"
pred_class <- ifelse(pred >=0.5, "Yes", "No")
pred_class[1:5]

# Create confusion matrix
table(Obs = Default[-trn,]$default, Pred = pred_class)

# Test Accuracy
mean(pred_class == Default[-trn,]$default)

contrasts(Default$default)

dev <- 2*(l1[1]-l0[1])
dev

qchisq(0.95, 1)

# Fit logistic regression model
# glm.fit <- glm(default ~ student + balance, data = )