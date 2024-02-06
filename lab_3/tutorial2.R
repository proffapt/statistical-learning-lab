library("catdata")
library(ggplot2)

data(heart)
head(heart)
summary(heart)

heart
?heart

heart_data <- as.data.frame(heart)
ggplot(heart_data, aes(y=tobacco, x=factor(y), fill =factor(y))) + geom_boxplot()

head(heart_data)
dim(heart_data)

# Randomly divide in training and test data
trn <- sample(dim(heart_data)[1],369)
trn

heart_train <- heart_data[trn,]
heart_test <- heart_data[-trn,]
heart_test <- heart_test[,-1]

head(heart_test)

dim(heart_train)
dim(heart_test)

glm.logit <- glm(y ~ sbp + tobacco, data = heart_train, family = binomial(link = "logit"))
summary(glm.logit)

glm.logit1 <- glm(y ~ sbp, data = heart_train, family = binomial(link = "logit"))
summary(glm.logit1)

glm.logit2 <- glm(y ~ sbp + tobacco + ldl + adiposity + famhist + typea + obesity + alcohol + age, data = heart_train, family = binomial(link = "logit"))
summary(glm.logit3)

glm.logit3 <- glm(y ~ tobacco + ldl + famhist + typea + age, data = heart_train, family = binomial(link = "logit"))
summary(glm.logit4)

# Predict using test data
pred <- predict(glm.logit3,heart_test, type="response")
pred[1:5]

#lassify the prediction as yes/no

pred_class <- ifelse(pred >= 0.5, 1, 0)
pred_class[1:5]

cm <-table(Obs = heart_data[-trn,]$y,pred = pred_class)
cm

TP<- cm[2,2]
TP
FP <- cm[1,2]
FP
TN <- cm[1,1]
FN <- cm[2,1]


sens <- TP/(TP+FN)
spec <- TN/(TN + FP)
sens
spec

f1 <- 2*TP/(2*TP+FP+FN)
f1

table(heart_train$y)