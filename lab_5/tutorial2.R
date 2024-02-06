library(ISLR)
library(caTools)
library(dplyr)
library(class)
library(MASS)

head(Default)
View(Default)

Default$student01 <- ifelse(Default$student == "YES", 1, 0)
head(Default)

# Convert all variables into numeric
# Unlice many of our previous methods, knn() requires that
Default$student = as.numeric(Default$student)
head(Default)

# Splitting data into train and test
set.seed(10000)
split <-sample.split(Default, SplitRatio = 0.8)
train_cl <- subset(Default, split == "TRUE")
head(train_cl)
test_cl <- subset(Default, split == "FALSE")
head(test_cl)

train_cl$balance <- scale(train_cl$balance)
test_cl$balance <- scale(test_cl$balance)

train_cl$income <- scale(train_cl$income)
test_cl$income <- scale(test_cl$income)

# Feature Scaling
train_scale <- scale(train_cl[, 2:4])
head(train_scale)
test_scale <- scale(test_cl[, 2:4])
head(test_scale)

# ALITER: If you want to remove default column from dataset then
train_scale = scale(train_scale[-1])
head(train_scale)
test_scale = scale(test_scale[-1])
head(test_scale)

classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$default,
                      k = 1)
classifier_knn

# Confusion Matrix