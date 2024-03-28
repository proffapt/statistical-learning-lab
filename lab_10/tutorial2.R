## REGRESSION
library("MASS")

# Boston
data(Boston)
dataset <- as.data.frame(Boston)
dataset

# Splitting of data
sample <- sample(c(TRUE, FALSE), nrow(Heartdata),replace = TRUE,prob = c(0.7,0.3))
train <- dataset[sample, ]
test <- dataset[!sample, ]
train
test

# Training the model
tree.boston <- rpart(medv~., data=train, method)
summary(tree.boston)
tree.boston
rpart.plot(tree.boston)

# Metrics
ypred1 <- predict(opttree.boston, test)
ypred1
MAE(ypred1, test$medv)
MSE(ypred1, test$medv)
RMSE(ypred1, test$medv)
MAPE(ypred1, test$medv)

