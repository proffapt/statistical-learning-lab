library("rpart")
library("rpart.plot")

# Heart
Heartdata <- as.data.frame(heart)
Heartdata

# Splitting of data
sample <- sample(c(TRUE, FALSE), nrow(Heartdata),replace = TRUE,prob = c(0.7,0.3))
train <- Heartdata[sample, ]
test <- Heartdata[!sample, ]
train
test

# Training the model
tree.heart <- rpart(y-sbp+tobacco+ldl+adiposity+factor(famhist)+typea+)
summary(tree.heart)
rpart.plot(tree.heart)

# Prunning
plotcp(tree.heart)
tree.heart$cptable
index<-which.min(tree.heart$cptable[, "xerror"])
cpopt<-tree.heart$cptable[index, "CP"]
cpopt
opttree.heart<-prune(tree.heart, cp=cpopt)
rpart.plot(opttree.heart)

# Confusion Matrix
ypred <- predict(opttree.heart, test, type='class')
ypred
table(predict=ypred, truth=test$y)

# Metrics
ConfusionMatrix(ypred, test$y)

