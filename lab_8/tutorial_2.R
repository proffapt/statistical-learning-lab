library(e1071)
dataset <- as.data.frame(iris)
dataset
head(iris)
attach(iris)
library(caTools)
split <- sample.split(dataset$Species, SplitRatio = 0.75)

trainset <- subset(dataset, split == TRUE)
testset <- subset(dataset, split == FALSE)
trainset
testset
summary(trainset)
summary(testset)
dim(trainset)
dim(testset)
svmfit1 <- svm(Species~., data= trainset, kernel = "linear", cost = 10, scale = FALSE)
summary(svmfit1)
#Plotting the model
plot(svmfit1, data = trainset, Petal.Width~Petal.Length)

#Confusion Matrix
ypred1 <- predict(svmfit1, testset)
table(predict = ypred1, truth = testset$Species)

#Metrics
ConfusionMatrix(ypred1, testset$Species) #just a transpose of the matrix we obtained
Accuracy(ypred1, testset$Species)
Precision(ypred1, testset$Species)
Recall(ypred1, testset$Species)
F1_Score(ypred1, testset$Species)

#For radial kernel

svmfit2 <- svm(Species~., data= trainset, kernel = "radial", cost = 10, scale = FALSE)
summary(svmfit2)
#Plotting the model
plot(svmfit2, data = trainset, Petal.Width~Petal.Length)

#Confusion Matrix
ypred2 <- predict(svmfit2, testset)
table(predict = ypred2, truth = testset$Species)

#Metrics
ConfusionMatrix(ypred2, testset$Species) #just a transpose of the matrix we obtained
Accuracy(ypred2, testset$Species)
Precision(ypred2, testset$Species)
Recall(ypred2, testset$Species)
F1_Score(ypred2, testset$Species)


#For polynomial kernel

svmfit3 <- svm(Species~., data= trainset, kernel = "polynomial", cost = 10, scale = FALSE)
summary(svmfit3)
#Plotting the model
plot(svmfit3, data = trainset, Petal.Width~Petal.Length)

#Confusion Matrix
ypred3 <- predict(svmfit3, testset)
table(predict = ypred3, truth = testset$Species)

#Metrics
ConfusionMatrix(ypred3, testset$Species) #just a transpose of the matrix we obtained
Accuracy(ypred3, testset$Species)
Precision(ypred3, testset$Species)
Recall(ypred3, testset$Species)
F1_Score(ypred3, testset$Species)