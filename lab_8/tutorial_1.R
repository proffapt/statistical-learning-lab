library(e1071)
library(MLmetrics)


set.seed(1)
x=matrix(rnorm(20*2),ncol=2)
y=c(rep(-1,10),rep(1,10))
x[y == 1, ] =x[y == 1, ] + 1 
x
y
plot(x, col = (3 - y))
dat=data.frame(x=x , y=as.factor(y))
dat
##Test Data Generation
xtest = matrix(rnorm(20*2), ncol = 2)
ytest = sample(c(-1, 1), 20, rep = TRUE)
xtest
ytest
xtest[ytest == 1, ] = xtest[ytest == 1, ] + 1
testdat = data.frame(x = xtest, y = as.factor(ytest))
testdat

#Model Building
svmfit1 = svm(y~. ,  data = dat, kernel = "linear", cost = 10, scale = FALSE)
plot(svmfit1, dat)
summary(svmfit1)

# Support Vectors
svmfit1$index

#Confusion Matrix
ypred1 = predict(svmfit1, testdat)
ypred1
cm<-table(predict = ypred1, truth = testdat$y)
cm
TP <-cm[1,1]
TP
FP <- cm[2,1]
FP
TN<-cm[2,2]
TN
FN <- cm[1,2]
FN

acc<-(TP+TN)/(TP+FP+TN+FN)
acc

#recall
sens<-TP/(TP+FN)
sens

#precission
prec<-TP/(TP+FP)
prec

f1<- 2*prec*sens/(prec+sens)
f1

#Metics
ConfusionMatrix(ypred1,testdat$y)
Accuracy(ypred1,testdat$y)
Precision(ypred1,testdat$y)
Recall(ypred1,testdat$y)
F1_Score(ypred1,testdat$y)

#radial
#Model Building
svmfit1 = svm(y~. ,  data = dat, kernel = "radial", cost = 10, scale = FALSE)
plot(svmfit1, dat)
summary(svmfit1)

# Support Vectors
svmfit1$index

#Confusion Matrix
ypred1 = predict(svmfit1, testdat)
ypred1

#Metics
ConfusionMatrix(ypred1,testdat$y)
Accuracy(ypred1,testdat$y)
Precision(ypred1,testdat$y)
Recall(ypred1,testdat$y)
F1_Score(ypred1,testdat$y)

#Model Building
svmfit2 = svm(y~. ,  data = dat, kernel = "linear", cost = 0.1, scale = FALSE)
plot(svmfit2, dat)
summary(svmfit2)

# Support Vectors
svmfit2$index

#Confusion Matrix
ypred2 = predict(svmfit2, testdat)
ypred2

#Metics
ConfusionMatrix(ypred2,testdat$y)
Accuracy(ypred2,testdat$y)
Precision(ypred2,testdat$y)
Recall(ypred2,testdat$y)
F1_Score(ypred2,testdat$y)