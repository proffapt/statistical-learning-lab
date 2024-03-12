#Handling the data
# Load the mtcars dataset
data("mtcars")
# View the mtcars dataset
View(mtcars)
# Dimensions of the mtcars dataset
dim(mtcars)
# Display the first few rows of the mtcars dataset
head(mtcars)
# Display the last few rows of the mtcars dataset
tail(mtcars)
# Column names of the mtcars dataset
colnames(mtcars)
# Number of Missing Values in each column of the mtcars dataset
colSums(is.na(mtcars))
# Structure of the mtcars dataset
str(mtcars)
# Summary statistics of the mtcars dataset
summary(mtcars)
# Summary statistics of the "mpg" column in the mtcars dataset
summary(mtcars$mpg)
# Summary statistics of the "wt" column in the mtcars dataset
summary(mtcars$wt)


#make a plot of LungCap vs Height
#plot(Height, LungCap, main="Polynomial Regression",las=1)
library(ggplot2)
attach(mtcars)
plot(disp, mpg, main="disp vs mpg",las=1)

plot(disp, mpg, main="Polynomial Regression",las=2)

#now lets fit linear model
model1 <- lm(mpg ~ disp)
summary(model1)

#add line to the plot make it thick and red
abline(model1,lwd=3,col="red")

#Fitting the polynomial regression model with degree 2
model2 <- lm(mpg ~ disp + I(disp^2))
summary(model2)
lines(smooth.spline(disp,predict(model2)),col="blue",lwd=3)

#Test if the model including Height^2 is significant,better than one with
#using partial F-test

anova(model1,model2)

#Try fitting a model that includes Height^3 as well
model3 <- lm(mpg ~ disp + I(disp^2) + I(disp^3))
summary(model3)

anova(model2,model3)

#now lets add this model to the plot using a thick green line
lines(smooth.spline(disp,predict(model3)),col="green",lwd=3,lty=3)
legend(46,15,legend =c("model1: linear","model2: poly X^2", "model3: poly x^2 + x^3"),col=c("red","blue","green"),lty=c(1,1,3),lwd=3,bty="n",cex=0.9)