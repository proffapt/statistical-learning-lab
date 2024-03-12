#lungCapacity
LungCapData2 <- read.table(file="~/Desktop/iitkgp/academics/sem6/stats_lab/lab_7/LungCapData2.csv", sep=",",header=T)
head(LungCapData2)
summary(LungCapData2)

#make a plot of LungCap vs Height
#plot(Height, LungCap, main="Polynomial Regression",las=1)

attach(LungCapData2)
plot(Height, LungCap, main="Height vs LungCap",las=1)

plot(Height, LungCap, main="Polynomial Regression",las=2)

#now lets fit linear model
model1 <- lm(LungCap ~ Height)
summary(model1)

#add line to the plot make it thick and red
abline(model1,lwd=3,col="red")

#Fitting the polynomial regression model with degree 2
model2 <- lm(LungCap ~ Height + I(Height^2))
summary(model2)
lines(smooth.spline(Height,predict(model2)),col="blue",lwd=3)

#Different way to fit a polynomial regression model
HeightSquare <- Height^2
model2again <- lm(LungCap ~ Height + HeightSquare)
summary(model2again)

#or use poly command
model2againagain <- lm(LungCap ~ poly(Height,degree=2,raw=T))
summary(model2againagain)

#Test if the model including Height^2 is significant,better than one with
#using partial F-test

anova(model1,model2)

#Try fitting a model that includes Height^3 as well
model3 <- lm(LungCap ~ Height + I(Height^2) + I(Height^3))
summary(model3)

anova(model2,model3)

#now lets add this model to the plot using a thick green line
lines(smooth.spline(Height,predict(model3)),col="green",lwd=3,lty=3)
legend(46,15,legend =c("model1: linear","model2: poly X^2", "model3: poly x^2 + x^3"),col=c("red","blue","green"),lty=c(1,1,3),lwd=3,bty="n",cex=0.9)
