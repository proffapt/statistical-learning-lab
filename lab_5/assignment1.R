# Train/validation split
set.seed(1)
ind <- sample(1:nrow(Boston), size = round(0.75*nrow(Boston))) 
train <- Boston[ind,]
valid <- Boston[-ind,]

# Linear model
lm.fit <- lm(medv ~ rm, data = train) 

# Model validation
pred <- predict(lm.fit, newdata = valid)
mse <- mean((valid$medv - pred)^2)
cv.error <- cv.glm(train, glm(medv ~ rm, data = train))$delta[1]
cv.error.kfold <- cv.glm(train, glm(medv ~ rm, data = train), K = 5)$delta[1]

# Polynomial Models
cv.error.poly <- numeric(4)
for(i in 2:5){
  glm.fit <- glm(medv ~ poly(rm, i), data = train)
  cv.error.poly[i-1] <- cv.glm(train, glm.fit, K=5)$delta[1]
}

# Bootstrap 
boot.fn <- function(data, index) {
  fit <- lm(medv ~ rm, data = data, subset = index) 
  return(coef(fit)) 
}
set.seed(2)
boot.results <- boot(train, boot.fn, R = 100) 

# Plotting
par(mfrow=c(2,2))

mse <- rep(mse, 4)
plot(2:5, mse, cv.error, cv.error.poly, pch=19, 
     xlim=c(1,5), ylim = range(c(mse, cv.error, cv.error.poly)),
     xlab="Order", ylab="MSE")
legend("topright", legend=c("Validation", "LOOCV", "5-fold CV"),
       col=1:3, pch=19, cex=0.8)

plot(train$rm, train$medv, xlab="rm", ylab="medv")
abline(lm.fit, col="red")

qqnorm(resid(lm.fit)); qqline(resid(lm.fit))

int <- apply(boot.results$t,2,quantile,c(0.025,0.975))
plot(int, xlab="Coef", ylab="Value")
abline(h=0, lty=2)  

hist(boot.results$t[,2], xlab="Coefficient")
abline(v=coef(lm.fit)[2], col="red")