library(readxl)
library(ggplot2)
library(dplyr)
library(car)

# Reading data
data <- read_excel("lab_test/collected_data.xlsx")
names(data)

########### Question 1 (a) :: START ##############
# Convert Annual Income to lakhs
data$`Annual family income`
data$`Annual family income` <- ifelse(data$`Annual family income` < 1000, 
                                      data$`Annual family income` * 100000,
                                      data$`Annual family income`)
data$`Annual family income`

# Convert the education variable to numerical data
## Highest Education
unique(data$`Highest level of education`)
data$`Highest level of education` <- ifelse(data$`Highest level of education` == "12th", 12,
                                           ifelse(data$`Highest level of education` == "Graduate", 15,
                                                 ifelse(data$`Highest level of education` == "Post Graduate", 17, 0)))
data$`Highest level of education`

## Father's Education
unique(data$`Father's education level`)
data$`Father's education level` <- ifelse(data$`Father's education level` == "12th", 12,
                                            ifelse(data$`Father's education level` == "Diploma or Equivalent", 14,
                                                   ifelse(data$`Father's education level` == "Graduate", 15,
                                                          ifelse(data$`Father's education level` == "Post Graduate", 17,
                                                                 ifelse(data$`Father's education level` == "Doctoral Degree", 25,
                                                                        ifelse(data$`Father's education level` %in% c("Less than High School", "10th"), 10, 0))))))
data$`Father's education level`

# Mother's Education
unique(data$`Mother's education level`)
data$`Mother's education level` <- ifelse(data$`Mother's education level` == "12th", 12,
                                          ifelse(data$`Mother's education level` == "Diploma or Equivalent", 14,
                                                 ifelse(data$`Mother's education level`  %in% c("Graduate", "Graduate Profession Degree eg. B.Tech,MBBS,LLB,BBA,etc.", "Graduate (Arts/Science)", "B.Com"), 15,
                                                        ifelse(data$`Mother's education level` %in% c("Post Graduate", "Post Graduate  Professional Degree eg. MD,Mtech,LLM,MBAetc.", "Post Graduate  (Arts/Science)", "Post graduate MSC"), 17,
                                                               ifelse(data$`Mother's education level` == "Doctoral Degree", 25,
                                                                      ifelse(data$`Mother's education level` %in% c("Less than High School", "10th"), 10, 0))))))
data$`Mother's education level`
########### Question 1 (a) :: FINISH ##############

########### Question 1 (b) :: START ##############
library(dplyr)

# Remove outliers for Highest level of education
data <- data %>% filter(`Highest level of education` >= 10 & `Highest level of education` <= 25)

# Remove outliers for Father's education level
data <- data %>% filter(`Father's education level` >= 10 & `Father's education level` <= 25)

# Remove outliers for Mother's education level
data <- data %>% filter(`Mother's education level` >= 10 & `Mother's education level` <= 25)

# Remove outliers for Total siblings (including yourself)
siblings_outlier_threshold <- quantile(data$`Total siblings (including yourself)`, 0.99, na.rm = TRUE)
data <- data %>% filter(`Total siblings (including yourself)` <= siblings_outlier_threshold)

# Remove outliers for Annual family income
income_outlier_threshold <- quantile(data$`Annual family income`, 0.99, na.rm = TRUE)
data <- data %>% filter(`Annual family income` <= income_outlier_threshold)

# Remove outliers for Willingness to pay (in INR)
wtp_outlier_threshold <- quantile(data$`Willingness to pay (in INR)`, 0.99, na.rm = TRUE)
data <- data %>% filter(`Willingness to pay (in INR)` <= wtp_outlier_threshold)
########### Question 1 (b) :: FINISH ##############

View(data)
########### Question 1 (c) :: START ##############
# Categorical data
cat_vars <- c("Gender", "Family type", "Area type", "State", "Institute name", 
              "Field of study", "Father's employment", "Mother's employment", 
              "Housing situation", "Chosen Envelope")

# Numerical data
num_vars <- c("Age", "Total siblings (including yourself)", "Position among siblings", "No. of sisters",
              "No. of brothers", "Highest level of education", "Father's education level",
              "Mother's education level", "Annual family income")

# Box plots - categorical data
for (var in cat_vars) {
  p <- ggplot(data, aes(x = .data[[var]], y = `Willingness to pay (in INR)`, fill = .data[[var]])) +
    geom_boxplot() +
    theme_minimal() +
    xlab(var) +
    ylab("Willingness to Pay (in INR)") +
    ggtitle(paste("Willingness to Pay vs", var))
  print(p)
}

# Scatter plots - numerical data
for (var in num_vars) {
  p <- ggplot(data, aes(x = .data[[var]], y = `Willingness to pay (in INR)`)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    theme_minimal() +
    xlab(var) +
    ylab("Willingness to Pay (in INR)") +
    ggtitle(paste("Willingness to Pay vs", var))
  print(p)
}
########### Question 1 (c) :: FINISH ##############

########### Question 2 :: START ##############
# Multiple linear regression model
model <- lm(`Willingness to pay (in INR)` ~ ., data = data)
summary(model)

# Assessing model assumptions
par(mfrow=c(2,2))
plot(model)

# Check correlation matrix
correlation_matrix <- cor(data[, num_vars])
print(correlation_matrix)

# Remove 'No. of sisters' from the dataset as it as collinearity of 0.58 with Total number of siblings (including yourself)
data <- subset(data, select = -c(`No. of sisters`))
## Again doing multiple linear regression model
model <- lm(`Willingness to pay (in INR)` ~ ., data = data)

# Perform hypothesis tests for significance of predictors
summary(model)
anova(model)

# Model Fit
# R-squared and adjusted R-squared
summary(model)$r.squared
summary(model)$adj.r.squared
########### Question 2 :: FINISH ##############

########### Question 3 :: START ##############
# Labelling of Willingness to pay (in INR)
data$wtp_category <- ifelse(data$`Willingness to pay (in INR)` >= 1000, "High", "Low")
data$wtp_category <- as.factor(data$wtp_category)

# Importing libraries
library(e1071) # For SVM
library(rpart) # For Decision Tree
library(caret) # For Logistic Regression and model evaluation

# Split data into train and test
set.seed(123) # For reproducibility
train_idx <- createDataPartition(data$wtp_category, p = 0.8, list = FALSE)
train_data <- data[train_idx, ]
test_data <- data[-train_idx, ]

# SVM
svm_model <- svm(wtp_category ~ ., data = train_data, na.action = na.pass)

# Decision Tree
dt_model <- rpart(wtp_category ~ ., data = train_data)

# Logistic Regression
log_model <- train(wtp_category ~ ., data = train_data, method = "glm", family = "binomial")

# Get the names of all factor variables
factor_vars <- names(which(sapply(train_data, is.factor)))

# Relevel factor variables in test data
for (var in factor_vars) {
  test_data[[var]] <- factor(test_data[[var]], levels = levels(train_data[[var]]))
}

# Evaluate models on test data
svm_pred <- predict(svm_model, newdata = test_data)
dt_pred <- predict(dt_model, newdata = test_data, type = "class")
log_pred <- predict(log_model, newdata = test_data)

# Confusion Matrix
svm_cm <- confusionMatrix(svm_pred, test_data$wtp_category)
dt_cm <- confusionMatrix(dt_pred, test_data$wtp_category)
log_cm <- confusionMatrix(log_pred, test_data$wtp_category)

# AIC and BIC
svm_aic <- svm_model$aic
svm_bic <- svm_model$bic

dt_aic <- dt_model$cptable[length(dt_model$cptable), "CP"]
dt_bic <- dt_model$cptable[which.min(dt_model$cptable[, "xerror"] == dt_model$cptable[, "xstd"]), "CP"]

log_aic <- log_model$finalModel$aic
log_bic <- log_model$finalModel$bic

# F1 Score
svm_f1 <- svm_cm$byClass["F1"]
dt_f1 <- dt_cm$byClass["F1"]
log_f1 <- log_cm$byClass["F1"]

# Print results
cat("SVM:\n")
print(svm_cm)
cat("AIC:", svm_aic, "\nBIC:", svm_bic, "\nF1 Score:", svm_f1, "\n\n")

cat("Decision Tree:\n")
print(dt_cm)
cat("AIC:", dt_aic, "\nBIC:", dt_bic, "\nF1 Score:", dt_f1, "\n\n")

cat("Logistic Regression:\n")
print(log_cm)
cat("AIC:", log_aic, "\nBIC:", log_bic, "\nF1 Score:", log_f1, "\n\n")
########### Question 3 :: FINISH ##############