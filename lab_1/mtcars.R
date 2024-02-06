# Handling the data
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

# DESCRIPTIVE STATISTICS
# Check for duplicated rows in the mtcars dataset
duplicated(mtcars)
# Calculate the mean of the 'mpg' column in the mtcars dataset
mean(mtcars$mpg)
# Calculate the median of the 'mpg' column in the mtcars dataset
median(mtcars$mpg)
# Calculate the variance of the 'mpg' column in the mtcars dataset
var(mtcars$mpg)
# Calculate various quantiles (25%, 50%, and 75%) of the 'mpg' column in the mtcars dataset
quantile(mtcars$mpg)
# Calculate the range (minimum and maximum) of the 'mpg' column in the mtcars dataset
range(mtcars$mpg)
# Calculate the standard deviation of the 'mpg' column in the mtcars dataset
sd(mtcars$mpg)
# Calculate the interquartile range (IQR) of the 'mpg' column in the mtcars dataset
IQR(mtcars$mpg)
# Calculate the Pearson correlation matrix for all columns in the mtcars dataset
cor(mtcars, mtcars, method = "pearson", use = "complete.obs")
# Perform a Pearson correlation test between 'mpg' and 'cyl' columns in the mtcars dataset
cor.test(mtcars$mpg, mtcars$cyl, method = "pearson", use = "complete.obs")

#Visualize the data
install.packages("ggplot2")
library(ggplot2)
plot(mtcars)

plot(mpg~ cyl,mtcars)
plot(mtcars$cyl, xlab="observation number", ylab="Cylinder Value", pch=10, col="Blue", main="Scatter plot")
boxplot(mtcars)
boxplot(mtcars$mpg)
boxplot(mpg ~ cyl,mtcars,xlab="mpg",ylab="cyl")
palette()
boxplot(mtcars, cyl ~ disp, col=c(1,2,4,6,8))
boxplot(mtcars$cyl)
par(mfrow=c(1,2))
hist(mtcars$disp)
with(mtcars,hist(disp))
par(mfrow=c(1,1))

# Subsetting in R
mtcars[[1]]
mtcars[[1]][[2]]
mtcars$mpg
mtcarsq <- subset(mtcars, hp < 100, select = c(mpg, cyl))
print(mtcarsq)

# Filter or Subsetting rows in R using dplyr
library(dplyr)
#group_by()
select(mtcars, -(mpg:cyl))
select(mtcars, ends_with("T"))
print(filter(mtcars,cyl=="8"))
print(filter(mtcars,cyl==8 & hp > 100))
print(filter(mtcars,cyl==8 | hp > 100))
mtcars %>% slice_head(n = 5)
mtcars %>% slice_tail(n = 5)
mtcars %>% slice_max(cyl, n = 5)
mtcars %>% slice_min(cyl, n = 5)
print(sample_frac(mtcars, 0.2))
print(sample_frac(mtcars, 0.5))