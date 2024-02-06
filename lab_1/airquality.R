# Handling the data
data("airquality")
View(airquality)
dim(airquality)
head(airquality)
tail(airquality)
colnames(airquality)
colSums(is.na(airquality))
str(airquality)
summary(airquality)
summary(airquality$Ozone)

# Handling missing values
mean(airquality$Ozone)
is.na(airquality)
mean(airquality$Ozone, na.rm = TRUE)
# airquality$Ozone[is.na(airquality$Ozone) <- mean(airquality$Ozone, na.rm = TRUE)]

airquality$Ozone <- ifelse(is.na(airquality$Ozone), 
                           mean(airquality$Ozone, na.rm = TRUE), 
                           airquality$Ozone)