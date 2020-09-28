
##----------------------------------------------------------------------------------##
##--- SIMPLE LINEAR REGRESSION MODEL building ------##
##----------------------------------------------------------------------------------##


##--- Step 1: Clear environment variables ------------------------------------------##
rm(list = ls(all = TRUE))

##__________________________________________________________________________________##


##--- Step 2: Set working Directory ------------------------------------------------##
getwd()
##setwd

##__________________________________________________________________________________##


##--- Step 3: Read the data from the csv file --------------------------------------##
cars_data = read.csv(file = "Toyota_SimpleReg.csv", header = T)
cars_data
names(cars_data)
str(cars_data)
summary(cars_data)
##__________________________________________________________________________________##


##--- Step 4: Perform Exploratory Data Analysis and Data Pre-processing-------------##
## Drop any irrelevant attribute(s):
cars_data = cars_data[,-c(1,2)]
## Summary of the data and look for any missing values:
str(cars_data)
summary(cars_data)

## Correlation and Covariance between the attributes:
cov(cars_data)
# The covariance of the Age of car and Price is -59136.1089
# It indicates a negative linear relationship between the 2 variables
# This relation could be observed from the scatter plot also
#Describe how the covarainace and correlation coefficients 
plot(cars_data$Age_06_15, cars_data$Price)
plot(cars_data$Age_06_15, cars_data$Price, xlab = "Age of the car", ylab = "Price in ($)", pch=18, col = "red")
# pch = type of shape pch = diamond

cor(cars_data)
cor(cars_data$Age_06_15, cars_data$Price)

#Do the attributes have a good enough correlation coefficient to support linear regres  sion model building?
# The correlation of the Age of car and Price is -0.8765905
# Since the value is close to 1 and has a -ve sign, we can conclude that the variables are strongly negatively correlated
##__________________________________________________________________________________##


##--- Step 5: Split the data into train and test datasets --------------------------##
#Split in (train:test) in (70:30) ratio # We can split in 70:30, 80:20, 90:10 ratios
rows = seq(1, nrow(cars_data),1)
set.seed(123)
trainrows = sample(rows,(70*nrow(cars_data))/100)
cars_train = cars_data[trainrows,]
cars_test = cars_data[-trainrows,]

trainRows1 = sample(rows,(80*nrow(cars_data))/100)
cars_train1 = cars_data[trainRows1,]
cars_test1 = cars_data[-trainRows1,]

trainRow2 = sample(rows,(90*nrow(cars_data))/100)
cars_train2 = cars_data[trainRow2,]
cars_test2 = cars_data[-trainRow2,]
##__________________________________________________________________________________##


##--- Step 6: Linear regression model building--------------------------------------##
LinReg = lm(Price~Age_06_15, data = cars_train)
LinReg
coefficients(LinReg)
## Summary of model:
summary(LinReg)
plot(LinReg$residuals)

#Extract the intercept coefficient from the linear regression model
coefficients(LinReg)

#Extract the residual values


##__________________________________________________________________________________##


##--- Step 7: Check for validity of linear regression assumptions ------------------##
#HINT: plot the 4 graphs to check. Write your comments
par(nfrow = c(2,2))
plot(LinReg)
par(nfrow = c(1,1))
##__________________________________________________________________________________##


##--- Step 8: Predict on testdata --------------------------------------------------##
test_prediction = predict(LinReg, cars_test)
test_prediction
plot(test_prediction)
test_actual = cars_test$Price
plot(test_actual)
##__________________________________________________________________________________##


##--- Step 9: Error Metrics --------------------------------------------------------##
library(DMwR)

#Error verification on train data
regr.eval(cars_train$Price, LinReg$fitted.values)
# 4 TYPES of errors
# mae- mean Absolute error... vertical or horizontal distance between the data points
# mse - mean std error ...combination measurement of Bias and variance
#rmse- root of mse...
# helps to find what kind of error is there in the data.. where we can find if it is a +ve of -ve error
#mape - mean absolute percentage error..we are finding what is the %
plot(regr.eval(cars_train$Price, LinReg$fitted.values))

#Error verification on test data
regr.eval(test_actual, test_prediction)


##__________________________________________________________________________________##


##--- Step 10: Confidence and Prediction Intervals----------------------------------##
#Find the confidence and prediction intervals and plot them for the WHOLE dataset
# CI talk about the avg values intervals
# CI talk about all the individual values intervals
Conf_Pred = data.frame(predict(LinReg, cars_test, interval = "confidence", level = 0.95))
Pred_pred = data.frame(predict(LinReg, cars_test, interval = "prediction", level = 0.95))
str(Conf_Pred)
summary(Conf_Pred)
plot(Conf_Pred)
##__________________________________________________________________________________##
#-----------------------end---------------------------------------------------------##
# Data Visualization
plot(cars_test$Age_06_15, cars_test$Price, xlab = "Age of the car", ylab = "Price in($)")

points(cars_test$Age_06_15, Conf_Pred$fit,type = "l",col = "green",lwd = 2)
points(cars_test$Age_06_15,Conf_Pred$lwr,pch="-",col="red",lwd=4)
points(cars_test$Age_06_15,Conf_Pred$upr,pch="-",col="red",lwd=4)
points(cars_test$Age_06_15,Conf_Pred$lwr,pch="-",col="blue",lwd=4)
points(cars_test$Age_06_15,Conf_Pred$upr,pch="-",col="red",lwd=4)
