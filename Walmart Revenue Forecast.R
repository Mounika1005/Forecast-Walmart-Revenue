
## USE FORECAST AND ZOO LIBRARIES.
library(forecast)
library(zoo)


## CREATE DATA FRAME.

# Set working directory for locating files.
setwd("C:/Users/STSC/Documents/Semester3/Time Series/Case_Studies/New/CS_2")

# Create data frame.
revenue.data <- read.csv("case2(1).csv")

# See the first 6 records of the file.
head(revenue.data)

## 1.Plot the data and visualize time series components
#1.a.Create time series data set in R using the ts() function.  

revenue.ts <- ts(revenue.data$Revenue, 
                   start = c(2005, 1), end = c(2020, 2), freq = 4)
revenue.ts

#1.b.Apply the plot() function to create a data plot with the historical data

plot(revenue.ts, 
     xlab = "Year", ylab = "Revenue", 
     ylim = c(60000, 145000), main = "Quarterly revenues (in $million) in Walmart", col = "blue")
axis(1, at = seq(2005, 2020))


## 2.Apply five regression models using data partition. 
#2.a.Develop data partition with the validation partition of 16 periods and the rest for the training partition 

nValid <- 16
nTrain <- length(revenue.ts) - nValid
train.ts <- window(revenue.ts, start = c(2005, 1), end = c(2005, nTrain))
valid.ts <- window(revenue.ts, start = c(2005, nTrain + 1), 
                   end = c(2005, nTrain + nValid))

train.ts
valid.ts


#2.b.Use the tslm() function for the training partition to develop each of the 5 regression models from the above list
#i.	Regression model with linear trend
train.linear <- tslm(train.ts ~ trend)

summary(train.linear)

train.linear.pred <- forecast(train.linear, h = nValid, level = 0)
train.linear.pred


#ii.	Regression model with quadratic trend
train.quad <- tslm(train.ts ~ trend + I(trend^2))

summary(train.quad)

train.quad.pred <- forecast(train.quad, h = nValid, level = 0)
train.quad.pred

#iii.	Regression model with seasonality 
train.season <- tslm(train.ts ~ season)

summary(train.season)

train.season.pred <- forecast(train.season, h = nValid, level = 0)
train.season.pred


#iv.	Regression model with linear trend and seasonality
train.lin_trend.season <- tslm(train.ts ~ trend + season)

summary(train.lin_trend.season)

train.lin_trend.season.pred <- forecast(train.lin_trend.season, h = nValid, level = 0)
train.lin_trend.season.pred


#v.	Regression model with quadratic trend and seasonality
train.quad_trend.season <- tslm(train.ts ~ trend + I(trend^2) + season)

summary(train.quad_trend.season)

train.quad_trend.season.pred <- forecast(train.quad_trend.season, h = nValid, level = 0)
train.quad_trend.season.pred


#c.Apply the accuracy() function to compare performance measure of the 5 forecasts 

round(accuracy(train.linear.pred$mean, valid.ts), 3)
round(accuracy(train.quad.pred$mean, valid.ts), 3)
round(accuracy(train.season.pred$mean, valid.ts), 3)
round(accuracy(train.lin_trend.season.pred$mean, valid.ts),3)
round(accuracy(train.quad_trend.season.pred$mean, valid.ts),3)



## 3.Employ the entire data set to make time series forecast
#3.a.Apply the two most accurate regression models identified in question to make the forecast for the last
## two quarters of 2020 and first two quarters of 2021

#i. Regression model with linear trend and seasonality

revenue.lin_trend.seas <- tslm(revenue.ts ~ trend + season)

summary(revenue.lin_trend.seas)

revenue.lin_trend.seas.pred <- forecast(revenue.lin_trend.seas, h = 8, level = 0)
revenue.lin_trend.seas.pred


#ii. Regression model with quadratic trend and seasonality

revenue.quad_trend.season <- tslm(revenue.ts ~ trend + I(trend^2) + season)

summary(revenue.quad_trend.season)

revenue.quad_trend.season.pred <- forecast(revenue.quad_trend.season, h = 8, level = 0)
revenue.quad_trend.season.pred


#3.b.Apply the accuracy() function to compare the performance measures of the regression models developed in 3a with those
# for naïve and seasonal naïve forecasts

round(accuracy(revenue.lin_trend.seas.pred$fitted, revenue.ts),3)
round(accuracy(revenue.quad_trend.season.pred$fitted, revenue.ts),3)
round(accuracy((naive(revenue.ts))$fitted, revenue.ts), 3)
round(accuracy((snaive(revenue.ts))$fitted, revenue.ts), 3)
