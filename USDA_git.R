#Reading Data
setwd("C:\\Users\\Vaibhav\\Desktop\\Chaps\\ML\\Assignments\\USDA")
USDA=read.csv("USDA.csv",header=T, stringsAsFactors=F)

example(ts)

#Loading libraries
library(dplyr)
library(tidyr)

#Defining Functions

plotForecastErrors <- function(forecasterrors)
{
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

#Code
USDA1=separate(USDA,Day.of.last_activity_date.Count,c("Date","last_activity_dateCount"),sep='\t')

#spreading data
USDA1$Date=as.Date(USDA1$Date,format = "%d-%b-%y")
USDA1
str(USDA1)
USDA1$last_activity_dateCount=as.numeric(USDA1$last_activity_dateCount)
USDA_daily=ts(USDA1[,2], start=c(2015,1,1), end=c(2016,5,18), frequency = nrow(USDA1))
plot.ts(USDA_daily)
class(USDA_daily)
USDA_daily
(USDA_daily)

#Trend is constant
#variance is constant
#covariance is constant


#Differencing Time Series
d=0

#USDA_daily_diff1=diff(USDA_daily, differences=1)
library(forecast)
auto.arima(USDA_daily)
#suggests ARIMA(1,0,3) as the best model

USDA_daily_arima = arima(USDA_daily, order=c(1,0,3)) 
USDA_daily_arima

USDA_daily_forecasts <- forecast.Arima(USDA_daily_arima, h=5)
USDA_daily_forecasts
plot.forecast(USDA_daily_forecasts)

#correlogram of residuals using ACF
acf(USDA_daily_forecasts$residuals, lag.max=20)

#Ljung-Box Test
Box.test(USDA_daily_forecasts$residuals, lag=20, type="Ljung-Box")
#so we accept null hyp that autocorrelation between residuals is 0s
plot.ts(USDA_daily_forecasts$residuals)            # make time plot of forecast errors

plotForecastErrors(USDA_daily_forecasts$residuals) # make a histogram
#looking normal

#so we finalize on order (1,0,3) arima model
