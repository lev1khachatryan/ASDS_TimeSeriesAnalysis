rm(list = ls())
library(quantmod)
library('zoo')
library('xts')
library('tseries')
# install.packages('Quandl')
library('Quandl')
library('urca')

##################################################################################
## 1. Download price time series for two financial instruments from list below: ##
## IBM, Yahoo, Amazon, General Electric.                                        ##
## Download daily prices for last year.                                         ##
##################################################################################
watchlist=c("IBM", 'GE')
getSymbols(watchlist,from="2018-01-01",to="2019-12-31",periodicity = 'daily' )
head(IBM)
head(GE)
chartSeries(IBM)
chartSeries(GE)

##########################################################################
## 2. Test the stationarity for price time series. (Dickey-Fuller test) ##
##########################################################################
adf_test <- adf.test(IBM[,6],alternative = 'stationary')
print(adf_test)

## As we can see the p-value is equal to 0.6617 which means that we fail to reject H0 hypothesis, 
## So we can say tht our time series is not stationary

adf_test2 <- adf.test(GE[,6],alternative = 'stationary')
print(adf_test2)

## As we can see the p-value is equal to 0.5029 which means that we fail to reject H0 hypothesis, 
## So we can say tht our time series is not stationary

##############################################
## 3.  Create time series for Daily Return. ##
##############################################
IBM <- merge(IBM,dailyReturn(IBM))
head(IBM)
GE <- merge(GE,dailyReturn(GE))
head(GE)


######################################################################
## 4. Test the stationarity for Daily Return. (Dickey-Fuller test). ##
######################################################################
adf_test_3 <- adf.test(IBM[,7],alternative = 'stationary')
print(adf_test_3)

## In this case p-value is equal to 0.01 therefore we reject H0 hypothesis
## Which means that IBM daily returns time series is stationary.

adf_test_4 <- adf.test(GE[,7],alternative = 'stationary')
print(adf_test_4)

## In this case p-value is equal to 0.01 therefore we reject H0 hypothesis
## Which means that GE daily returns time series is stationary.

################################################################################################
## 5. Analyze which type of time series proses (AR or MA) is more suitable for Daily Returns, ##
## using ACF and PACF correlograms.                                                           ##
################################################################################################
head(IBM)
acf(IBM[,7])
pacf(IBM[,7])

## With taken into account that acf and pacf are not similar to neither AR's acf pacf nor MA's acf pacf
## we can say that daily returns for IBM time series not AR not MA model.

head(GE)
acf(GE[,7])
pacf(GE[,7])

## With taken into account that acf and pacf are not similar to neither AR's acf pacf nor MA's acf pacf
## we can say that daily returns for IBM time series not AR not MA model.

