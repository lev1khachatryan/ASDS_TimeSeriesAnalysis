rm(list = ls())
library(quantmod)
library('zoo')
library('xts')
library('tseries')
# install.packages('Quandl')
library('Quandl')
library('urca')

#####################################################################################################################
## 1. Download one of the following time series: Yahoo, Microsoft, IBM, and Google. (Weakly data for last 3 years) ##
#####################################################################################################################
watchlist=c("IBM")
getSymbols(watchlist,from="2016-01-01",to="2019-12-31",periodicity = 'weekly' )
head(IBM)
chartSeries(IBM)

#################################################################################################
## 2. Test downloaded time series for stationarity (Dickey -Fuller test). Comment your results ##
#################################################################################################
adf_test <- adf.test(IBM[,6],alternative = 'stationary')
print(adf_test)

## As we can see the p-value is equal to 0.03461 which means that we Reject H0 hypothesis, 
## So we can say tht our time series is stationary


#######################################
## 3. Generate weekly return series. ##
#######################################

IBM <- merge(IBM,weeklyReturn(IBM))
head(IBM)

##############################################################################
## 4. Test the stationarity for weekly return series. Comment your results. ##
##############################################################################
adf_test_2 <- adf.test(IBM[,7],alternative = 'stationary')
print(adf_test_2)

## In this case p-value is equal to 0.01 therefore we reject H0 hypothesis
## Which means that weakly returns time series is stationary as well.


#####################################################################################################
## 5. Use ACF and PACF correlograms for weekly returns and select from following possible answers: ##
#####################################################################################################
head(IBM)
acf(IBM[,7])
pacf(IBM[,7])

## With taken into account that acf and pacf are not similar to neither AR's acf pacf nor MA's acf pacf
## we can say that IBM time series not AR not MA model.
## So it is	Impossible to classify IBM time series as AR(1) or MA(1)



