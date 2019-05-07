rm(list = ls())
library(quantmod)
library('zoo')
library('xts')
library('tseries')
# install.packages('Quandl')
library('Quandl')
library('urca')

watchlist=c("IBM", 'GE')
getSymbols(watchlist,from="2018-01-01",to="2019-01-01",periodicity = 'daily' )
head(IBM)
head(GE)
chartSeries(IBM)
chartSeries(GE)

## a. Calculate daily returns
IBM <- merge(IBM,dailyReturn(IBM))
head(IBM)
GE <- merge(GE,dailyReturn(GE))
head(GE)

## b. Construct the portfolio of two assets with equal weights and calculate portfolio daily
##    returns, which is weighted average of individual returns.

w <- 0.5
R <- w*IBM$daily.returns + (1-w)*GE$daily.returns

# R$daily.returns

## c. Test the stationarity of portfolio return.
adf_test <- adf.test(R,alternative = 'stationary')
print(adf_test)

## As we can see from above printed result, new obtained time series is stationar because of p-value is less than 0.05
## therefore we reject H0 hypothesis,  return is stationary

## d. Download daily values of Dow-Jones Industrial (DJI) for last year.

watchlist=c("DJI")
getSymbols(watchlist,from="2018-01-01",to="2019-01-01",periodicity = 'daily' )
head(DJI)

## e. Calculate daily returns for DJI.

DJI <- merge(DJI,dailyReturn(DJI))
head(DJI)

## f. Conduct linear regression between Portfolio and DJI. Include AR(1) or MA(1) in the
## model. Which model is better describe relationship between portfolio and DJI. Explain
## your answer.

Reg_1 <- lm(R ~ DJI$daily.returns)
summary(Reg_1)  ##  Adjusted R-squared:  0.3222 

res = resid(Reg_1)
head(res)

res_shifted = c()
for (i in 2:length(res)){
  res_shifted[i - 1] <- res[i]
}
res_shifted[i] = 0
model_MA <- lm(R ~ DJI$daily.returns + res_shifted)
summary(model_MA) # Adjusted R-squared:  0.3202 



return_shifted = c()
for (i in 2:length(R)){
  return_shifted[i - 1] <- R[i]
}
return_shifted[i] = 0
model_AR <- lm(R ~ DJI$daily.returns + return_shifted)
summary(model_AR) # Adjusted R-squared:  0.323 


## Comparing the Adjusted R-squares, we can say that both models are fit the data in the same approximation.
## But the second one ( with AR(1) process) Adjusted R-squared a little bit higher than the first model's r-squared
## That's why i think that mode with AR(1) process is better than the model with MA(1) process in that case.


