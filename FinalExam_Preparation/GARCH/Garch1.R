install.packages('quantmod')
require(quantmod)

##################################
## Get data from WEB
##################################
getSymbols("^GSPC",from="2007-01-03",to="2017-04-06")

dim(GSPC)
head(GSPC, 2)
spc <- log(as.numeric(GSPC[,6]))
spc

rtn <- diff(spc)
acf(rtn^2)
pacf(rtn^2)
acf(rtn)
pacf(rtn)

##################################
## Arima model
##################################
m1 <- arima(rtn,order=c(0,0,2))
m1
acf(m1$residuals)
acf(m1$residuals^2)
pacf(m1$residuals)
pacf(m1$residuals^2)

plot.ts(rtn)

##################################
## Install and Imort Garch model
##################################
install.packages('fGarch')
library(fGarch)

setwd("C:/_Files/MyProjects/ASDS_2/TimeSeriesAnalysis/FinalExam_Preparation/GARCH")


da=read.table("m-intc7303.txt",header=T)
head(da,2)
dim(da)
intc=log(da$rtn+1)
head(intc)

acf(intc)
pacf(intc)
acf(intc^2)
pacf(intc^2)

##################################
## Box test - Compute the Box--Pierce or Ljung--Box test 
## statistic for examining the null hypothesis of independence 
## in a given time series. These are sometimes known as 'portmanteau' tests.
## https://www.rdocumentation.org/packages/stats/versions/3.6.0/topics/Box.test
##################################
Box.test(intc^2,lag=10,type="Ljung")
Box.test(intc,lag=10,type="Ljung")

########################################
## Fit GARCH model
m1=garchFit(~garch(3,0),data=intc,trace=F)
summary(m1)



m1=garchFit(~arma(1,1)+garch(3,0),data=intc,trace=F)
summary(m1)

plot(m1)

m2=garchFit(~garch(1,0),data=intc,cond.dist="std",trace=F)
summary(m2)


sp5=scan("sp500.txt")
head(sp5)
length(sp5)

pacf(sp5)
acf(sp5)
pacf(sp5^2)
acf(sp5^2)

m1=arima(sp5,order=c(3,0,0))
m1
acf(m1$residuals)
pacf(m1$residuals)
acf(m1$residuals^2)
pacf(m1$residuals^2)
m2=garchFit(~arma(3,0)+garch(1,1),data=sp5,trace=F)
summary(m2)

m2=garchFit(~garch(1,1),data=sp5,trace=F)
summary(m2)


#############################
## Prediction in GARCH model
predict(m2,6)

m2=garchFit(~garch(1,1),data=intc,cond.dist="std",trace=F)
summary(m2)
