# install.packages("ggfortify")
# install.packages("tseries")
# install.packages("forecast")

library(ggfortify)
library(tseries)
library(forecast)
data(AirPassengers)
AP <- AirPassengers
# Take a look at the class of the dataset AirPassengers
class(AP)
AP
sum(is.na(AP))
frequency(AP)
cycle(AP)

summary(AP)
plot(AP,xlab="Date", ylab = "Passenger numbers (1000's)",main="Air Passenger numbers from 1949 to 1961")

boxplot(AP~cycle(AP),xlab="Date", ylab = "Passenger Numbers (1000's)" ,main ="Monthly Air Passengers Boxplot from 1949 to 1961")

decomposeAP <- decompose(AP,"multiplicative")
autoplot(decomposeAP)

adf.test(AP) 

autoplot(acf(AP,plot=FALSE))+ labs(title="Correlogram of Air Passengers from 1949 to 1961") 

autoplot(acf(decomposeAP$random[7:138],plot=FALSE))+ labs(title="Correlogram of Air Passengers Random Component from 1949 to 1961") 

autoplot(AP) + geom_smooth(method="lm")+ labs(x ="Date", y = "Passenger numbers (1000's)", title="Air Passengers from 1949 to 1961") 

arimaAP <- auto.arima(AP)
arimaAP
ggtsdiag(arimaAP)

forecastAP <- forecast(arimaAP, level = c(95), h = 36)
autoplot(forecastAP)

