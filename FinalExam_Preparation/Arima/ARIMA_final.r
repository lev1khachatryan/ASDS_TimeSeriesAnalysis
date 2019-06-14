library('ggplot2')
# install.packages('forecast')
# install.packages('xts')
library('forecast')
library('tseries')
getwd()
setwd("C:/_Files/MyProjects/ASDS_2/TimeSeriesAnalysis/FinalExam_Preparation/Arima")
daily_data = read.csv('day.csv', header=TRUE, stringsAsFactors=FALSE)

head(daily_data)

daily_data$Date = as.Date(daily_data$dteday)

ggplot(daily_data, aes(Date, cnt)) + geom_line() +
  scale_x_date('month')  + ylab("Daily Bike Checkouts") +  xlab("")

count_ts = ts(daily_data[, c('cnt')])

############################
## Clean outliers
###########################
daily_data$clean_cnt = tsclean(count_ts)

ggplot() +
  geom_line(data = daily_data, aes(x = Date, y = clean_cnt)) +
  ylab('Cleaned Bicycle Count')

############################
## weekly and monthly MA model
###########################
daily_data$cnt_ma = ma(daily_data$clean_cnt, order=7) # using the clean count with no outliers
daily_data$cnt_ma30 = ma(daily_data$clean_cnt, order=30)

ggplot() +
  geom_line(data = daily_data, aes(x = Date, y = clean_cnt, colour = "Counts")) +
  geom_line(data = daily_data, aes(x = Date, y = cnt_ma,   colour = "Weekly Moving Average"))  +
  geom_line(data = daily_data, aes(x = Date, y = cnt_ma30, colour = "Monthly Moving Average"))  +
  ylab('Bicycle Count')

############################
## na.omit 
## https://stat.ethz.ch/R-manual/R-devel/library/stats/html/na.fail.html
###########################
count_ma = ts(na.omit(daily_data$cnt_ma), frequency=30)
count_ma
daily_data$cnt_ma

############################
## en 4 hatanoc graphna decomp-y
###########################
decomp = stl(count_ma, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
deseasonal_cnt
plot(deseasonal_cnt)
plot(decomp)

############################
## Dicky Fuller
###########################
adf.test(count_ma, alternative = "stationary")

Acf(count_ma, main='')
Pacf(count_ma, main='')

count_d1 = diff(deseasonal_cnt, differences = 1)
plot(count_d1)
adf.test(count_d1, alternative = "stationary")

Acf(count_d1, main='ACF for Differenced Series')
Pacf(count_d1, main='PACF for Differenced Series')

auto.arima(deseasonal_cnt, seasonal=FALSE)

fit<-auto.arima(deseasonal_cnt, seasonal=FALSE)

par(mar=c(1,1,1,1))
# dev.off()

tsdisplay(residuals(fit), main='(1,1,1) Model Residuals')

fit2 = arima(deseasonal_cnt, order=c(1,1,7))
fit2
tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model Residuals')

fcast <- forecast(fit2, h=30)
plot(fcast)

############################
## Forecasting and comparing with actual result
###########################
hold <- window(ts(deseasonal_cnt), start=700)
fit_no_holdout = arima(ts(deseasonal_cnt[-c(700:725)]), order=c(1,1,7))
fcast_no_holdout <- forecast(fit_no_holdout,h=25)
plot(fcast_no_holdout, main=" ")
lines(ts(deseasonal_cnt))

fit_w_seasonality = auto.arima(deseasonal_cnt, seasonal=TRUE)
fit_w_seasonality

seas_fcast <- forecast(fit_w_seasonality, h=30)
plot(seas_fcast)
