install.packages("tidyverse")
library(tidyverse)

#sim 1
sim1 <- tibble(x = seq(0, 3.5 * pi, length = 50), y = 4 * sin(x) + rnorm(length(x), sd = 1))
ggplot(sim1, aes(x , y)) + geom_line()
sim1

#sim 2
sim2 <- tibble(x = seq(0, 3.5 * pi, length = 50), y = 4 * cos(x) + rnorm(length(x)))
ggplot(sim2, aes(x, y))+ geom_line()
sim2

#sim 3
sim2%>%mutate(sim1$y)  ## > ic aj koghminy kiraruma dzax masi vra
# mutate - miacnel
ggplot(sim2,aes(x))+geom_line(aes(y = y, colour = "y")) + 
  geom_line(aes(y = sim1$y, colour = "sim1$y"))

#sim 4
sim4 <- tibble(x = seq(0, 100, length = 50), y = 1.4 * x + rnorm(length(x)))
ggplot(sim4, aes(x, y))+ geom_line()

#sim 4 part 2
sim4 <- tibble(x = seq(0, 100, length = 50), y = 0.1 * x + rnorm(length(x)))
ggplot(sim4, aes(x, y))+ geom_line()

#sim 5
sim5 <- tibble(x = seq(0, 100, length = 50), y = 1.4 * x + rnorm(length(x), 0.5, 3))
ggplot(sim5, aes(x, y))+ geom_line()

#sim 6
sim6 <- tibble(x = seq(0, 100, length = 50), y = 1.4 * cos(x) + rnorm(length(x), 0.5, 10))
ggplot(sim6, aes(x, y))+ geom_line()+ geom_smooth(method = "lm", se=TRUE) # se - standard error

#sim7
a<-0.3
n<-50
x<-c()
x[1]<-1
for(i in 2:n){
  x[i]=a*x[i-1]
}
time <- seq(from = as.Date("1970-01-01"), to = as.Date("2019-01-01"), by = 'year')
series <- ts(x, frequency=1, start=c(1970)) 
#year = format(time, "%Y")
l<-cbind.data.frame(time,x)
colnames(l)<-cbind("time","series")
data<-as.tibble(l)
ggplot(data, aes(time, x), xlab=format(time, "%Y")) + geom_line()

#sim8
a<-0.3
n<-50
x<-c()
e<-rnorm(n, mean = 0, sd = 1)
for(i in 2:n){
  x[1]<-1
  x[i]=a*x[i-1]+e[i]
}
time <- seq(from = as.Date("1970-01-01"), to = as.Date("2019-01-01"), by = 'year')
series <- ts(x, frequency=1, start=c(1970)) 
#year = format(time, "%Y")
l<-cbind.data.frame(time,x)
colnames(l)<-cbind("time","series")
data<-as.tibble(l)
ggplot(data, aes(time, series), xlab=format(time, "%Y")) + geom_line()

## Stationarity momend ka  , a > 1 , heto nayelu enq 

#sim8
a<-1.3
n<-50
x<-c()
e<-rnorm(n, mean = 0, sd = 1)
for(i in 2:n){
  x[1]<-1
  x[i]=a*x[i-1]+e[i]
}
time <- seq(from = as.Date("1970-01-01"), to = as.Date("2019-01-01"), by = 'year')
series <- ts(x, frequency=1, start=c(1970)) 
#year = format(time, "%Y")
l<-cbind.data.frame(time,x)
colnames(l)<-cbind("time","series")
data<-as.tibble(l)
ggplot(data, aes(time, x), xlab=format(time, "%Y")) + geom_line()

library(quantmod)
## The following code does not work
getSymbols("DJI",from="2019-01-01", to="2019-02-20")
chartSeries(DJI[,6])
chartSeries(diff(DJI)[,6])






X <- seq(0, 100, length = 50)
Y <- 1.4 * sin(x) + rnorm(length(x), 0.5, 10)
data <- tibble(x = X, y = Y)
ggplot(data, aes(X, Y))+ geom_line()+ geom_smooth(method = "lm", se=TRUE) # se - standard error


