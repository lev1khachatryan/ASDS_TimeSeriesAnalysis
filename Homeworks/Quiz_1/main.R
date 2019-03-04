library(tidyverse)
library(gridExtra)

################################
##                            ##
###### Generate two models######
##                            ##
## X[t] = 0.3*X[t-1] + e1[t]  ##
## Y[t] = e2[t] + 0.2*e2[t-1] ##
##                            ##
################################

rm(list = ls())
n <- 100
x <- c()
x[1] <- 1
e1 <- rnorm(n, mean = 0, sd = 1)
for(i in 2:n){
  x[i] <- 0.3 * x[i-1] +e1[i]
}
sim1<-tibble(t=seq(0,n, length=n), x)
y <- c()
e2 <- rnorm(n, mean = 0, sd = 1)
y[1] <- e2[1]
for(i in 2:n){
  y[i] <- e2[i] + 0.2*e2[i-1]
}
sim2<-tibble(t=seq(0,n, length=n), y)
data<-sim1%>%mutate(sim2$y)
p1<-ggplot(data, aes(t))+geom_line(aes(y=x, colour = "x")) + geom_line(aes(y=sim2$y, colour= "sim2$y"))
p2<-ggplot(data, aes(x,sim2$y)) + geom_point() + geom_smooth(method = "lm", se = F)
grid.arrange(p1, p2, ncol=2)
