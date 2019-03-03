library(tidyverse)
library(gridExtra)

################
## Exercise 1 ##
################

rm(list = ls())
n <- 1000
x <- c()
x[1] <- 1
e <- rnorm(n, mean = 0, sd = 1)
for(i in 2:n){
  x[i] <- x[i-1] +e[i]
}
sim1<-tibble(t=seq(0,n, length=n), x)
x <- c()
x[1] <- 1
e <- rnorm(n, mean = 0, sd = 1)
for(i in 2:n){
  x[i] <- x[i-1] +e[i]
}
sim2<-tibble(t=seq(0,n, length=n), x)
data<-sim1%>%mutate(sim2$x)
p1<-ggplot(data, aes(t))+geom_line(aes(y=x, colour = "x")) + geom_line(aes(y=sim2$x, colour= "sim2$x"))
p2<-ggplot(data, aes(x,sim2$x)) + geom_point() + geom_smooth(method = "lm", se = F)
grid.arrange(p1, p2, ncol=2)


################
## Exercise 2 ##
################
rm(list = ls())
var_1 <- 2
var_2 <- 7
slope_1 <- 0.5
slope_2 <- -0.5
n_1 <- 500
n_2 <- 1500
sim <- c()
sim[1] <- 1
for(i in 2:n_1){
  sim[i] <- slope_1*sim[i-1] + rnorm(1,0,var_1)
}
sim[n_1+1] <- 1
for(i in (n_1+2):n_2){
  sim[i] <- slope_2*sim[i-1] + rnorm(1,0,var_2)
}
simulation <- tibble(t=seq(0,n_2, length=n_2), sim)
ggplot(simulation, aes(t, sim))+ geom_line()






