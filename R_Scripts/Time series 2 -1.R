library(tidyverse)

library(gridExtra)

#simulation of stationary series
sim1 <- tibble(t = seq(0, 1000, length = 100), x = 6 + rnorm(length(t),0,3))
ggplot(sim1, aes(t, x))+ geom_line()

#simulation of nonstationary in mean series
sim2<-tibble(t=seq(0,10, length=100), x=6+6*t+rnorm(length(t),0, 5))
sim3<-tibble(t=seq (0, 10, length = 100), x=6+5*t +rnorm(length(t), 0, 5))
data<-sim2%>%mutate(sim3$x)


p1<-ggplot(data, aes(t))+geom_line(aes(y=x, colour = "x"))+
  geom_line(aes(y=sim3$x, colour= "sim3$x"))
p1

p2<-ggplot(data, aes(x,sim3$x))+
  geom_point()+
  geom_smooth(method = "lm", se = F)
p2

grid.arrange(p1, p2, ncol=2)

eq<-lm(sim3$x~ data$x)
summary(eq)


