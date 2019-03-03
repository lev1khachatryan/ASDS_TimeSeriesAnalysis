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


#simulation of nonstationary in variance series

a<-3
b<-0
c<-0
n<-1000
sigma<-c()
sim5<-c()
for(i in 1:n){
  
  sigma[i]<-a*i^0.5
  sim5[i]<-b+c*i+rnorm(1,0,sigma[i])
  
}
        sim5_1<-tibble(t=seq(0,n, length=n), sim5)
        sim5_1
        ggplot(sim5_1, aes(t, sim5))+ geom_line()
       
         
#simulation of nonstationary in variance series
a1<-2
a2<-7
b<-0
c<-0
n<-1000
n1<-500
sigma<-c()
sim6<-c()
for(i in 1:n1){
          
    sim6[i]<-b+c*i+rnorm(1,0,a1)
}

for(i in (n1+1):n){
      sim6[i]<-b+c*i+rnorm(1,0,a2)
}

        sim6_1<-tibble(t=seq(0,n, length=n), sim6)
        sim6_1
        ggplot(sim6_1, aes(t, sim6))+ geom_line()