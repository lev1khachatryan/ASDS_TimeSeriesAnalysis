rm(list = ls())
library(AER)
data("CPS1988")
summary(CPS1988)
head(CPS1988)
y<- CPS1988$wage
x<- CPS1988$experience
plot(x, y, xlab = "Experience", ylab = "Wage")
plot(x, log(y), xlab = "Experience", ylab = "Log(Wage)")
lgy<-log(y)
model1<-lm(lgy~x)
model1
abline(model1)
summary(model1)
xbar <- mean(x)
lgybar <- mean(lgy)
Sxy <- sum( (lgy-lgybar) * (x-xbar) )
Sxx <- sum( (x-xbar)^2 )
n <- length(y)
b1.hat <- cov(lgy,x)/var(x)
b1.hat
c(b1.hat, Sxy/Sxx, model1$coefficients[2])
b0.hat <- lgybar - b1.hat * xbar
b0.hat
c(b0.hat,model1$coefficients[1])
lgyfit <- b0.hat + b1.hat * x
e <- lgy - lgyfit
sig.hat <- sqrt(sum(e^2)/(n-2))
sig.hat
disp <- c(beta1.hat = b1.hat, beta0.hat = b0.hat,
          sigma.hat = sig.hat)
disp
signif(x = disp, digits = 2)


#xseq<-seq(-4,4,.01)
#densities<-dnorm(xseq, 0,1)
#densities1<-dnorm(xseq, 2,1)
#cumulative<-pnorm(xseq, 0, 1)
#library(tidyverse)
#ggplot(as_tibble(cbind(xseq,densities)))+
 # geom_point(mapping=aes(x=xseq,y=densities))

cps_lm <- lm(log(wage) ~ experience + I(experience^2) +
               + education + ethnicity, data = CPS1988)
summary(cps_lm)
cps_noeth <- lm(log(wage) ~ experience + I(experience^2) +
                  + education, data = CPS1988)
anova(cps_noeth, cps_lm)
anova(cps_lm)
