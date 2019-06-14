install.packages('MSwM')
library(MSwM)
data(example)
vignette("examples")
View(example)
plot(ts(example))

############################
## Simple Linear Regression
###########################
mod=lm(y~x,example)
summary(mod)

############################
## Visualize Linear regression
###########################
library(tidyverse)
ggplot(data=example)+geom_line(aes(x=x, y=y))+geom_line(aes(x,mod$fitted.values))
qqnorm(mod$residuals)
qqline(mod$residuals)
acf(mod$residuals)


bull1 = rnorm( 100, 0.10, 0.15 )
bear  = rnorm( 100, -0.01, 0.20 )
bull2 = rnorm( 100, 0.10, 0.15 )
true.states = c(rep(1,100),rep(2,100),rep(1,100))
returns = c( bull1, bear,  bull2 )

############################
## Markov Switching
###########################
mod.mswm=msmFit(mod,k=2,p=1,sw=c(TRUE,TRUE,TRUE,TRUE),control=list(parallel=FALSE))
summary(mod.mswm)
plotProb(mod.mswm, which = 1)
plotProb(mod.mswm, which = 2)
plotProb(mod.mswm, which = 3)
plotReg(mod.mswm,expl="x")
plot(mod.mswm)
plotDiag(mod.mswm, which = 1)
plotDiag(mod.mswm, which = 2)
plotDiag(mod.mswm, which = 3)



#par(mar=c(1.5,1.5,1.5,1.5))

data("traffic")
View(traffic)
plot(ts(traffic[-1]))

############################
## Deneralized Linear Models
###########################
model=glm(NDead~Temp+Prec,traffic,family="poisson")
summary(model)

m1=msmFit(model,k=2,sw=c(TRUE,TRUE,TRUE),family="poisson",control=list(parallel=FALSE))
summary(m1)
intervals(m1)
plot(m1)
plotProb(m1,which=2)


bull1 = rnorm( 100, 0.10, 0.15 )
bear  = rnorm( 100, -0.01, 0.20 )
bull2 = rnorm( 100, 0.10, 0.15 )
true.states = c(rep(1,100),rep(2,100),rep(1,100))
returns = c( bull1, bear,  bull2 )

