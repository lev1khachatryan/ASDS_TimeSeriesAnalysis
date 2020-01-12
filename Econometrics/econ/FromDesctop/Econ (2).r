library(AER)
data("Journals", package = "AER")
dim(Journals)
names(Journals)
attach(Journals)
plot(subs~ price/citations)
pr_cit<-price/citations
plot(subs~ pr_cit)
journals <- Journals[c("subs", "price")]
names(journals)
journals$citeprice <- Journals$price/Journals$citations
summary(journals)
plot(log(subs) ~ log(citeprice), data = journals)
y<-log(journals$subs)
x<-log(journals$citeprice)
jour_lm <- lm(y ~ x)
abline(jour_lm)
summary(jour_lm)
# class(jour_lm)
# names(jour_lm)
# summary(jour_lm)
# xbar <- mean(x)
# ybar <- mean(y)
# Sxy <- sum( (y-ybar) * (x-xbar) )
# Sxx <- sum( (x-xbar)^2 )
# n <- length(y)
# b1.hat <- cov(y,x)/var(x)
# b1.hat
# c(b1.hat, Sxy/Sxx)
# b0.hat <- ybar - b1.hat * xbar
# b0.hat
# yfit <- b0.hat + b1.hat * x
# e <- y - yfit
# sig.hat <- sqrt(sum(e^2)/(n-2))
# sig.hat
# disp <- c(beta1.hat = b1.hat, beta0.hat = b0.hat,
#           sigma.hat = sig.hat)
# disp
# signif(x = disp, digits = 2)
# sig.hat1 <- sqrt(sum(e^2)/n)
# sig.hat1
# xseq<-seq(-4,4,.01)
# densities<-dnorm(xseq, 0,1)
# densities1<-dnorm(xseq, 2,1)
# cumulative<-pnorm(xseq, 0, 1)
# 
# ggplot(as_tibble(cbind(xseq,densities)))+
#   geom_point(mapping=aes(x=xseq,y=densities))
###################
###################Lesson 3
###################
install.packages('AER')
library(AER)
# library(CPS1988)
data("CPS1988")

# summary(CPS1988)

y <- CPS1988$wage
x <- CPS1988$experience
plot( x,y)
plot(x,log(y))
lgy <- log(y)
model1 <- lm(lgy ~ x)  # regression model enq sarqum , log(wage) = a + b*experience
model1 #  a = 5.9303      b = 0.0132, exp-y 1 miavorov bqardzranalu depqum wage-i miavory bardzranaluya
# ayd a-n ev b-n yntrvela nenc , vor OLS-y minimize lini
abline(model1) # regression havasarman gicna qashum
summary(model1) # 
xbar <- mean(x)
lgybar <- mean(lgy)

summary(ceosal2)


coef(model1)

