install.packages("CEOSAL2")
library(ceosal2)

data('ceosal2')
summary(ceosal2)

install.packages("wooldridge")
library(wooldridge)

attach(ceosal2)

mean(comten)

sum(ceosal2$ceoten == 0)

ceosal2[ceosal2$ceoten == max(ceosal2$ceoten),]

myDf <- as.data.frame(ceosal2)

myDf[ceosal2$ceoten == max(ceosal2$ceoten)]

par(mfrow=c(1,1))
model1 <- lm(log(salary) ~ ceoten)
plot(log(salary) ~ ceoten, data = ceosal2)
abline(model1)


b1 <- (sum((ceoten-mean(ceoten))*(log(salary)-mean(log(salary)))))/(ceoten-mean(ceoten))^2
b1



