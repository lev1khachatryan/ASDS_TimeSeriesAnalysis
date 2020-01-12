logitdata <- as.data.frame(bankloan_cs_noweights)
attach(logitdata)

logitmodel <- glm(default ~  ., family=binomial(link="logit"), data=logitdata)
summary(logitmodel)

LogL<-logLik(logitmodel)

logitmodel1 <- glm(default ~  ed, family=binomial(link="logit"), data=logitdata)
summary(logitmodel1)
LogL1<-logLik(logitmodel1)

logitmodel2 <- glm(default ~  1, family=binomial(link="logit"), data=logitdata)
summary(logitmodel2)
LogL2<-logLik(logitmodel2)
log(mean(default)) - log(1 - mean(default))

#logistic.model.list <- list(null = logitmodel2, lwt = logitmodel1, full = logitmodel)
#lapply(logistic.model.list, summary)

McF<-1-LogL/LogL2
McF

McF1<-1-LogL1/LogL2
McF1
library(rcompanion)
nagelkerke(logitmodel)

default.prob = predict(logitmodel, type="response")
default.pred = rep(0, nrow(logitdata))
default.pred[default.prob > .5] = 1
table(default.pred, logitmodel$data$default)


library(pROC)
roc(logitmodel$data$default,default.pred)
plot.roc(logitmodel$data$default,default.pred)



