rm(list = ls())
logitdata = read.csv("D:/MyProjects/Econometrics_Homeworks/Homework_4/sources/bankloan_cs_noweights.csv", header = TRUE)

smp_size <- floor(0.75 * nrow(logitdata))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(logitdata)), size = smp_size)

train <- logitdata[train_ind, ]
test <- logitdata[-train_ind, ]

dim(train)
dim(test)

logitmodel <- glm(default ~  ., family=binomial(link="logit"), data=train)
# summary(logitmodel)
LogL<-logLik(logitmodel)

# attach(train)
# attach(test)

###
logitmodel1 <- glm(default ~  ed, family=binomial(link="logit"), data=train)
LogL1<-logLik(logitmodel1)

logitmodel2 <- glm(default ~  1, family=binomial(link="logit"), data=train)
LogL2<-logLik(logitmodel2)
log(mean(default)) - log(1 - mean(train$default))

McF<-1-LogL/LogL2
McF

McF1<-1-LogL1/LogL2
McF1
# install.packages("rcompanion")
library(rcompanion)
nagelkerke(logitmodel)

default.prob = predict(logitmodel, type="response")
default.pred = rep(0, nrow(test))
default.pred[default.prob > .5] = 1
table(default.pred, logitmodel$data$default)

# install.packages("pROC")
library(pROC)
roc(logitmodel$data$default,default.pred)
plot.roc(logitmodel$data$default,default.pred)
