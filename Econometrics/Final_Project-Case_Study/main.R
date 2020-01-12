# R.version.string # "R version 3.5.1 (2018-07-02)"
rm(list = ls())
df = read.csv("D:/MyProjects/Econometrics_Homeworks/Final_Project-Case_Study/DI2007_Regional_Database.csv", header = TRUE)
# library(haven)
# df <- read_sav("D:/MyProjects/Econometrics_Homeworks/Final_Project-Case_Study/DI2007_Regional_Database.sav")
# df <- as.data.frame(df)
# df <- df[,-which(sapply(df, class) == "logical")]
# df <- sapply( df, as.numeric )
dim(df)
unique(sapply(df, class))  ## "integer" "factor"  "logical" "numeric"
class(df)  ## "data.frame"
# unique(df$a25)

df[is.na(df$a25), "a25"] <- 0

df[df$a25 > 1,"a25"] <- 1

unique(sapply(df, class))  ## "integer" "factor"  "logical" "numeric"
unique(df$a25)  ## 0 1
names(df)
attach(df)
levels(df$a25)
# df$ï.¿diaryid

df$a25 <- as.factor(df$a25)

# logitmodel <- glm(a25 ~  ., family=binomial(link="logit"), data=df)
levels(df$a25)



logitdata = read.csv("D:/MyProjects/Econometrics_Homeworks/Homework_4/sources/bankloan_cs_noweights.csv", header = TRUE)
unique(sapply(logitdata, class))  ## "integer" "factor"  "numeric"
class(logitdata$default)

logitmodel <- glm(default ~  ., family=binomial(link="logit"), data=logitdata)
LogL<-logLik(logitmodel)
LogL
