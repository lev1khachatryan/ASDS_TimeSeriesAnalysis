# installing/loading the package:
if(!require(installr)) {
  install.packages("installr"); 
  library(installr)
} #load / install+load installr
# using the package:
# updateR()

#####
#####



rm(list = ls())
dat = read.csv("D:/MyProjects/Econometrics_Homeworks/Homework_3/sources/bankloan_cs_noweights.csv", header = TRUE)


#This data has blanks.  Let's convert the blanks to "NA"
dat[dat==""]  <- NA 

#Check for missing values using the sapply() function
sapply(dat, function(x) sum(is.na(x)))
#if you want to see how may unique values there are for each column:
sapply(dat, function(x) length(unique(x)))


overall_survival_rate = sum(dat$default == 1)/length(dat$default)
overall_survival_rate

cat("Fraction of people who've default = ", format(overall_survival_rate, digits = 3))



dat$ed<-as.factor(dat$ed)

dim(dat)

levels(dat$ed)
contrasts(dat$ed)


table(dat$ed)

names(dat)


# dat[,c("age","ed","employ","address","income","debtinc","creddebt","othdebt")]


dat.small <- dat[,c("age","ed","employ","address","income","debtinc","creddebt","othdebt", "default")]
dat.small$income <- as.double(dat.small$income)
model1 <- glm(default ~ . , family=binomial(link='logit'),data=dat.small)
summary(model1)

# library(ggplot2)

ggplot(dat.small, aes(y = dat.small$default, x = dat.small$age ) + geom_point() +
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE))

install.packages("pscl")
library(pscl)

pR2(model1)

###
quartz(title="bodysize vs. survival") # creates a quartz window with title

plot(bodysize,survive,xlab="Body size",ylab="Probability of survival") # plot with body size on x-axis and survival (0 or 1) on y-axis
g=glm(survive~bodysize,family=binomial,dat) # run a logistic regression model (in this case, generalized linear model with logit link). see ?glm

curve(predict(g,data.frame(bodysize=x),type="resp"),add=TRUE) # draws a curve based on prediction from logistic regression model

points(bodysize,fitted(g),pch=20) # optional: you could skip this draws an invisible set of points of body size survival based on a 'fit' to glm model. pch= changes type of dots.



install.packages("Amelia")
library(Amelia)
missmap(dat.small, main = "Missing values vs observed")







final_model <-glm(dat.small$default ~ age + employ + income + debtinc + creddebt, data = dat.small )
exp(final_model$coefficients)

final_model$fitted.values

a <- c(1, 45 , 1 , 252.23 , 21 , 24)
sum(final_model$coefficients * a )


myFunc <- function(age , employ , income , debtinc , creddebt, cat = 0.5){
  a <- c(1, age , employ , income , debtinc , creddebt)
  ez <- exp(-sum(final_model$coefficients * a ))
  prob <- 1/(1+ez)
  if (prob > cat){
    print("default")
  } else{
    print("Non default")
  }
  return(prob)
}

zz <- myFunc( 45 , 1 , 252.23 , 122 , 12)
print(zz)






