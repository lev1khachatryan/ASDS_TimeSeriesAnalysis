rm(list = ls())

install.packages("titanic")
library(titanic)

data.raw = titanic_train
dim(data.raw)
# View(data.raw)

#This data has blanks.  Let's convert the blanks to "NA"
data.raw[data.raw==""]  <- NA 
# View(data.raw)
#Check for missing values using the sapply() function
sapply(data.raw, function(x) sum(is.na(x)))
#if you want to see how may unique values there are for each column:
sapply(data.raw, function(x) length(unique(x)))


# A few basic ratios from the data:
overall_survival_rate = sum(data.raw$Survived == 1)/length(data.raw$Survived)
cat("Fraction of people who survivied = ", format(overall_survival_rate, digits = 3))
male_survival_rate = sum((data.raw$Survived == 1) & (data.raw$Sex == "male"))/sum(data.raw$Sex == "male")
cat("Fraction of men who survivied = ", format(male_survival_rate, digits = 3))
female_survival_rate = sum((data.raw$Survived == 1) & (data.raw$Sex == "female"))/sum(data.raw$Sex == "female")
cat("Fraction of women who survivied = ", format(female_survival_rate, digits = 3))
class1_survival_rate = sum((data.raw$Survived == 1) & (data.raw$Pclass == 1))/sum(data.raw$Pclass == 1)
cat("Fraction of 1st class passengers who survivied = ", format(class1_survival_rate, digits = 3))
class2_survival_rate = sum((data.raw$Survived == 1) & (data.raw$Pclass == 2))/sum(data.raw$Pclass == 2)
cat("Fraction of 2nd class passengers who survivied = ", format(class2_survival_rate, digits = 3))
class3_survival_rate = sum((data.raw$Survived == 1) & (data.raw$Pclass == 3))/sum(data.raw$Pclass == 3)
cat("Fraction of 3rd class passengers who survivied = ", format(class3_survival_rate, digits = 3))

data.T <- na.omit(subset(data.raw,select=c(2,3,5))) #keep only Survived, Pclass, and Sex
table(data.T)
prop.table(table(data.raw$Survived))
prop.table(table(data.raw$Sex))
prop.table(table(data.raw$Pclass))

# Let's drop the columns that we know we are not going to use:
# 1=PassengerId, 4=Name, 9=Ticket, 11=Cabin
data.new <- subset(data.raw,select=c(2,3,5,6,7,8,10,12))
# View(data.new)
data.new$Survived<-as.factor(data.new$Survived)
data.new$Sex<-as.factor(data.new$Sex)
data.new$Pclass<-as.factor(data.new$Pclass)
data.new$Embarked<-as.factor(data.new$Embarked)
levels(data.new$Pclass)
contrasts(data.new$Pclass)

names(data.new)

model <- glm(Survived ~ .,family=binomial(link='logit'),data=data.new)
summary(model)

#use the anova function to compare the addition of each variable
anova(model, test="Chisq")

#create a small data set, containing only the rows and columns that we will use in out model
data.small <- na.omit(subset(data.new,select=c(1,2,3,4,5)))
model1 <- glm(Survived ~ .,family=binomial(link='logit'),data=data.small)
summary(model1)
#confidence intervals for the coefficients
confint(model)  
# We can express each of the coefficeints as an odds ratio:
exp(coef(model1))
# Odds Ratio with its 95% confience interval
exp(cbind(OddsRatio = coef(model), confint(model)))

library(MASS)
model11 <- glm(Survived ~ .,family=binomial(link='logit'),data=na.omit(data.new))
summary(model11)
step <- (stepAIC(model, direction="backward"))
step$anova # display results

