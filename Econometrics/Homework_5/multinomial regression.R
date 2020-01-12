require(foreign)
require(nnet)
require(tidyverse)
require(reshape2)
ml <- read.dta("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")
table(ml$ses, ml$prog)
as_tibble(ml)%>%group_by(ses)%>%summarise(mean_write=mean(write),st_write=sd(write))
ml$prog2 <- relevel(ml$prog, ref = "academic")
test <- multinom(prog2 ~ ses + write, data = ml)
summary(test)
z <- summary(test)$coefficients/summary(test)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
exp(coef(test))
head(pp <- fitted(test))
pp
pp1<-rowRanks(pp)

multinom(cell ~ factor(treat), data=VA)
# multinom(formula = cell ~ factor(treat), data = VA)
VA.tab <- table(VA[, c('cell', 'treat')])
summary(glm(Freq ~ cell * treat, data=VA.tab, family=poisson))


tab <- table(ml[, c('prog2', 'ses', 'write')])
test2 <- glm(Freq ~ ses + write, data = ml)


