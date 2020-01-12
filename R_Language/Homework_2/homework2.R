## 1
R.version.string
rm(list = ls())
Continents<-c("Europe", "Australia", "Asia",
              "America", "Africa", "Antarctica")
x<-c(USA=120, Europe=320, Australia=360, Japan=450,
     Asia=680,America=200, France=80,Armenia=600,Africa=250)

z <- x[Continents][which(x[Continents] <=350)] + 0.5*x[Continents][which(x[Continents] <=350)]
x[Continents][which(x[Continents] <=350)]<- x[Continents][which(x[Continents] <=350)] + 0.5*x[Continents][which(x[Continents] <=350)]

## 2
set.seed(1)
n<-100
X<-c(sample(c(NaN,NA,1),replace=TRUE, size=n),rep("a",n))

#Number of NAs
sum(is.na(X))  ##38

#Number of NaNs
sum(!is.na(X[X == "NaN"]))  ##27
