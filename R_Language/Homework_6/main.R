setwd("D:\\MyProjects\\R_Homeworks\\Homework_6")
R.version.string
##"R version 3.5.1 (2018-07-02)"
rm(list = ls())
###########
#### 1 ####
###########
dat1 <- read.csv("WB_Growth.csv", stringsAsFactors = FALSE, skip=4)
dat2 <- read.csv("WB_Regions.csv", stringsAsFactors = FALSE)
dat2$SpecialNotes <- NULL
dat2[c("Region")][is.na(dat2[c("Region")]) | dat2[c("Region")] == ""]  <- "Unknown"
###########
#### 2 ####
###########
dim.data.frame(dat1)
dim.data.frame(dat2)
dat1 <- dat1[dat1$Country.Code != "WLD",]
###########
#### 3 ####
###########
AllNA <- function(vect){
  return(all(is.na(vect)))
}
dat1 <- dat1[,!sapply(dat1, AllNA)]
dat2 <- dat2[,!sapply(dat2, AllNA)]
###########
#### 4 ####
###########
dim.data.frame(dat1)
dim.data.frame(dat2)
dat <- cbind(dat1,dat2) 
dim.data.frame(dat)
###########
#### 5 ####
###########
# tapply(X = dat$Country.Name, dat[,startsWith(x = names(dat),prefix = 'X')] , rowMeans(na.rm = TRUE))
dat["Growth"] <- rowMeans(dat[,startsWith(x = names(dat),prefix = 'X')], na.rm = TRUE)
###########
#### 7 ####
###########
dat <- dat[!is.na(dat$Growth),]
dim(dat)
###########
#### 8 ####
###########
dat <- dat[order(dat$Growth, decreasing = FALSE),]
rownames(dat) <- NULL
## or 
# rownames(dat) <- 1:254
###########
#### 9 ####
###########
rownames(dat[dat$Country.Name=="Armenia",])
##############
##### 10 #####
##############
## First solution ##
library(stringr)
dat$number.of.a <- str_count(tolower(dat$Country.Name), "a")
dat[,c("Country.Name","number.of.a")]

## Second solution ##
NumAs <- function(strg){
  ifelse (gregexpr( "A", strg,ignore.case = T)[[1]][1] != -1 ,length(gregexpr( "A", strg,ignore.case = T)[[1]]), 0 )
}
dat$numA <- sapply(dat$Country.Name, NumAs)
## Checking
dat[dat$numA != dat$number.of.a , c("numA", "number.of.a")]
