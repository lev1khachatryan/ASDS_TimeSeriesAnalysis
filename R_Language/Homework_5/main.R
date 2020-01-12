## "R version 3.5.1 (2018-07-02)"
rm(list = ls())
dat <- airquality
dat[dat[,"Temp"] <= 86 & dat[,"Temp"] >= 68,"Category" ] <- "Normal"
dat[dat[,"Temp"] < 68,"Category" ] <- "Cold"
dat[dat[,"Temp"] > 86,"Category" ] <- "Hot"
###1###
x <- list(dat[dat$Category == "Cold","Ozone"],dat[dat$Category == "Normal","Ozone"],dat[dat$Category == "Hot","Ozone"])
lapply(x, function(c){(min(c, na.rm = TRUE) + max(c, na.rm = TRUE))/2})
###2###
sapply(split(dat[, "Ozone"], list(dat$Month, dat$Category)),mean, na.rm=TRUE)
###3###
sapply(dat, class)
lapply(dat,function (vect) ifelse(class(vect) == "numeric" | class(vect) == "integer" , sort(vect)[2], "it isn't numeric vector"))
