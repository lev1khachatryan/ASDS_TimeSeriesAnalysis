R.version.string
rm(list = ls())

# 1)
dat<-airquality
dat["Temp.C"] <- round((dat["Temp"]-32)*5/9,1)

# 2)
MeanTemp <- mean(dat["Temp.C"][dat["Month"] == 5])

# 3)
dat[which.max(dat$Temp.C),c("Day","Month", "Temp.C")]

# 4)
dat[which(dat["Wind"] > mean(dat[which(dat$Temp.C >= 30),"Wind"])),c("Day","Month", "Temp.C","Wind")]
# second version
as.data.frame( na.omit(dat[dat$Temp.C>30,][dat[dat$Temp.C>30,]$Wind > mean(dat[dat$Temp.C>30,]$Wind),c("Day","Month","Temp.C","Wind") ] ))
