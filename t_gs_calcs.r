###############################################################################
###########This code is for organizing raw sensor data ########################
###########and converting sensor data to transpiration ########################
###########and canopy stomatal conductance             ########################
###############################################################################

#load libraries
library(lubridate)
library(plyr)

#set wd
setwd("c:\\Users\\hkropp\\Google Drive\\Viper_SF")

#read in data from each stand gradient
datH <- read.table("high_density_TDP.csv", sep=",", head=TRUE, na.strings=c("NAN"))
datL <- read.table("low_density_TDP.csv", sep=",", head=TRUE, na.strings=c("NAN"))

#read in sensor info

datS <- read.csv("sensor_info.csv")


#tower was put up on 6/28/16 and all set up at Davy wrapped up by 6/29/16
#tower at LDF2 6/30 with set up  finishing on 7/1/16

#however, I think both were only fully operational on 7/1/16 after tweaking the Voltage

#the high density stand also cuts out on 8/15/16 because the battery was stolen

# start by converting dates

#low
datL$dateF <- as.Date(datL$TIMESTAMP, "%m/%d/%Y %H:%M")
#high
datH$dateF <- as.Date(datH$TIMESTAMP, "%m/%d/%Y %H:%M")

#now convert to day of year
#low
datL$doy<-yday(datL$dateF)
#high
datH$doy<-yday(datH$dateF)

#now get just the year

#low
datL$year<-year(datL$dateF)
#high
datH$year<-year(datH$dateF)


#now set up matrix with just dt
#low
dTLraw<-datL[,5:20]
#high
dTHraw<-datH[,5:20]

#now set up a a day index for working with data
#make figures to visualize the data 

#low
for(i in 1:16){
	jpeg(file=paste0("c:\\Users\\hkropp\\Google Drive\\Viper_SF\\plots\\raw_dT\\lowD", i, ".jpeg"), width=1500, height=1000, units="px")
	plot(seq(1:dim(dTLraw)[1]), dTLraw[,i], xlab="time", ylab="dT", type="b",
			main=paste("sensor #", i), pch=19)
	dev.off()
}
#high
for(i in 1:16){
	jpeg(file=paste0("c:\\Users\\hkropp\\Google Drive\\Viper_SF\\plots\\raw_dT\\highD", i, ".jpeg"), width=1500, height=1000, units="px")
	plot(seq(1:dim(dTHraw)[1]), dTHraw[,i], xlab="time", ylab="dT", type="b",
			main=paste("sensor #", i), pch=19)
	dev.off()
}

	jpeg(file=paste0("c:\\Users\\hkropp\\Google Drive\\Viper_SF\\plots\\raw_dT\\highDt", 1, ".jpeg"), width=1500, height=1000, units="px")
	plot(seq(1:dim(dTHraw)[1]), dTHraw[,1], xlab="time", ylab="dT", type="b",
			main=paste("sensor #", i), pch=19)
	dev.off()

#get unique


