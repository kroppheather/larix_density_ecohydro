#############################################################
############reads in dT data from TDP sensors ###############
############and calculates sap flow and gc    ###############
#############################################################
library(lubridate)
library(plyr)

setwd("c:\\Users\\hkropp\\Google Drive\\Viper_SF")

#read in TDP 
#not these files are truncated to start at 5am
#on first day of a full data measurement with no
#start up data included in the first day of data
#high density
datDTH<-read.csv("high_density_TDP.csv")
#low density
datDTL<-read.csv("low_density_TDP.csv")

#sensors 9-16 were run at the incorrect voltage
#and clearly have incorrect dT values for it
#so excluding from calculations
datH<-datDTH[,5:(4+8)]
datL<-datDTL[,5:dim(datDTL)[2]]

#set up date
dateDH<-as.Date(datDTH$TIMESTAMP, "%m/%d/%Y %H:%M")
dateDL<-as.Date(datDTL$TIMESTAMP, "%m/%d/%Y %H:%M")

doyDH<-yday(dateDH)
doyDL<-yday(dateDL)

#convert time
timeDH<-ifelse(datDTH$JHM/100-floor(datDTH$JHM/100)==0,datDTH$JHM/100,floor(datDTH$JHM/100)+.5)
timeDL<-ifelse(datDTL$JHM/100-floor(datDTL$JHM/100)==0,datDTL$JHM/100,floor(datDTL$JHM/100)+.5)

####### make plots of the raw dT values
#low
for(i in 1:16){
	jpeg(file=paste0("c:\\Users\\hkropp\\Google Drive\\Viper_SF\\plots\\raw_dT\\Low\\lowdT", i, ".jpeg"), width=1500, height=1000, units="px")
	plot(seq(1:dim(datL)[1]), datL[,i], xlab="time", ylab="dT", type="b",
			main=paste("sensor #", i), pch=19)
	dev.off()
}
#high
for(i in 1:8){
	jpeg(file=paste0("c:\\Users\\hkropp\\Google Drive\\Viper_SF\\plots\\raw_dT\\High\\highdT", i, ".jpeg"), width=1500, height=1000, units="px")
	plot(seq(1:dim(datH)[1]), datH[,i], xlab="time", ylab="dT", type="b",
			main=paste("sensor #", i), pch=19)
	dev.off()
}

####################################################
#now start working on the data calculations

#need to create new doy index that defines day of year between 5am and 5am
datL$doy<-ifelse(timeDL<5,doyDL-1,doyDL)
datH$doy<-ifelse(timeDH<5,doyDH-1,doyDH)

#get the unique dayid for each stand
#get the number of observations in a day for each stand
DaysL<-aggregate(datL$doy, by=list(datL$doy), FUN="length")
colnames(DaysL)<-c("doy", "nobs")
DaysL$dayid<-seq(1,dim(DaysL)[1])

DaysH<-aggregate(datH$doy, by=list(datH$doy), FUN="length")
colnames(DaysH)<-c("doy", "nobs")
DaysH$dayid<- seq(1, dim(DaysH)[1])	


###########
####calculate the max dT in a day
#low density
maxDTL<-matrix(rep(0,dim(DaysL)[1]*16),ncol=16)
#output matrix of the max per each day
for(j in 1:16){
	for(i in 1:dim(DaysL)[1]){
		maxDTL[i,j]<-max(na.omit(datL[datL$doy==DaysL$doy[i],j]))
	}
}
#calcuate high density
maxDTH<-matrix(rep(0,dim(DaysH)[1]*8),ncol=8)

for(j in 1:8){
	for(i in 1:dim(DaysH)[1]){
		maxDTH[i,j]<-max(na.omit(datH[datH$doy==DaysH$doy[i],j]))
	}
}
maxDTL<-as.data.frame(maxDTL)
maxDTL$doy<-DaysL$doy

maxDTH<-as.data.frame(maxDTH)
maxDTH$doy<-DaysH$doy

# now combine both
datAH<-join(datH,maxDTH, by=c("doy"), type="left")
datAL<-join(datL,maxDTL, by=c("doy"), type="left")




###########
##### read in sapwood thickness or sensor correciton

datSW<-read.csv("sap_thick.csv")





