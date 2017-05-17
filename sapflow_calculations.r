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
datDTH <- read.csv("high_density_TDP.csv")
#low density
datDTL <- read.csv("low_density_TDP.csv")

#read in sensor info

datS <- read.csv("sensor_info.csv")

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
####################################################
########get max dT and organize days to
########start at 5am to not allow for night fill

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


plot(datSW$DBH[datSW$stand=="LDF2"],datSW$SWT[datSW$stand=="LDF2"],ylim=c(0,2.1),xlim=c(0,18),
		xlab="DBH", ylab="SWT", col="cornflowerblue",pch=19)
points(datSW$DBH[datSW$stand=="DAV"],datSW$SWT[datSW$stand=="DAV"], pch=19, col="darkgreen")


#fit a linear regression
lmSWL<-lm(datSW$SWT[datSW$stand=="LDF2"]~datSW$DBH[datSW$stand=="LDF2"])
summary(lmSWL)
abline(lmSWL,col="cornflowerblue")


lmSWH<-lm(datSW$SWT[datSW$stand=="DAV"]~datSW$DBH[datSW$stand=="DAV"])
summary(lmSWH)
abline(lmSWH, col="darkgreen")


lmSWall<-lm(datSW$SWT~datSW$DBH)
summary(lmSWall)
abline(lmSWall)



plot(datSW$DBH[datSW$stand=="LDF2"],datSW$Bark[datSW$stand=="LDF2"],ylim=c(0,1),xlim=c(0,18),
		xlab="DBH", ylab="Bark Thick", col="cornflowerblue",pch=19)
points(datSW$DBH[datSW$stand=="DAV"],datSW$Bark[datSW$stand=="DAV"], pch=19, col="darkgreen")


#fit a linear regression
lmBL<-lm(datSW$Bark[datSW$stand=="LDF2"]~datSW$DBH[datSW$stand=="LDF2"])
summary(lmBL)
abline(lmBL,col="cornflowerblue")


lmBH<-lm(datSW$Bark[datSW$stand=="DAV"]~datSW$DBH[datSW$stand=="DAV"])
summary(lmBH)
abline(lmBH, col="darkgreen")

#looks like sapwood thickness varies with stand
#predict the sapwood thickness for the trees that had sensors
datS$SWT<-ifelse(datS$stand=="high", coefficients(lmSWH)[1]+(coefficients(lmSWH)[2]*datS$DBH),
				coefficients(lmSWL)[1]+(coefficients(lmSWL)[2]*datS$DBH))

datS$Bark<-ifelse(datS$stand=="high", coefficients(lmBH)[1]+(coefficients(lmBH)[2]*datS$DBH),
				coefficients(lmBL)[1]+(coefficients(lmBL)[2]*datS$DBH))				
				
#calculate the heartwood 			
datS$Htwd<-datS$DBH-(datS$Bark*2)-(datS$SWT*2)
#calculate sapwood area
datS$sapA<-(pi*(((datS$SWT/2)+(datS$Htwd/2))^2))-(pi*((datS$Htwd/2)^2))			
				
				
#now calculate the porportion of the sensor in sapwood

SensDiff<-datS$Sensor.length-datS$SWT

#if value is negative, it means that the sapwood is thicker than the sensor length
#so it doesn't need to be corrected

#b represents the proption of the probe not in sapwood
datS$b<-ifelse(SensDiff>0,SensDiff/datS$Sensor.length,0)
datS$a<-1-datS$b


datSH<-datS[datS$stand=="high",]
datSL<- datS[datS$stand=="low",]


###################################################
###########corrected dT value with sapwood

#now calcualte a corrected dt value based on sensor length
#note when b=0 and a=1 the dT corrected is equal to the raw dT

dTcorrL<-matrix(rep(NA,dim(dTLraw)[1]*16), ncol=16)
dTcorrH<-matrix(rep(NA,dim(dTHraw)[1]*16), ncol=16)
for(i in 1:16){
	for(j in 1:dim(DaysL)[1]){
		dTcorrL[,i]<-(dTLraw[,i]-(datSL$b[i]*maxDTobsL[,i]))/datSL$a[i]
	}
	for(j in 1:dim(DaysH)[1]){
		dTcorrH[,i]<-(dTHraw[,i]-(datSH$b[i]*maxDTobsH[,i]))/datSH$a[i]
	}
}


#low
for(i in 1:16){
	jpeg(file=paste0("c:\\Users\\hkropp\\Google Drive\\Viper_SF\\plots\\dt_corr\\lowD", i, ".jpeg"), width=1500, height=1000, units="px")
	plot(seq(1:dim(dTcorrL)[1]), dTcorrL[,i], xlab="time", ylab="dT cor", type="b",
			main=paste("sensor #", i), pch=19)
	dev.off()
}
#high
for(i in 1:16){
	jpeg(file=paste0("c:\\Users\\hkropp\\Google Drive\\Viper_SF\\plots\\dt_corr\\highD", i, ".jpeg"), width=1500, height=1000, units="px")
	plot(seq(1:dim(dTcorrH)[1]), dTcorrH[,i], xlab="time", ylab="dT cor", type="b",
			main=paste("sensor #", i), pch=19)
	dev.off()
}

#T diff calculation
KH<-(maxDTobsH-dTcorrH)/dTcorrH
KL<- (maxDTobsL-dTLraw)/dTLraw

#calculate velocity in cm/ s
V.h<-matrix(rep(0,dim(KH)[1]*16), ncol=16)
V.l<-matrix(rep(0,dim(KL)[1]*16), ncol=16)
for(i in 1:16){

	V.h[,i]<-ifelse(KH[,i]>=0,0.0119*(KH[,i]^1.231),NA)
	V.l[,i]<-ifelse(KL[,i]>=0,0.0119*(KL[,i]^1.231),NA)
}


#now calculate in g per s

F.h<-matrix(rep(0,dim(KH)[1]*16), ncol=16)
F.hf<-matrix(rep(0,dim(KH)[1]*16), ncol=16)
F.lf<-matrix(rep(0,dim(KL)[1]*16), ncol=16)
F.l<-matrix(rep(0,dim(KL)[1]*16), ncol=16)
for(i in 1:16){

	F.h[,i]<-V.h[,i]*datSH$sapA[i]
	F.l[,i]<-V.l[,i]*datSL$sapA[i]
	F.hf[,i]<-ifelse(F.h[,i]<200,F.h[,i],NA)
	F.lf[,i]<-ifelse(F.l[,i]<500,F.l[,i],NA)
}

#filter out any spikes




