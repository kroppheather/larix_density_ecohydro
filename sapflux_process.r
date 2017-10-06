###########################################################################
###########################################################################
############## Created by Heather Kropp in October 2017      ##############
############## This script is to be run for all analyses     ##############
############## using transpriation (T) or canopy stomatal    ##############
############## conductance (gc)                              ##############
###########################################################################
###########################################################################
############## Input data files:                             ##############
############## 
###########################################################################
###########################################################################
############## Output data files:                            ##############
library(lubridate)
library(plyr)
library(caTools)

#################################################################
## IMPORTANT: switch to flip on plot diagnostics               ##
## set to 1 to run the code to generate all diagnostic plots   ##
## set to 0 to skip plots if they have already been generated  ##
#################################################################
plotcheck <- 1



#################################################################
####specify directories                                   #######
#################################################################
#directory to save plot checks
diagP <- "c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\sapflux_diag"
#sub folders in diagnostics: maxT, aspectV


#################################################################
####read in datafiles                                     #######
#################################################################

#high density tdp 2016
datDTH <- read.csv("c:\\Users\\hkropp\\Google Drive\\viperSensor\\sapflow\\hd_td_2016.csv")
#low density tdp 2016
datDTL <- read.csv("c:\\Users\\hkropp\\Google Drive\\viperSensor\\sapflow\\ld_td_2016.csv")

#high density tdp 2017
datDTH17 <- read.csv("c:\\Users\\hkropp\\Google Drive\\viperSensor\\sapflow\\hd_td_2017.csv")
#low density tdp 2017
datDTL17 <- read.csv("c:\\Users\\hkropp\\Google Drive\\viperSensor\\sapflow\\ld_td_2017.csv")

#subset to pull out sensor values
#sensors 9-16 were run at the incorrect voltage
#and clearly have incorrect dT values for it
#so excluding from calculations
datH <- datDTH[,6:13]
datL <- datDTL[,6:21]
datH17 <- datDTH17[,6:21]
datL17 <- datDTL17[,6:21]
#need to create new doy index that defines day of year between 5am and 5am
#for defining intervals that would count refil as in the same day
datL$doy5 <- ifelse(datDTL$hour<5,datDTL$doy-1,datDTL$doy)
datH$doy5 <- ifelse(datDTH$hour<5,datDTH$doy-1,datDTH$doy)
datL17$doy5 <- ifelse(datDTL17$hour<5,datDTL17$doy-1,datDTL17$doy)
datH17$doy5 <- ifelse(datDTH17$hour<5,datDTH17$doy-1,datDTH17$doy)


#read in sensor info
datS <- read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\individual_data\\sensor_info.csv")

#read in 2017 sensor info
datS17<-read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\individual_data\\sensor_info17.csv")

##### read in sapwood thickness or sensor correciton
datSW <- read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\individual_data\\sap_thick.csv")


#################################################################
####calculate maxdT                                       #######
#################################################################

#create an index that indicates nighttime or daytime
#to check how maximums are comparing

#add time
datH$hour <- datDTH[,3]
datL$hour <- datDTL[,3]
datH17$hour <- datDTH17[,3]
datL17$hour <- datDTL17[,3]
#get maximum for each sesnor
LmaxTemp <- list()
LmaxDTA <- list()
LmaxTemp17 <- list()
LmaxDTA17 <- list()
HmaxTemp17 <- list()
HmaxDTA17 <- list()
for(i in 1:16){
	#ld 2016
	LmaxTemp[[i]]<- na.omit(data.frame(dT=datL[,i],doy5=datL$doy5))
	LmaxDTA[[i]] <- aggregate(LmaxTemp[[i]]$dT, by=list(LmaxTemp[[i]]$doy5), FUN="max")
	colnames(LmaxDTA[[i]]) <- c("doy5",  paste0("mdT",i))
	#ld 2017
	LmaxTemp17[[i]]<- na.omit(data.frame(dT=datL17[,i],doy5=datL17$doy5))
	LmaxDTA17[[i]] <- aggregate(LmaxTemp17[[i]]$dT, by=list(LmaxTemp17[[i]]$doy5), FUN="max")
	colnames(LmaxDTA17[[i]]) <- c("doy5",  paste0("mdT",i))
	#hd 2017
	HmaxTemp17[[i]]<- na.omit(data.frame(dT=datH17[,i],doy5=datH17$doy5))
	HmaxDTA17[[i]] <- aggregate(HmaxTemp17[[i]]$dT, by=list(HmaxTemp17[[i]]$doy5), FUN="max")
	colnames(HmaxDTA17[[i]]) <- c("doy5",  paste0("mdT",i))	
	}
#hd 2016
#get maximum for each sesnor
HmaxTemp <- list()
HmaxDTA <- list()
for(i in 1:8){	
	HmaxTemp[[i]]<- na.omit(data.frame(dT=datH[,i],doy5=datH$doy5))
	HmaxDTA[[i]] <- aggregate(HmaxTemp[[i]]$dT, by=list(HmaxTemp[[i]]$doy5), FUN="max")
	colnames(HmaxDTA[[i]]) <- c("doy5",  paste0("mdT",i))	
}
	
#join the daily maximums back into a dataframe
LmaxDTA2 <- join_all(LmaxDTA, by="doy5", type="full")	
LmaxDTA172 <- join_all(LmaxDTA17, by="doy5", type="full")
HmaxDTA2 <- join_all(HmaxDTA, by="doy5", type="full")
HmaxDTA172 <- join_all(HmaxDTA17, by="doy5", type="full")	
	
#diagnostic plots to see what the maximum temp looks like
if(plotcheck==1){
	#ld 2016
	for(i in 1:16){
	jpeg(file=paste0(diagP, "\\maxT\\LD_2016_sensor",i,".jpg"), width=1500, height=1000, units="px")
		plot(LmaxDTA[[i]]$doy5,LmaxDTA[[i]]$mdT,
				xlim=c(min(LmaxDTA[[i]]$doy5)-.5,max(LmaxDTA[[i]]$doy5)+.5),
				ylim=c(0,15),
				xlab="Day of year", ylab="dT (C)", main=paste("sensor",i),
				pch=19, cex=1.5)

	dev.off()
	}
	#ld 2017
	for(i in 1:16){
	jpeg(file=paste0(diagP, "\\maxT\\LD_2017_sensor",i,".jpg"), width=1500, height=1000, units="px")
		plot(LmaxDTA17[[i]]$doy5,LmaxDTA17[[i]]$mdT,
				xlim=c(min(LmaxDTA17[[i]]$doy5)-.5,max(LmaxDTA17[[i]]$doy5)+.5),
				ylim=c(0,15),
				xlab="Day of year", ylab="dT (C)", main=paste("sensor",i),
				pch=19, cex=1.5)

	dev.off()
	}
	
	#hd 2016
	for(i in 1:8){
	jpeg(file=paste0(diagP, "\\maxT\\HD_2016_sensor",i,".jpg"), width=1500, height=1000, units="px")
		plot(HmaxDTA[[i]]$doy5,HmaxDTA[[i]]$mdT,
				xlim=c(min(HmaxDTA[[i]]$doy5)-.5,max(HmaxDTA[[i]]$doy5)+.5),
				ylim=c(0,15),
				xlab="Day of year", ylab="dT (C)", main=paste("sensor",i),
				pch=19, cex=1.5)

	dev.off()
	}
	#hd 2017
	for(i in 1:16){
	jpeg(file=paste0(diagP, "\\maxT\\HD_2017_sensor",i,".jpg"), width=1500, height=1000, units="px")
		plot(HmaxDTA17[[i]]$doy5,HmaxDTA17[[i]]$mdT,
				xlim=c(min(HmaxDTA17[[i]]$doy5)-.5,max(HmaxDTA17[[i]]$doy5)+.5),
				ylim=c(0,15),
				xlab="Day of year", ylab="dT (C)", main=paste("sensor",i),
				pch=19, cex=1.5)

	dev.off()
	}
}

#join the daily maximum back into the full sapflox data frame


datAH <- join(datH, HmaxDTA2, by="doy5", type="left")
datAH17 <- join(datH17, HmaxDTA172, by="doy5", type="left")
datAL <- join(datL, LmaxDTA2, by="doy5", type="left")
datAL17 <- join(datL17, LmaxDTA172, by="doy5", type="left")

#################################################################
####calculate sapwood thicknes                            #######
#################################################################

#fit a linear regression for sap thickness
#low
lmSWL <- lm(datSW$SWT[datSW$stand=="LDF2"]~datSW$DBH[datSW$stand=="LDF2"])
summary(lmSWL)
#high
lmSWH <- lm(datSW$SWT[datSW$stand=="DAV"]~datSW$DBH[datSW$stand=="DAV"])
summary(lmSWH)
#fit a linear regression for bark thickness
#low
lmBL <- lm(datSW$Bark[datSW$stand=="LDF2"]~datSW$DBH[datSW$stand=="LDF2"])
summary(lmBL)
#high
lmBH <- lm(datSW$Bark[datSW$stand=="DAV"]~datSW$DBH[datSW$stand=="DAV"])
summary(lmBH)
#looks like sapwood thickness varies with stand
#predict the sapwood thickness for the trees that had sensors
datS$SWT <- ifelse(datS$stand=="high", coefficients(lmSWH)[1]+(coefficients(lmSWH)[2]*datS$DBH),
				coefficients(lmSWL)[1]+(coefficients(lmSWL)[2]*datS$DBH))

datS$Bark <- ifelse(datS$stand=="high", coefficients(lmBH)[1]+(coefficients(lmBH)[2]*datS$DBH),
				coefficients(lmBL)[1]+(coefficients(lmBL)[2]*datS$DBH))		

datS17$SWT <- ifelse(datS17$stand=="hd", coefficients(lmSWH)[1]+(coefficients(lmSWH)[2]*datS17$DBH..cm.),
				coefficients(lmSWL)[1]+(coefficients(lmSWL)[2]*datS17$DBH..cm.))

datS17$Bark <- ifelse(datS$stand=="hd", coefficients(lmBH)[1]+(coefficients(lmBH)[2]*datS17$DBH..cm.),
				coefficients(lmBL)[1]+(coefficients(lmBL)[2]*datS17$DBH..cm.))		

				
#calculate the heartwood 			
datS$Htwd <- datS$DBH-(datS$Bark*2)-(datS$SWT*2)

datS17$Htwd <- datS17$DBH-(datS17$Bark*2)-(datS17$SWT*2)
#calculate sapwood area
datS$sapA <- (pi*(((datS$SWT/2)+(datS$Htwd/2))^2))-(pi*((datS$Htwd/2)^2))

datS17$sapA <- (pi*(((datS17$SWT/2)+(datS17$Htwd/2))^2))-(pi*((datS17$Htwd/2)^2))		
								
#now calculate the porportion of the sensor in sapwood
SensDiff <- datS$Sensor.length-datS$SWT
	
SensDiff17 <- datS17$Sensor.length-datS17$SWT	
#if value is negative, it means that the sapwood is thicker than the sensor length
#so it doesn't need to be corrected

#b represents the proption of the probe not in sapwood
datS$b <- ifelse(SensDiff>0,SensDiff/datS$Sensor.length,0)
datS$a <- 1-datS$b

datS17$b <- ifelse(SensDiff17>0,SensDiff17/datS17$Sensor.length,0)
datS17$a <- 1-datS17$b
#seperate df
datSH <- datS[datS$stand=="high",]
datSL <- datS[datS$stand=="low",]

datSH17 <- datS17[datS17$stand=="hd",]
datSL17 <- datS17[datS17$stand=="ld",]
#only sensors 1-8 were at the right voltage
datSH <- datSH[1:8,]

#################################################################
####correct dT for  sensors                               #######
#####with smaller sapwood thickness than sensor           #######
#################################################################
#now calcualte a corrected dt value based on sensor length
#note when b=0 and a=1 the dT corrected is equal to the raw dT
dTcorrL<-matrix(rep(NA,dim(datAL)[1]*16), ncol=16)
dTcorrH<-matrix(rep(NA,dim(datAH)[1]*8), ncol=8)
dTcorrL17<-matrix(rep(NA,dim(datAL17)[1]*16), ncol=16)
dTcorrH17<-matrix(rep(NA,dim(datAH17)[1]*16), ncol=16)
for(i in 1:16){
		dTcorrL[,i] <- (datAL[,i]-(datSL$b[i]*datAL[,18+i]))/datSL$a[i]	
		dTcorrL17[,i] <- (datAL17[,i]-(datSL17$b[i]*datAL17[,18+i]))/datSL17$a[i]	
		dTcorrH17[,i]<- (datAH17[,i]-(datSH17$b[i]*datAH17[,18+i]))/datSH17$a[i]	
		}
for(i in 1:8){

		dTcorrH[,i]<- (datAH[,i]-(datSH$b[i]*datAH[,10+i]))/datSH$a[i]	
}

#################################################################
##### velocity calculations                               #######
#################################################################
#T diff calculation
#Tmax-dtcor/dtcor
KL<-matrix(rep(NA,dim(datAL)[1]*16), ncol=16)
KH<-matrix(rep(NA,dim(datAH)[1]*8), ncol=8)
KL17<-matrix(rep(NA,dim(datAL17)[1]*16), ncol=16)
KH17<-matrix(rep(NA,dim(datAH17)[1]*16), ncol=16)
#low
for(i in 1:16){
		KL[,i] <- (datAL[,18+i]-dTcorrL[,i])/dTcorrL[,i]	
		KL17[,i] <- (datAL17[,18+i]-dTcorrL17[,i])/dTcorrL17[,i]
		KH17[,i] <- (datAH17[,18+i]-dTcorrH17[,i])/dTcorrH17[,i]			
}
#high
for(i in 1:8){
		KH[,i] <- (datAH[,10+i]-dTcorrH[,i])/dTcorrH[,i]	
}
#calculate velocity in cm/ s
V.h<-matrix(rep(0,dim(KH)[1]*8), ncol=8)
V.l<-matrix(rep(0,dim(KL)[1]*16), ncol=16)
V.h17<-matrix(rep(0,dim(KH17)[1]*16), ncol=16)
V.l17<-matrix(rep(0,dim(KL17)[1]*16), ncol=16)
#high
for(i in 1:8){
	V.h[,i] <- ifelse(KH[,i]>=0,0.0119*(KH[,i]^1.231),NA)

}
#low	
for(i in 1:16){
	V.l[,i] <- ifelse(KL[,i]>=0,0.0119*(KL[,i]^1.231),NA)
	V.h17[,i] <- ifelse(KH17[,i]>=0,0.0119*(KH17[,i]^1.231),NA)
	V.l17[,i] <- ifelse(KL17[,i]>=0,0.0119*(KL17[,i]^1.231),NA)
}

	#turn sensor 1 in low 17 to NA until day 170 because the sensor
	#was switched and that is the info used 
	
	V.l17[,1]<-ifelse(datAL17$doy5<170,NA,V.l17[,1])
	
#################################################################
##### compare aspect velocities to see if need to         #######
##### do an aspect correction                             #######
#################################################################	

#paired sensor numbers
LNorthS<-c(5,8,11)
LSouthS<-c(6,9,12)
LNorth17S<-c(5,11,15)
LSouth17S<-c(1,12,16)
HNorthS<-c(3)
HSouthS<-c(4)
HNorth17S<-c(3,5,11)
HSouth17S<-c(4,6,12)

#pull out sensor comparisions

LNorth<-c(as.vector(V.l[,LNorthS]),as.vector(V.l17[,LNorth17S]))
LSouth<-c(as.vector(V.l[,LSouthS]),as.vector(V.l17[,LSouth17S]))

HNorth<-c(as.vector(V.h[,3]),as.vector(V.h17[,HNorth17S]))
HSouth<-c(as.vector(V.h[,4]),as.vector(V.h17[,HSouth17S]))

LNorth1<-c(as.vector(V.l[,5]),as.vector(V.l17[,5]))
LSouth1<-c(as.vector(V.l[,6]),as.vector(V.l17[,1]))
LNorth2<-c(as.vector(V.l[,8]),as.vector(V.l17[,11]))
LSouth2<-c(as.vector(V.l[,9]),as.vector(V.l17[,12]))
LNorth3<-c(as.vector(V.l[,11]),as.vector(V.l17[,15]))
LSouth3<-c(as.vector(V.l[,12]),as.vector(V.l17[,16]))

HNorth1<-c(as.vector(V.h[,3]),as.vector(V.h17[,3]))
HSouth1<-c(as.vector(V.h[,4]),as.vector(V.h17[,4]))
HNorth2<-c(as.vector(V.h17[,5]))
HSouth2<-c(as.vector(V.h17[,6]))
HNorth3<-c(as.vector(V.h17[,11]))
HSouth3<-c(as.vector(V.h17[,12]))


#filter out extreme data spikes in comparisions
LNorth<-ifelse(LNorth>quantile(LNorth, probs=.9, na.rm=TRUE),NA,LNorth)
LSouth<-ifelse(LSouth>quantile(LSouth, probs=.9, na.rm=TRUE),NA,LSouth)

LNorth1<-ifelse(LNorth1>quantile(LNorth1, probs=.9, na.rm=TRUE),NA,LNorth1)
LSouth1<-ifelse(LSouth1>quantile(LSouth1, probs=.9, na.rm=TRUE),NA,LSouth1)
LNorth2<-ifelse(LNorth2>quantile(LNorth2, probs=.9, na.rm=TRUE),NA,LNorth2)
LSouth2<-ifelse(LSouth2>quantile(LSouth2, probs=.9, na.rm=TRUE),NA,LSouth2)
LNorth3<-ifelse(LNorth3>quantile(LNorth3, probs=.9, na.rm=TRUE),NA,LNorth3)
LSouth3<-ifelse(LSouth3>quantile(LSouth3, probs=.9, na.rm=TRUE),NA,LSouth3)

##high density
##filter extreme values

HNorth<-ifelse(HNorth>quantile(HNorth, probs=.9, na.rm=TRUE),NA,HNorth)
HSouth<-ifelse(HSouth>quantile(HSouth, probs=.9, na.rm=TRUE),NA,HSouth)

HNorth1<-ifelse(HNorth1>quantile(HNorth1, probs=.9, na.rm=TRUE),NA,HNorth1)
HSouth1<-ifelse(HSouth1>quantile(HSouth1, probs=.9, na.rm=TRUE),NA,HSouth1)

HNorth2<-ifelse(HNorth2>quantile(HNorth2, probs=.9, na.rm=TRUE),NA,HNorth2)
HSouth2<-ifelse(HSouth2>quantile(HSouth2, probs=.9, na.rm=TRUE),NA,HSouth2)

HNorth3<-ifelse(HNorth3>quantile(HNorth3, probs=.9, na.rm=TRUE),NA,HNorth3)
HSouth3<-ifelse(HSouth3>quantile(HSouth3, probs=.9, na.rm=TRUE),NA,HSouth3)

AspNH<-lm(HSouth~HNorth)
summary(AspNH)

AspNL<-lm(LSouth~LNorth)
summary(AspNL)



AspNL1<-lm(LSouth1~LNorth1)

AspNL2<-lm(LSouth2~LNorth2)

AspNL3<-lm(LSouth3~LNorth3)


AspNH1<-lm(HSouth1~HNorth1)


AspNH2<-lm(HSouth2~HNorth2)

AspNH3<-lm(HSouth3~HNorth3)

if(plotcheck==1){
#check all trees together in each stand
jpeg(file=paste0(diagP, "\\aspectV\\standcomp.jpg"), width=1500, height=1000, units="px")
par(mfrow=c(1,2))
plot(HNorth,HSouth,xlab="North V cm/s", ylab="south V cm/s", pch=19, main="high")
text( .002,.0045, paste("South=",round(AspNH$coefficients[1],2),"+ ",round(AspNH$coefficients[2],2),"*North" ), col="red", cex=2 )
text(.001,.004, paste("R.squared=",round(summary(AspNH)$r.squared,2)), col="red", cex=2)
plot(LNorth,LSouth, ,xlab="North V cm/s", ylab="south V cm/s", pch=19, main="low")
text( .002,.0045, paste("South=",round(AspNL$coefficients[1],2),"+ ",round(AspNL$coefficients[2],2),"*North" ), col="red", cex=2 )
text(.001,.004, paste("R.squared=",round(summary(AspNL)$r.squared,2)), col="red", cex=2)


dev.off()

##check low density individual trees
jpeg(file=paste0(diagP, "\\aspectV\\lowInd.jpg"), width=1500, height=1000, units="px")
par(mfrow=c(1,3))
plot(LNorth1,LSouth1,xlab="North V cm/s", ylab="south V cm/s", pch=19, main="low")
text( .002,.003, paste("South=",round(AspNL1$coefficients[1],2),"+ ",round(AspNL1$coefficients[2],2),"*North" ), col="red", cex=2 )
text(.001,.0025, paste("R.squared=",round(summary(AspNL1)$r.squared,2)), col="red", cex=2)
plot(LNorth2,LSouth2,xlab="North V cm/s", ylab="south V cm/s", pch=19, main="low")
text( .002,.0045, paste("South=",round(AspNL2$coefficients[1],2),"+ ",round(AspNL2$coefficients[2],2),"*North" ), col="red", cex=2 )
text(.001,.004, paste("R.squared=",round(summary(AspNL2)$r.squared,2)), col="red", cex=2)
plot(LNorth3,LSouth3,xlab="North V cm/s", ylab="south V cm/s", pch=19, main="low")
text( .002,.0045, paste("South=",round(AspNL3$coefficients[1],2),"+ ",round(AspNL3$coefficients[2],2),"*North" ), col="red", cex=2 )
text(.001,.004, paste("R.squared=",round(summary(AspNL3)$r.squared,2)), col="red", cex=2)
dev.off()
##check high density individual trees
jpeg(file=paste0(diagP, "\\aspectV\\highInd.jpg"), width=1500, height=1000, units="px")
par(mfrow=c(1,3))
plot(HNorth1,HSouth1,xlab="North V cm/s", ylab="south V cm/s", pch=19, main="high")
text( .002,.0045, paste("South=",round(AspNH1$coefficients[1],2),"+ ",round(AspNH1$coefficients[2],2),"*North" ), col="red", cex=2 )
text(.001,.004, paste("R.squared=",round(summary(AspNH1)$r.squared,2)), col="red", cex=2)
plot(HNorth2,HSouth2,xlab="North V cm/s", ylab="south V cm/s", pch=19, main="high")
text( .002,.0045, paste("South=",round(AspNH2$coefficients[1],2),"+ ",round(AspNH2$coefficients[2],2),"*North" ), col="red", cex=2 )
text(.001,.004, paste("R.squared=",round(summary(AspNH2)$r.squared,2)), col="red", cex=2)
plot(HNorth3,HSouth3,xlab="North V cm/s", ylab="south V cm/s", pch=19, main="high")
text( .0005,.0025, paste("South=",round(AspNH3$coefficients[1],2),"+ ",round(AspNH3$coefficients[2],2),"*North" ), col="red", cex=2 )
text(.0003,.002, paste("R.squared=",round(summary(AspNH3)$r.squared,2)), col="red", cex=2)
dev.off()

}

#################################################################
###### calculate flow in g/s                    #################
#################################################################

