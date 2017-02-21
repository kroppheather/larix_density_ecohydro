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
datH <- read.table("Dav_sub.csv", sep=",", head=TRUE, na.strings=c("NAN"))
datL <- read.table("ldf_sub.csv", sep=",", head=TRUE, na.strings=c("NAN"))

#exclude first few measurements becasue many look like the the sensors hadn't
#fully equilibriated after set up
datH<-datH[3:dim(datH)[1],]
datL<-datL[3:dim(datL)[1],]

#read in sensor info

datS <- read.csv("sensor_info.csv")


#tower was put up on 6/28/16 and all set up at Davy wrapped up by 6/29/16
#tower at LDF2 6/30 with set up  finishing on 7/1/16

#however, I think both were only fully operational on 7/1/16 after tweaking the Voltage

#the high density stand also cuts out on 8/15/16 because the battery was stolen

# start by converting dates

#low
#datL$dateF <- as.Date(datL$TIMESTAMP, "%m/%d/%Y %H:%M")
#high
#datH$dateF <- as.Date(datH$TIMESTAMP, "%m/%d/%Y %H:%M")


#convert time by campbell to more continous output

datL$hour<-ifelse(datL$JHM/100-floor(datL$JHM/100)==0,datL$JHM/100,floor(datL$JHM/100)+.5)
datH$hour<-ifelse(datH$JHM/100-floor(datH$JHM/100)==0,datH$JHM/100,floor(datH$JHM/100)+.5)
#now convert to day of year
#low
datL$doy<-datL$doy-1
#high one day ahead
datH$doy<-datH$doy-1

#need to create new doy index that defines day of year between 5am and 5am
datL$doy5<-ifelse(datL$hour<=5,datL$doy-1,datL$doy)
datH$doy5<-ifelse(datH$hour<=5,datH$doy-1,datH$doy)

#now get just the year

#low
#datL$year<-year(datL$dateF)
#high
#datH$year<-year(datH$dateF)


#redefine the day of year so that it 


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
	


#get the unique dayid for each stand
#get the number of observations in a day for each stand
DaysL<-aggregate(datL$doy5, by=list(datL$doy5), FUN="length")
colnames(DaysL)<-c("doy", "nobs")
DaysL$dayid<-seq(1,dim(DaysL)[1])

DaysH<-aggregate(datH$doy5, by=list(datH$doy5), FUN="length")
colnames(DaysH)<-c("doy", "nobs")
DaysH$dayid<- seq(1, dim(DaysH)[1])	

#calculate maximum dT
#omit na from each sensor observaion


maxDTL<-matrix(rep(0,dim(DaysL)[1]*16),ncol=16)

for(j in 1:16){
	for(i in 1:dim(DaysL)[1]){
		maxDTL[i,j]<-max(na.omit(dTLraw[DaysL$dayid==i,j]))
	}
}
#calcuate high density
maxDTH<-matrix(rep(0,dim(DaysH)[1]*16),ncol=16)

for(j in 1:16){
	for(i in 1:dim(DaysH)[1]){
		maxDTH[i,j]<-max(na.omit(dTHraw[DaysH$dayid==i,j]))
	}
}

maxDTobsH<-matrix(rep(NA,dim(dTHraw)[1]*16), ncol=16)
maxDTobsL<-matrix(rep(NA,dim(dTLraw)[1]*16), ncol=16)
#set up a max dT matrix to match each dT obs
for(i in 1:16){
	maxDTobsH[,i]<-rep(maxDTH[,i], times=DaysH$nobs)
	maxDTobsL[,i]<-rep(maxDTL[,i], times=DaysL$nobs)
}

#now read in sapwood thickness or sensor correciton

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
		xlab="DBH", ylab="SWT", col="cornflowerblue",pch=19)
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


#now set up a a day index for working with data
#make figures to visualize the data 

#low
for(i in 1:16){
	jpeg(file=paste0("c:\\Users\\hkropp\\Google Drive\\Viper_SF\\plots\\flow\\lowD", i, ".jpeg"), width=1500, height=1000, units="px")
	plot(seq(1:dim(F.lf)[1]), F.lf[,i], xlab="time", ylab="Flow (g/hr)", type="b",
			main=paste("sensor #", i), pch=19)
	dev.off()
}
#high
for(i in 1:16){
	jpeg(file=paste0("c:\\Users\\hkropp\\Google Drive\\Viper_SF\\plots\\flow\\highD", i, ".jpeg"), width=1500, height=1000, units="px")
	plot(seq(1:dim(F.hf)[1]), F.hf[,i], xlab="time", ylab="Flow (g/hr)", type="b",
			main=paste("sensor #", i), pch=19)
	dev.off()
}

#now normalize by leaf area
leaf.bio<-function(DBH,a.leaf,b.leaf){a.leaf*(DBH^b.leaf)}
datSH$leafwt<-leaf.bio(datSH$DBH,40.5,1.41)	
datSH$leaf<-datSH$leafwt*143
datSH$leafm2<-datSH$leaf*.0001


datSL$leafwt<-leaf.bio(datSL$DBH,40.5,1.41)	
datSL$leaf<-datSL$leafwt*143
datSL$leafm2<-datSL$leaf*.0001

#now calculate in g m-2 s
T.gL<-matrix(rep(0,dim(KL)[1]*16), ncol=16)
T.gH<-matrix(rep(0,dim(KH)[1]*16), ncol=16)
T.gLf<-matrix(rep(0,dim(KL)[1]*16), ncol=16)
T.gHf<-matrix(rep(0,dim(KH)[1]*16), ncol=16)
for(i in 1:16){
	T.gL[,i]<-F.lf[,i]/datSL$leafm2[i]
	T.gH[,i]<-F.hf[,i]/datSH$leafm2[i]
	
	T.gLf[,i]<-ifelse(T.gL[,i]<.01,T.gL[,i],NA)
	T.gHf[,i]<-ifelse(T.gH[,i]<.01,T.gH[,i],NA)
}

#low
for(i in 1:16){
	jpeg(file=paste0("c:\\Users\\hkropp\\Google Drive\\Viper_SF\\plots\\Tg\\lowD", i, ".jpeg"), width=1500, height=1000, units="px")
	plot(seq(1:dim(T.gL)[1]), T.gLf[,i], xlab="time", ylab="Flow (g/ m2 s)", type="b",
			main=paste("sensor #", i), pch=19)
	dev.off()
}
#high
for(i in 1:16){
	jpeg(file=paste0("c:\\Users\\hkropp\\Google Drive\\Viper_SF\\plots\\Tg\\highD", i, ".jpeg"), width=1500, height=1000, units="px")
	plot(seq(1:dim(T.gH)[1]), T.gHf[,i], xlab="time", ylab="Flow (g/ m2 s)", type="b",
			main=paste("sensor #", i), pch=19)
	dev.off()
}


#now combine back with original part of data frame
#and convert ot kg m-2 s-1 
datLc1<-data.frame(doy=datL$doy,hour=datL$hour,T.gLf/1000)
datHc1<-data.frame(doy=datH$doy,hour=datH$hour,T.gHf/1000)

datLmet<-read.csv("LowD_met.csv")
datHmet<-read.csv("HighD_met.csv")
#read in th
datLtkg<-join(datLc1,datLmet, by=c("doy","hour"), type="left")
datHtkg<-join(datHc1,datHmet, by=c("doy","hour"), type="left")
#read in vpd and temp




#now merge

Kg.coeff<-function(T){115.8+(.423*T)}
datLtkg$Kg<-Kg.coeff(datLtkg$temp)
datHtkg$Kg<-Kg.coeff(datHtkg$temp)
#assuming a constate pressure for now needs to change
Gs.convert1<-function(Kg,El,D){((Kg*El)/D)*100}
unit.conv<-function(gs,T){gs*.446*(273/(T+273))*(100/101.3)}

Gshigh<-matrix(rep(0,dim(KH)[1]*16), ncol=16)
Gslow<-matrix(rep(0,dim(KL)[1]*16), ncol=16)
Gshighmm<-matrix(rep(0,dim(KH)[1]*16), ncol=16)
Gslowmm<-matrix(rep(0,dim(KL)[1]*16), ncol=16)
Gshighf<-matrix(rep(0,dim(KH)[1]*16), ncol=16)
Gslowf<-matrix(rep(0,dim(KL)[1]*16), ncol=16)
for(i in 1:16){
	Gshigh[,i]<-Gs.convert1(datHtkg$Kg,datHtkg[,i+2],datHtkg$D)
	Gslow[,i]<-Gs.convert1(datLtkg$Kg,datLtkg[,i+2],datLtkg$D)
	Gshighmm[,i]<-unit.conv(Gshigh[,i],datHtkg$temp)*1000
	Gslowmm[,i]<-unit.conv(Gslow[,i],datLtkg$temp)*1000
	Gshighf[,i]<-ifelse(Gshighmm[,i]<300,Gshighmm[,i],NA)
	Gslowf[,i]<-ifelse(Gslowmm[,i]<300,Gslowmm[,i],NA)
}


#low
for(i in 1:16){
	jpeg(file=paste0("c:\\Users\\hkropp\\Google Drive\\Viper_SF\\plots\\gsmm\\lowD", i, ".jpeg"), width=1500, height=1000, units="px")
	plot(seq(1:dim(Gslowf)[1]), Gslowf[,i], xlab="time", ylab="gs (mmol m-2 s-1)", type="b",
			main=paste("sensor #", i), pch=19)
	dev.off()
}
#high
for(i in 1:16){
	jpeg(file=paste0("c:\\Users\\hkropp\\Google Drive\\Viper_SF\\plots\\gsmm\\highD", i, ".jpeg"), width=1500, height=1000, units="px")
	plot(seq(1:dim(Gshighf)[1]),Gshighf[,i], xlab="time", ylab="gs (mmol m-2 s-1)", type="b",
			main=paste("sensor #", i), pch=19)
	dev.off()
}


#now take the average value for each stand at each measurement
gsaveH<-rowMeans(Gshighf, na.rm=TRUE)

gsaveL<-rowMeans(Gslowf, na.rm=TRUE)

#now combine back

gsdfL<-data.frame(doy=datLtkg$doy,hour=datLtkg$hour,D=datLtkg$D,gs=gsaveL)
gsdfH<-data.frame(doy=datHtkg$doy,hour=datHtkg$hour,D=datHtkg$D,gs=gsaveH)


plot(gsdfL$D,gsdfL$gs, pch=19, col="cornflowerblue")
points(gsdfH$D,gsdfH$gs, pch=19, col="darkgreen")

plot(log(gsdfL$D[gsdfL$gs>0&gsdfL$D>.3]),gsdfL$gs[gsdfL$gs>0&gsdfL$D>.3], pch=19, col="cornflowerblue")
points(log(gsdfH$D[gsdfH$gs>0&gsdfH$D>.3]),gsdfH$gs[gsdfH$gs>0&gsdfH$D>.3], pch=19, col="darkgreen")

gsorL<-lm(gsdfL$gs[gsdfL$gs>0&gsdfL$D>.3]~log(gsdfL$D[gsdfL$gs>0&gsdfL$D>.3]))
summary(gsorL)
abline(gsorL, lwd=2, col="cornflowerblue")

gsorH<-lm(gsdfH$gs[gsdfH$gs>0&gsdfH$D>.3]~log(gsdfH$D[gsdfH$gs>0&gsdfH$D>.3]))
summary(gsorH)
abline(gsorH, lwd=2, col="darkgreen")


#high
20/12.88
#low
15.55/8.9

datwp<-read.csv("density_water potential.csv")

#see how many water potential observations across day and location
wp.n<-aggregate(datwp$wp,by=list(datwp$Species,datwp$Site,datwp$DOY),
				FUN="length")
				
datwp<-datwp[datwp$Species=="larix",]
#round to nearest halfhour
datwp$hour<-ifelse(datwp$Hour-floor(datwp$Hour)<=.25,floor(datwp$Hour),
				ifelse(datwp$Hour-floor(datwp$Hour)>.25&datwp$Hour-floor(datwp$Hour)<.45,floor(datwp$Hour)+.5,
				floor(datwp$Hour)+1))
				
datwpH<-datwp[datwp$Site=="h",]
colnames(datwpH)[1]<-"doy"
gswpH<-join(gsdfH,datwpH, by=c("doy", "hour"), type="inner")

datwpL<-datwp[datwp$Site=="l",]
colnames(datwpL)[1]<-"doy"
gswpL<-join(gsdfL,datwpL, by=c("doy", "hour"), type="inner")



plot(gswpL$wp,gswpL$gs, ylab="gs", xlab="water potential", ylim=c(0,60), xlim=c(0,2), col="cornflowerblue", pch=19)
points(gswpH$wp, gswpH$gs,pch=19, col="darkgreen")

TgaveL<-(rowMeans(T.gLf, na.rm=TRUE)/18)*1000
TgaveH<-(rowMeans(T.gHf,na.rm=TRUE)/18)*1000

datLg<-data.frame(doy=datL$doy,hour=datL$hour,T=TgaveL)
datHg<-data.frame(doy=datH$doy,hour=datH$hour,T=TgaveH)

TwpH<-join(datHg,datwpH, by=c("doy", "hour"), type="inner")
TwpL<-join(datLg,datwpL, by=c("doy", "hour"), type="inner")


plot(TwpL$T,TwpL$wp, xlab="T (mmol m-2 s-1)", ylab="water potential", xlim=c(0,.3), ylim=c(2,0), col="cornflowerblue", pch=19)
points(TwpH$T,TwpH$wp,pch=19, col="darkgreen")
