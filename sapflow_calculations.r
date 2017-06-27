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
datSH<-datSH[1:8,]

###################################################
###########corrected dT value with sapwood

#now calcualte a corrected dt value based on sensor length
#note when b=0 and a=1 the dT corrected is equal to the raw dT

dTcorrL<-matrix(rep(NA,dim(datAL)[1]*16), ncol=16)
dTcorrH<-matrix(rep(NA,dim(datAH)[1]*8), ncol=8)
for(i in 1:16){
		dTcorrL[,i]<-(datAL[,i]-(datSL$b[i]*datAL[,16+i]))/datSL$a[i]
	
}
for(i in 1:8){

		dTcorrH[,i]<-(datAH[,i]-(datSH$b[i]*datAH[,9+i]))/datSH$a[i]
	
}


#low
for(i in 1:16){
	jpeg(file=paste0("c:\\Users\\hkropp\\Google Drive\\Viper_SF\\plots\\cor_dT\\Low\\corDT", i, ".jpeg"), width=1500, height=1000, units="px")
	plot(seq(1:dim(dTcorrL)[1]), dTcorrL[,i], xlab="time", ylab="dT cor", type="b",
			main=paste("sensor #", i), pch=19)
	dev.off()
}
#high
for(i in 1:8){
	jpeg(file=paste0("c:\\Users\\hkropp\\Google Drive\\Viper_SF\\plots\\cor_dT\\High\\corDT", i, ".jpeg"), width=1500, height=1000, units="px")
	plot(seq(1:dim(dTcorrH)[1]), dTcorrH[,i], xlab="time", ylab="dT cor", type="b",
			main=paste("sensor #", i), pch=19)
	dev.off()
}

#T diff calculation
KL<-matrix(rep(NA,dim(datAL)[1]*16), ncol=16)
KH<-matrix(rep(NA,dim(datAH)[1]*8), ncol=8)
for(i in 1:16){
		KL[,i]<-(datAL[,16+i]-dTcorrL[,i])/dTcorrL[,i]
	
}

for(i in 1:8){

		KH[,i]<-(datAH[,9+i]-dTcorrH[,i])/dTcorrH[,i]
	
}



#calculate velocity in cm/ s
V.h<-matrix(rep(0,dim(KH)[1]*8), ncol=8)
V.l<-matrix(rep(0,dim(KL)[1]*16), ncol=16)
for(i in 1:8){

	V.h[,i]<-ifelse(KH[,i]>=0,0.0119*(KH[,i]^1.231),NA)
}	
for(i in 1:16){
	V.l[,i]<-ifelse(KL[,i]>=0,0.0119*(KL[,i]^1.231),NA)
}

#output the low density velocity for sapfluxnet
#provide the 
 #doyDL
 #timeDL
 
 #filter any velocity values under zero
 V.l1<-ifelse(V.l<0,NA,V.l)
 #filter any values above the 95% quantile for each sensor
 V.l2<-matrix(rep(0,dim(KL)[1]*16), ncol=16)
 for(i in 1:16){
 
	V.l2[,i]<-ifelse(V.l1[,i]>quantile(V.l1[,i],probs=.9,na.rm=TRUE)|V.l1[,i]==Inf|V.l1[,i]==-Inf,NA,V.l1[,i])
 
 }
 
 #leaf area calc for larch
 leaf.bio<-function(DBH,a.leaf,b.leaf){a.leaf*(DBH^b.leaf)}
datSL$leafwt<-leaf.bio(datSL$DBH,40.5,1.41)	
datSL$leaf<-datSL$leafwt*143
datSL$leafm2<-datSL$leaf*.0001

#sensor 1 consistently has really unusually high values. Omitting becasue
#cannot verify if these values are real or a result of sensor/installation error

#now plot the velocity
#low
for(i in 1:16){
	jpeg(file=paste0("c:\\Users\\hkropp\\Google Drive\\Viper_SF\\plots\\velocity\\Low\\velocity", i, ".jpeg"), width=1500, height=1000, units="px")
	plot(seq(1:dim(V.l2)[1]), V.l2[,i], xlab="time", ylab="V (cm/s)", type="b",
			main=paste("sensor #", i), pch=19)
	dev.off()
}
#high
for(i in 1:8){
	jpeg(file=paste0("c:\\Users\\hkropp\\Google Drive\\Viper_SF\\plots\\velocity\\High\\velocity", i, ".jpeg"), width=1500, height=1000, units="px")
	plot(seq(1:dim(V.h)[1]), V.h[,i], xlab="time", ylab="V (cm/s)", type="b",
			main=paste("sensor #", i), pch=19)
	dev.off()
}




#now calculate in g per s

F.h<-matrix(rep(0,dim(KH)[1]*8), ncol=8)
F.hf<-matrix(rep(0,dim(KH)[1]*8), ncol=8)
F.lf<-matrix(rep(0,dim(KL)[1]*16), ncol=16)
F.l<-matrix(rep(0,dim(KL)[1]*16), ncol=16)
for(i in 1:16){

	F.l[,i]<-V.l[,i]*datSL$sapA[i]
	F.lf[,i]<-ifelse(F.l[,i]<5,F.l[,i],NA)
	
}	
for(i in 1:8){
	F.h[,i]<-V.h[,i]*datSH$sapA[i]
	F.hf[,i]<-ifelse(F.h[,i]<5,F.h[,i],NA)
	}	
	
#now plot the flow in g/s
#low
for(i in 1:16){
	jpeg(file=paste0("c:\\Users\\hkropp\\Google Drive\\Viper_SF\\plots\\Flow\\Low\\flow", i, ".jpeg"), width=1500, height=1000, units="px")
	plot(seq(1:dim(F.lf)[1]), F.lf[,i], xlab="time", ylab="Flow (g/s)", type="b",
			main=paste("sensor #", i), pch=19)
	dev.off()
}
#high
for(i in 1:8){
	jpeg(file=paste0("c:\\Users\\hkropp\\Google Drive\\Viper_SF\\plots\\Flow\\High\\flow", i, ".jpeg"), width=1500, height=1000, units="px")
	plot(seq(1:dim(F.hf)[1]), F.hf[,i], xlab="time", ylab="Flow (g/s)", type="b",
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
T.gH<-matrix(rep(0,dim(KH)[1]*8), ncol=8)
T.gLf<-matrix(rep(0,dim(KL)[1]*16), ncol=16)
T.gLf2<-matrix(rep(0,dim(KL)[1]*16), ncol=16)
T.gHf<-matrix(rep(0,dim(KH)[1]*8), ncol=8)
for(i in 1:8){
	T.gH[,i]<-F.hf[,i]/datSH$leafm2[i]
	T.gHf[,i]<-ifelse(T.gH[,i]<.01,T.gH[,i],NA)
	
}
for(i in 1:16){
	T.gL[,i]<-F.lf[,i]/datSL$leafm2[i]	
	T.gLf[,i]<-ifelse(T.gL[,i]<1,T.gL[,i],NA)

}

#low
for(i in 1:16){
	jpeg(file=paste0("c:\\Users\\hkropp\\Google Drive\\Viper_SF\\plots\\Tg\\Low\\sensor", i, ".jpeg"), width=1500, height=1000, units="px")
	plot(seq(1:dim(T.gLf)[1]), T.gLf[,i], xlab="time", ylab="Flow (g/ m2 s)", type="b",
			main=paste("sensor #", i), pch=19)
	dev.off()
}
#high
for(i in 1:8){
	jpeg(file=paste0("c:\\Users\\hkropp\\Google Drive\\Viper_SF\\plots\\Tg\\High\\sensor", i, ".jpeg"), width=1500, height=1000, units="px")
	plot(seq(1:dim(T.gH)[1]), T.gHf[,i], xlab="time", ylab="Flow (g/ m2 s)", type="b",
			main=paste("sensor #", i), pch=19)
	dev.off()
}

datLc1<-data.frame(doy=doyDL,hour=timeDL,T.gLf/1000)
datHc1<-data.frame(doy=doyDH,hour=timeDH,T.gHf/1000)

datLmet<-read.csv("LowD_met.csv")
datHmet<-read.csv("HighD_met.csv")
#read in th
datLtkg<-join(datLc1,datLmet, by=c("doy","hour"), type="left")
datHtkg<-join(datHc1,datHmet, by=c("doy","hour"), type="left")
#calculate saturated vapor pressure
datLe.sat<-0.611*exp((17.502*datLtkg$temp)/(datLtkg$temp+240.97))
datHe.sat<-0.611*exp((17.502*datHtkg$temp)/(datHtkg$temp+240.97))
#calculate vapor pressure deficit
#here rh is is in decimal form 
datLtkg$RHfix<-ifelse(datLtkg$RH>=1,.999,datLtkg$RH)
datHtkg$RHfix<-ifelse(datHtkg$RH>=1,.999,datHtkg$RH)

datLtkg$D<-(datLe.sat-(datLtkg$RHfix*datLe.sat))
datHtkg$D<-(datHe.sat-(datHtkg$RHfix*datHe.sat))


Kg.coeff<-function(T){115.8+(.423*T)}
datLtkg$Kg<-Kg.coeff(datLtkg$temp)
datHtkg$Kg<-Kg.coeff(datHtkg$temp)
#assuming a constate pressure for now needs to change
Gs.convert1<-function(Kg,El,D){((Kg*El)/D)*100}
unit.conv<-function(gs,T){gs*.446*(273/(T+273))*(100/101.3)}

Gshigh<-matrix(rep(0,dim(KH)[1]*8), ncol=8)
Gslow<-matrix(rep(0,dim(KL)[1]*16), ncol=16)
Gshighmm<-matrix(rep(0,dim(KH)[1]*8), ncol=8)
Gslowmm<-matrix(rep(0,dim(KL)[1]*16), ncol=16)
Gshighf<-matrix(rep(0,dim(KH)[1]*8), ncol=8)
Gslowf<-matrix(rep(0,dim(KL)[1]*16), ncol=16)
for(i in 1:8){
	Gshigh[,i]<-Gs.convert1(datHtkg$Kg,datHtkg[,i+2],datHtkg$D)
	Gshighmm[,i]<-unit.conv(Gshigh[,i],datHtkg$temp)*1000
	Gshighf[,i]<-ifelse(Gshighmm[,i]<400,Gshighmm[,i],NA)
}	
for(i in 1:16){	
	Gslow[,i]<-Gs.convert1(datLtkg$Kg,datLtkg[,i+2],datLtkg$D)
	Gslowmm[,i]<-unit.conv(Gslow[,i],datLtkg$temp)*1000
	Gslowf[,i]<-ifelse(Gslowmm[,i]<400,Gslowmm[,i],NA)
}
###initially tried filtering gs by quantile but the values
###can be so extreme from numerical anomolies that even the 
###90th quantile can result in gs in the millions
###however, going lower can mean some sensors are capped at 80mmol
###and others at 25000. It was better to stick to what is a realistic
###threshold for these trees

#low
for(i in 2:16){
	jpeg(file=paste0("c:\\Users\\hkropp\\Google Drive\\Viper_SF\\plots\\gsmm\\Low\\sensor", i, ".jpeg"), width=1500, height=1000, units="px")
	plot(seq(1:dim(Gslowf)[1]), Gslowf[,i], xlab="time", ylab="gs (mmol m-2 s-1)", type="b",
			main=paste("sensor #", i), pch=19)
	dev.off()
}
#high
for(i in 1:8){
	jpeg(file=paste0("c:\\Users\\hkropp\\Google Drive\\Viper_SF\\plots\\gsmm\\High\\sensor", i, ".jpeg"), width=1500, height=1000, units="px")
	plot(seq(1:dim(Gshighf)[1]),Gshighf[,i], xlab="time", ylab="gs (mmol m-2 s-1)", type="b",
			main=paste("sensor #", i), pch=19)
	dev.off()
}
