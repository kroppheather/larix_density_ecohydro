######################################
##script for sapflow calculations ####
##of gc and model script for      ####
##density gradient sapflow        ####
######################################


library(lubridate)
library(plyr)
library(caTools)
library(rjags)
library(coda)
library(mcmcplots)

setwd("c:\\Users\\hkropp\\Google Drive\\Viper_SF")

#read in TDP 
#not these files are truncated to start at 5am
#on first day of a full data measurement with no
#start up data included in the first day of data
#high density
datDTH <- read.csv("high_density_TDP.csv")
#low density
datDTL <- read.csv("low_density_TDP.csv")

#high density 2017
datDTH17 <- read.table("HD_TDP_DT.csv", sep=",", head=TRUE, na.strings=c("NAN"))
#low density 2017
datDTL17 <- read.table("LD_TDP_DT.csv", sep=",", head=TRUE, na.strings=c("NAN"))
#read in sensor info
datS <- read.csv("sensor_info.csv")

#read in 2017 sensor info
datS17<-read.csv("sensor_info17.csv")

##### read in sapwood thickness or sensor correciton
datSW <- read.csv("sap_thick.csv")

####met data for both sites
datLmet<-read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_SF\\csv_out\\LDF2RS.csv.csv")
datHmet<-read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_SF\\csv_out\\DavCnpy.csv.csv")

#airport met data
datAD<-read.table("airport.csv", sep=";", head=TRUE, skip=6, stringsAsFactors=FALSE)

#sensors 9-16 were run at the incorrect voltage
#and clearly have incorrect dT values for it
#so excluding from calculations
datH <- datDTH[,5:(4+8)]
datL <- datDTL[,5:dim(datDTL)[2]]
datH17 <- datDTH17[,7:22]
datL17 <- datDTL17[,7:22]
#set up date
dateDH <- as.Date(datDTH$TIMESTAMP, "%m/%d/%Y %H:%M")
dateDL <- as.Date(datDTL$TIMESTAMP, "%m/%d/%Y %H:%M")
dateDH17 <- as.Date(datDTH17$TIMESTAMP, "%m/%d/%Y %H:%M")
dateDL17 <- as.Date(datDTL17$TIMESTAMP, "%m/%d/%Y %H:%M")

doyDH <- yday(dateDH)
doyDL <- yday(dateDL)
doyDH17 <- yday(dateDH17)
doyDL17 <- yday(dateDL17)

#convert time
timeDH <- ifelse(datDTH$JHM/100-floor(datDTH$JHM/100)==0,datDTH$JHM/100,floor(datDTH$JHM/100)+.5)
timeDL <- ifelse(datDTL$JHM/100-floor(datDTL$JHM/100)==0,datDTL$JHM/100,floor(datDTL$JHM/100)+.5)
#convert minutues to demicamal in 2017
timeDL17<-datDTL17$hour+(datDTL17$minute/60)
timeDH17<-datDTH17$hour+(datDTH17$minute/60)
#need to create new doy index that defines day of year between 5am and 5am
#for defining intervals that would count refil as in the same day
datL$doy <- ifelse(timeDL<5,doyDL-1,doyDL)
datH$doy <- ifelse(timeDH<5,doyDH-1,doyDH)
datL17$doy <- ifelse(timeDL17<5,doyDL17-1,doyDL17)
datH17$doy <- ifelse(timeDH17<5,doyDH17-1,doyDH17)


#get the unique dayid for each stand
#get the number of observations in a day for each stand
DaysL <- aggregate(datL$doy, by=list(datL$doy), FUN="length")
colnames(DaysL) <- c("doy", "nobs")
DaysL$dayid <- seq(1,dim(DaysL)[1])

DaysH <- aggregate(datH$doy, by=list(datH$doy), FUN="length")
colnames(DaysH) <- c("doy", "nobs")
DaysH$dayid <-  seq(1, dim(DaysH)[1])	

DaysL17 <- aggregate(datL17$doy, by=list(datL17$doy), FUN="length")
colnames(DaysL17) <- c("doy", "nobs")
DaysL17$dayid <- seq(1,dim(DaysL17)[1])

DaysH17 <- aggregate(datH17$doy, by=list(datH17$doy), FUN="length")
colnames(DaysH17) <- c("doy", "nobs")
DaysH17$dayid <-  seq(1, dim(DaysH17)[1])

###########
####calculate the max dT in a day
#low density
#and 2017 data
maxDTL <- matrix(rep(NA,dim(DaysL)[1]*16),ncol=16)
maxDTL17 <- matrix(rep(NA,dim(DaysL17)[1]*16),ncol=16)
maxDTH17 <- matrix(rep(NA,dim(DaysH17)[1]*16),ncol=16)
#output matrix of the max per each day
for(j in 1:16){
	for(i in 1:dim(DaysL)[1]){
		maxDTL[i,j] <- max(na.omit(datL[datL$doy==DaysL$doy[i],j]))
	}
	
	for(i in 1:dim(DaysL17)[1]){
		if(length(na.omit(datL17[datL17$doy==DaysL17$doy[i],j]))!=0){
			maxDTL17[i,j] <- max(na.omit(datL17[datL17$doy==DaysL17$doy[i],j]))
		}
	}
	for(i in 1:dim(DaysH17)[1]){
		maxDTH17[i,j] <- max(na.omit(datH17[datH17$doy==DaysH17$doy[i],j]))
	}
}
#calcuate high density 2016
maxDTH <- matrix(rep(0,dim(DaysH)[1]*8),ncol=8)

for(j in 1:8){
	for(i in 1:dim(DaysH)[1]){
		maxDTH[i,j] <- max(na.omit(datH[datH$doy==DaysH$doy[i],j]))
	}
}
maxDTL <- as.data.frame(maxDTL)
maxDTL$doy <- DaysL$doy

maxDTH <- as.data.frame(maxDTH)
maxDTH$doy <- DaysH$doy

maxDTL17 <- as.data.frame(maxDTL17)
maxDTL17$doy <- DaysL17$doy

maxDTH17 <- as.data.frame(maxDTH17)
maxDTH17$doy <- DaysH17$doy

# now combine both
datAH <- join(datH,maxDTH, by=c("doy"), type="left")
datAL <- join(datL,maxDTL, by=c("doy"), type="left")

datAH17 <- join(datH17,maxDTH17, by=c("doy"), type="left")
datAL17 <- join(datL17,maxDTL17, by=c("doy"), type="left")

###################################################
##### sapwood thickness and sensor correction

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

###########corrected dT value with sapwood
#now calcualte a corrected dt value based on sensor length
#note when b=0 and a=1 the dT corrected is equal to the raw dT
dTcorrL<-matrix(rep(NA,dim(datAL)[1]*16), ncol=16)
dTcorrH<-matrix(rep(NA,dim(datAH)[1]*8), ncol=8)
dTcorrL17<-matrix(rep(NA,dim(datAL17)[1]*16), ncol=16)
dTcorrH17<-matrix(rep(NA,dim(datAH17)[1]*16), ncol=16)
for(i in 1:16){
		dTcorrL[,i] <- (datAL[,i]-(datSL$b[i]*datAL[,17+i]))/datSL$a[i]	
		dTcorrL17[,i] <- (datAL17[,i]-(datSL17$b[i]*datAL17[,17+i]))/datSL17$a[i]	
		dTcorrH17[,i]<- (datAH17[,i]-(datSH17$b[i]*datAH17[,17+i]))/datSH17$a[i]	
		}
for(i in 1:8){

		dTcorrH[,i]<- (datAH[,i]-(datSH$b[i]*datAH[,9+i]))/datSH$a[i]	
}

#T diff calculation
#Tmax-dtcor/dtcor
KL<-matrix(rep(NA,dim(datAL)[1]*16), ncol=16)
KH<-matrix(rep(NA,dim(datAH)[1]*8), ncol=8)
KL17<-matrix(rep(NA,dim(datAL17)[1]*16), ncol=16)
KH17<-matrix(rep(NA,dim(datAH17)[1]*16), ncol=16)
#low
for(i in 1:16){
		KL[,i] <- (datAL[,17+i]-dTcorrL[,i])/dTcorrL[,i]	
		KL17[,i] <- (datAL17[,17+i]-dTcorrL17[,i])/dTcorrL17[,i]
		KH17[,i] <- (datAH17[,17+i]-dTcorrH17[,i])/dTcorrH17[,i]			
}
#high
for(i in 1:8){
		KH[,i] <- (datAH[,9+i]-dTcorrH[,i])/dTcorrH[,i]	
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
	
	V.l17[,1]<-ifelse(doyDL17<170,NA,V.l17[,1])

######END Corrected velocity done with V.h and V.l ##############


#################################################################
###### calculate transpiration                  #################
#################################################################

#now calculate flow in  in g per s

F.h<-matrix(rep(0,dim(KH)[1]*8), ncol=8)
F.hf<-matrix(rep(0,dim(KH)[1]*8), ncol=8)
F.lf17<-matrix(rep(0,dim(KL17)[1]*16), ncol=16)
F.l17<-matrix(rep(0,dim(KL17)[1]*16), ncol=16)
F.lf<-matrix(rep(0,dim(KL)[1]*16), ncol=16)
F.l<-matrix(rep(0,dim(KL)[1]*16), ncol=16)
F.hf17<-matrix(rep(0,dim(KH17)[1]*16), ncol=16)
F.h17<-matrix(rep(0,dim(KH17)[1]*16), ncol=16)
########################################
########################################
########FILTER POINT 1   ###############
########Quantile filter  ###############
########################################

for(i in 1:16){
	#filter for values above the 95% 
	#filtering at 0.9 seems to remove some values in the a realistic range
	#of the data
	F.l[,i]<-V.l[,i]*datSL$sapA[i]
	F.lf[,i]<-ifelse(F.l[,i]<quantile(F.l[,i],probs=c(0.95),na.rm=TRUE),F.l[,i],NA)
	F.l17[,i]<-V.l17[,i]*datSL17$sapA[i]
	F.lf17[,i]<-ifelse(F.l17[,i]<quantile(F.l17[,i],probs=c(0.95),na.rm=TRUE),F.l17[,i],NA)	
	F.h17[,i]<-V.h17[,i]*datSH17$sapA[i]
	F.hf17[,i]<-ifelse(F.h17[,i]<quantile(F.h17[,i],probs=c(0.95),na.rm=TRUE),F.h17[,i],NA)
}	
for(i in 1:8){

	F.h[,i]<-V.h[,i]*datSH$sapA[i]
	F.hf[,i]<-ifelse(F.h[,i]<quantile(F.h[,i],probs=c(0.95),na.rm=TRUE),F.h[,i],NA)
	}

###########################################
##normalize by leaf area for transpiration
###########################################

 #leaf area calc for larch
 #allometry from Alexander 2012
leaf.bio<-function(DBH,a.leaf,b.leaf){a.leaf*(DBH^b.leaf)}
datSL$leafwt<-leaf.bio(datSL$DBH,40.5,1.41)	
#larch cm2/g
datSL$leaf<-datSL$leafwt*143
#convert to m2 
datSL$leafm2<-datSL$leaf*.0001


datSL17$leafwt<-leaf.bio(datSL17$DBH..cm.,40.5,1.41)	
#larch cm2/g
datSL17$leaf<-datSL17$leafwt*143
#convert to m2 
datSL17$leafm2<-datSL17$leaf*.0001

#high density calcs
datSH$leafwt<-leaf.bio(datSH$DBH,40.5,1.41)	
datSH$leaf<-datSH$leafwt*143
datSH$leafm2<-datSH$leaf*.0001

datSH17$leafwt<-leaf.bio(datSH17$DBH..cm.,40.5,1.41)	
datSH17$leaf<-datSH17$leafwt*143
datSH17$leafm2<-datSH17$leaf*.0001

########################################
########################################
########FILTER POINT2    ###############
########Quantile filter  ###############
########################################

#now calculate in g m-2 s
T.gL<-matrix(rep(0,dim(KL)[1]*16), ncol=16)
T.gH17<-matrix(rep(0,dim(KH17)[1]*16), ncol=16)
T.gHf17<-matrix(rep(0,dim(KH17)[1]*16), ncol=16)
T.gLf<-matrix(rep(0,dim(KL)[1]*16), ncol=16)
T.gL17<-matrix(rep(0,dim(KL17)[1]*16), ncol=16)
T.gLf17<-matrix(rep(0,dim(KL17)[1]*16), ncol=16)
T.gH<-matrix(rep(0,dim(KH)[1]*8), ncol=8)
T.gHf<-matrix(rep(0,dim(KH)[1]*8), ncol=8)
for(i in 1:8){
	T.gH[,i]<-F.hf[,i]/datSH$leafm2[i]
	T.gHf[,i]<-ifelse(T.gH[,i]<quantile(T.gH[,i],probs=c(0.95),na.rm=TRUE),T.gH[,i],NA)
	
}
for(i in 1:16){
	T.gL[,i]<-F.lf[,i]/datSL$leafm2[i]	
	T.gLf[,i]<-ifelse(T.gL[,i]<quantile(T.gL[,i],probs=c(0.95),na.rm=TRUE),T.gL[,i],NA)
	T.gL17[,i]<-F.lf17[,i]/datSL17$leafm2[i]	
	T.gLf17[,i]<-ifelse(T.gL17[,i]<quantile(T.gL17[,i],probs=c(0.95),na.rm=TRUE),T.gL17[,i],NA)
	T.gH17[,i]<-F.hf17[,i]/datSH17$leafm2[i]
	T.gHf17[,i]<-ifelse(T.gH17[,i]<quantile(T.gH17[,i],probs=c(0.95),na.rm=TRUE),T.gH17[,i],NA)
}

#create a dataframe
El.H<-data.frame(doy=doyDH, year=rep(2016,length(doyDH)),T.gHf[,1:8])

El.L<-data.frame(doy=doyDL, year=rep(2016,length(doyDL)),T.gLf[,1:16])

El.H17<-data.frame(doy=doyDH17, year=rep(2017,length(doyDH17)),T.gHf17[,1:16])

El.L17<-data.frame(doy=doyDL17, year=rep(2017,length(doyDL17)),T.gLf17[,1:16])

#########End Transpiration (T) calcs ####################

#########################################################
######calculate stomatal conductance ####################
#########################################################

#join air pressure to each met table
#get date from each
dateAP<-as.Date(rownames(datAD), "%d.%m.%Y %H:%M")
#make a data frame that is pressure
#Po is the air pressure but the headers get
# shifted in the format so the row name is
# T

datPhh<-data.frame(doy=yday(dateAP),year=year(dateAP), Pkpa= datAD$T/7.474)

datPj<-aggregate(datPhh$Pkpa, by=list(datPhh$doy, datPhh$year), FUN="mean")
colnames(datPj)<-c("doy","year","Pkpa")

datLmet<-join(datLmet, datPj, by=c("doy", "year"), type="left")
datHmet<-join(datHmet, datPj, by=c("doy", "year"), type="left")



#data frame of T in kg m-2 s-1
datLc1<-data.frame(doy=doyDL,year=rep(2016, length(doyDL)),hour=timeDL,T.gLf/1000)
datHc1<-data.frame(doy=doyDH,year=rep(2016, length(doyDH)),hour=timeDH,T.gHf/1000)

datL17c1<-data.frame(doy=doyDL17,year=rep(2017, length(doyDL17)), hour=timeDL17,T.gLf17/1000)
datH17c1<-data.frame(doy=doyDH17,year=rep(2017, length(doyDH17)), hour=timeDH17,T.gHf17/1000)

#join met data to T
datLtkg<-join(datLc1,datLmet, by=c("doy","year","hour"), type="left")
datHtkg<-join(datHc1,datHmet, by=c("doy","year","hour"), type="left")

datLtkg17<-join(datL17c1,datLmet, by=c("doy","year","hour"), type="inner")
datHtkg17<-join(datH17c1,datHmet, by=c("doy","year","hour"), type="left")

#calculate saturated vapor pressure
datLe.sat<-0.611*exp((17.502*datLtkg$Temp)/(datLtkg$Temp+240.97))
datHe.sat<-0.611*exp((17.502*datHtkg$Ctemp)/(datHtkg$Ctemp+240.97))
datL17e.sat<-0.611*exp((17.502*datLtkg17$Temp)/(datLtkg17$Temp+240.97))
datH17e.sat<-0.611*exp((17.502*datHtkg17$Ctemp)/(datHtkg17$Ctemp+240.97))

#calculate vapor pressure deficit
#here rh is is in decimal form 
datLtkg$RHfix<-ifelse(datLtkg$RH>=1,.999,datLtkg$RH)
datHtkg$RHfix<-ifelse(datHtkg$RH>=1,.999,datHtkg$RH)
datLtkg17$RHfix<-ifelse(datLtkg17$RH>=1,.999,datLtkg17$RH)
datHtkg17$RHfix<-ifelse(datHtkg17$RH>=1,.999,datHtkg17$RH)


datLtkg$D<-(datLe.sat-(datLtkg$RHfix*datLe.sat))
datHtkg$D<-(datHe.sat-(datHtkg$RHfix*datHe.sat))
datLtkg17$D<-(datL17e.sat-(datLtkg17$RHfix*datL17e.sat))
datHtkg17$D<-(datH17e.sat-(datHtkg17$RHfix*datH17e.sat))

Kg.coeff<-function(T){115.8+(.423*T)}
datLtkg$Kg<-Kg.coeff(datLtkg$Temp)
datHtkg$Kg<-Kg.coeff(datHtkg$Ctemp)

datLtkg17$Kg<-Kg.coeff(datLtkg17$Temp)
datHtkg17$Kg<-Kg.coeff(datHtkg17$Ctemp)
#convert to gs
Gs.convert1<-function(Kg,El,D,P){((Kg*El)/D)*P}
#change units to moles
unit.conv<-function(gs,T,P){gs*.446*(273/(T+273))*(P/101.3)}

Gshigh<-matrix(rep(0,dim(KH)[1]*8), ncol=8)
Gslow<-matrix(rep(0,dim(KL)[1]*16), ncol=16)
Gshighmm<-matrix(rep(0,dim(KH)[1]*8), ncol=8)
Gslowmm<-matrix(rep(0,dim(KL)[1]*16), ncol=16)
Gshighf<-matrix(rep(0,dim(KH)[1]*8), ncol=8)
Gslowf<-matrix(rep(0,dim(KL)[1]*16), ncol=16)


Gshigh17<-matrix(rep(0,dim(KH17)[1]*16), ncol=16)
Gslow17<-matrix(rep(0,dim(datLtkg17)[1]*16), ncol=16)
Gshighmm17<-matrix(rep(0,dim(KH17)[1]*16), ncol=16)
Gslowmm17<-matrix(rep(0,dim(datLtkg17)[1]*16), ncol=16)
Gshighf17<-matrix(rep(0,dim(KH17)[1]*16), ncol=16)
Gslowf17<-matrix(rep(0,dim(datLtkg17)[1]*16), ncol=16)

########################################
########################################
########FILTER POINT 3   ###############
########Range filter     ###############
########################################
for(i in 1:8){
	Gshigh[,i]<-Gs.convert1(datHtkg$Kg,datHtkg[,i+3],datHtkg$D, datHtkg$Pkpa)
	Gshighmm[,i]<-unit.conv(Gshigh[,i],datHtkg$Ctemp, datHtkg$Pkpa)*1000
	Gshighf[,i]<-ifelse(Gshighmm[,i]<400,Gshighmm[,i],NA)
}	
for(i in 1:16){	
	Gslow[,i]<-Gs.convert1(datLtkg$Kg,datLtkg[,i+3],datLtkg$D, datLtkg$Pkpa)
	Gslowmm[,i]<-unit.conv(Gslow[,i],datLtkg$Temp, datLtkg$Pkpa)*1000
	Gslowf[,i]<-ifelse(Gslowmm[,i]<400,Gslowmm[,i],NA)
	
	Gshigh17[,i]<-Gs.convert1(datHtkg17$Kg,datHtkg17[,i+3],datHtkg17$D, datHtkg17$Pkpa)
	Gshighmm17[,i]<-unit.conv(Gshigh17[,i],datHtkg17$Ctemp, datHtkg17$Pkpa)*1000
	Gshighf17[,i]<-ifelse(Gshighmm17[,i]<400,Gshighmm17[,i],NA)
	
	Gslow17[,i]<-Gs.convert1(datLtkg17$Kg,datLtkg17[,i+3],datLtkg17$D, datLtkg17$Pkpa)
	Gslowmm17[,i]<-unit.conv(Gslow17[,i],datLtkg17$Temp, datLtkg17$Pkpa)*1000
	Gslowf17[,i]<-ifelse(Gslowmm17[,i]<400,Gslowmm17[,i],NA)
}

#create a dataframe
gc.H<-data.frame(doy=doyDH, year=rep(2016,length(doyDH)),Gshighf[,1:8])
#exclude sensor 1 because it is very high and can't be verified 
gc.L<-data.frame(doy=doyDL, year=rep(2016,length(doyDL)),Gslowf[,1:16])

gc.H17<-data.frame(doy=doyDH17, year=rep(2017,length(doyDH17)),Gshighf17[,1:8])
#exclude sensor 1 because it is very high and can't be verified 
gc.L17<-data.frame(doy=doyDL17, year=rep(2017,length(doyDL17)),Gslowf17[,1:16])


###########END of canopy stomatal conductance calc ############

###################################################################################
###################################################################################
############### Average across each sensor                            #############
###################################################################################

################################
#####Canopy T

#get average value excluding na
mEl.H<-data.frame(doy=doyDH,year=El.H$year,hour=timeDH, El=rowMeans(El.H[,3:10],na.rm=TRUE))
mEl.L<-data.frame(doy=doyDL,year=El.L$year,hour=timeDL, El=rowMeans(El.L[,3:18],na.rm=TRUE))
mEl.H17<-data.frame(doy=doyDH17,year=El.H17$year,hour=timeDH17, El=rowMeans(El.H17[,3:18],na.rm=TRUE))
mEl.L17<-data.frame(doy=doyDL17,year=El.L17$year,hour=timeDL17, El=rowMeans(El.L17[,3:18],na.rm=TRUE))

#count how many observations are in each time period
#so that any observations with less than 3 sensor obs can
#be excluded
Hna<-apply(El.H[,3:10],2,is.na)
Hna.c<-apply(Hna[,1:8],1,sum)
Hflag<-ifelse(Hna.c>6,1,0)

Lna<-apply(El.L[,3:18],2,is.na)
Lna.c<-apply(Lna[,1:16],1,sum)
Lflag<-ifelse(Lna.c>14,1,0)

Hna17<-apply(El.H17[,3:10],2,is.na)
Hna.c17<-apply(Hna17[,1:8],1,sum)
Hflag17<-ifelse(Hna.c17>6,1,0)

Lna17<-apply(El.L17[,3:18],2,is.na)
Lna.c17<-apply(Lna17[,1:16],1,sum)
Lflag17<-ifelse(Lna.c17>14,1,0)

#now turn any means into NA without sufficient obs
mEl.L$El<-ifelse(Lflag==1,NA,mEl.L$El)
mEl.L17$El<-ifelse(Lflag17==1,NA,mEl.L17$El)
mEl.H$El<-ifelse(Hflag==1,NA,mEl.H$El)
mEl.H17$El<-ifelse(Hflag17==1,NA,mEl.H17$El)

###############################
### gc
mgc.H<-data.frame(doy=doyDH,year=El.H$year,hour=timeDH, gc=rowMeans(Gshighf[,1:8],na.rm=TRUE))
mgc.L<-data.frame(doy=doyDL,year=El.L$year,hour=timeDL, gc=rowMeans(Gslowf[,1:16],na.rm=TRUE))
mgc.H17<-data.frame(doy=doyDH17,year=El.H17$year,hour=timeDH17, gc=rowMeans(Gshighf17[,1:16],na.rm=TRUE))
mgc.L17<-data.frame(doy=doyDL17,year=El.L17$year,hour=timeDL17, gc=rowMeans(Gslowf17[,1:16],na.rm=TRUE))

#set up flags
gHna<-apply(Gshighf[,1:8],2,is.na)
gHna.c<-apply(gHna[,1:8],1,sum)
gHflag<-ifelse(gHna.c>6,1,0)

gLna<-apply(Gslowf[,1:16],2,is.na)
gLna.c<-apply(gLna[,1:16],1,sum)
gLflag<-ifelse(gLna.c>14,1,0)

gHna17<-apply(Gshighf17[,1:16],2,is.na)
gHna.c17<-apply(gHna17[,1:16],1,sum)
gHflag17<-ifelse(gHna.c17>14,1,0)

gLna17<-apply(Gslowf17[,1:16],2,is.na)
gLna.c17<-apply(gLna17[,1:16],1,sum)
gLflag17<-ifelse(gLna.c17>14,1,0)

#use flags to make na for anything with insufficient
#observation count
mgc.L$gc<-ifelse(gLflag==1,NA,mgc.L$gc)
mgc.H$gc<-ifelse(gHflag==1,NA,mgc.H$gc)
mgc.L17$gc<-ifelse(gLflag17==1,NA,mgc.L17$gc)
mgc.H17$gc<-ifelse(gHflag17==1,NA,mgc.H17$gc)




###############################################################
###Filter for unreliable sap flow days ########################
###d

#header is shifted over 1 
#precip is RRR in reliable prognosis, but here it is in the Td column
Precip1 <- datAD$Td

#no convert precip to numbers
#calling trace precipitation 0.01
#precip is in mm
PrecipF1 <- ifelse(Precip1 == "Trace of precipitation" ,"0.01" ,
			ifelse(Precip1 == "No precipitation", "0",
				ifelse(Precip1 == "",0 ,Precip1)))
				
PrecipN1<-as.numeric(PrecipF1)



#now turn into a dataframe
PrecipDat <- data.frame(doy=yday(dateAP),year=year(dateAP),
						Precip=PrecipN1)
						

#now summarize across days

PrecipDay <- aggregate(PrecipDat$Precip, 
				by=list(PrecipDat$doy,PrecipDat$year), FUN="sum")
colnames(PrecipDay)<-c("doy","year","Pr.mm")				

#join D to dataframes
mEl.H$D<-datHtkg$D
mEl.L$D<-datLtkg$D
mEl.H17$D<-datHtkg17$D
mEl.L17$D<-datLtkg17$D

mgc.L$D<-datLtkg$D
mgc.H$D<-datHtkg$D
mgc.L17$D<-datLtkg17$D
mgc.H17$D<-datHtkg17$D

#join Precipitation with measurements 
Hgc<-join(mgc.H, PrecipDay, by=c("doy","year"), type="left")
Lgc<-join(mgc.L, PrecipDay, by=c("doy","year"), type="left")
Hgc17<-join(mgc.H17, PrecipDay, by=c("doy","year"), type="left")
Lgc17<-join(mgc.L17, PrecipDay, by=c("doy","year"), type="left")

HEl<-join(mEl.H,PrecipDay, by=c("doy","year"), type="left")
LEl<-join(mEl.L,PrecipDay, by=c("doy","year"), type="left")
HEl17<-join(mEl.H17,PrecipDay, by=c("doy","year"), type="left")
LEl17<-join(mEl.L17,PrecipDay, by=c("doy","year"), type="left")

#exclude precip days over 1mm 
Hgc$gc<-ifelse(Hgc$Pr.mm>1,NA,Hgc$gc)
Lgc$gc<-ifelse(Lgc$Pr.mm>1,NA,Lgc$gc)
Hgc17$gc<-ifelse(Hgc17$Pr.mm>1,NA,Hgc17$gc)
Lgc17$gc<-ifelse(Lgc17$Pr.mm>1,NA,Lgc17$gc)

HEl$El<-ifelse(HEl$Pr.mm>1,NA, HEl$El)
LEl$El<-ifelse(LEl$Pr.mm>1,NA, LEl$El)
HEl17$El<-ifelse(HEl17$Pr.mm>1,NA, HEl17$El)
LEl17$El<-ifelse(LEl17$Pr.mm>1,NA, LEl17$El)

##### end filter  ######################


###########################################
###########################################
### start organizing for model version ####
###########################################
###########################################


#aggregate daily air temp
LtempD<-aggregate(datLmet$Temp,by=list(datLmet$doy,datLmet$year),FUN="mean",na.action=na.omit)
HtempD<-aggregate(datHmet$Ctemp,by=list(datHmet$doy,datHmet$year),FUN="mean",na.action=na.omit)
colnames(LtempD)<-c("doy","year","Temp")
colnames(HtempD)<-c("doy","year","Temp")
#join together and add a standID
LtempD$standID<-rep(2,dim(LtempD)[1])
HtempD$standID<-rep(1,dim(HtempD)[1])
TempAll<-rbind(LtempD,HtempD)

#PrecipDay
#need to calculate previous weeks total precipitation
PrecipPast<-rep(NA,dim(PrecipDay)[1])
for(i in 7:dim(PrecipDay)[1]){
	PrecipPast[i]<-(PrecipDay$Pr.mm[i]+PrecipDay$Pr.mm[i-1]+PrecipDay$Pr.mm[i-2]+
					PrecipDay$Pr.mm[i-3]+PrecipDay$Pr.mm[i-4]+PrecipDay$Pr.mm[i-5]
					+PrecipDay$Pr.mm[i-6])

}
PrecipDay$WeekP<-PrecipPast



#need a day siteID

#first combine the gc for both years for each stand
#1 for high and 2 for low
Hgc$standID<-rep(1,dim(Hgc)[1])
Lgc$standID<-rep(2,dim(Lgc)[1])
Hgc17$standID<-rep(1,dim(Hgc17)[1])
Lgc17$standID<-rep(2,dim(Lgc17)[1])

#combine into the same dataframe
gcAllt<-rbind(Hgc,Lgc,Hgc17,Lgc17)
#exclude days with NA
gcAll<-na.omit(gcAllt)

#now get unique day, site, year ID for the model

SiteDayTable<-unique(data.frame(doy=gcAll$doy,year=gcAll$year,standID=gcAll$standID))
#create the ID
SiteDayTable$SiteDayID<- seq(1,dim(SiteDayTable)[1])


#join the DaysiteID to the Temp data

TempforMod<-join(SiteDayTable,TempAll,by=c("doy","year","standID"),type="left")

#now join Precip day to table

MDatforMod<-join(TempforMod, PrecipDay,by=c("doy","year"), type="left")


#there is one missing canopy temp for low density on day 162
#fill in missing value for now from high densisity
MDatforMod$Temp[MDatforMod$doy==173&MDatforMod$year==2017&MDatforMod$standID==2]<-MDatforMod$Temp[MDatforMod$doy==173&MDatforMod$year==2017&MDatforMod$standID==1]

#last step is to add the siteday index to the gc data

gcforMod<-join(gcAll,SiteDayTable,by=c("doy","year","standID"),type="left")


#now fit the model
#note only loop through sitedayID through 146 because laste one only has 2 obs
#from the data download time and power issues

dataformodel<-list(Nobs=6243,NdaySiteID=146,Nstand=2,gs=gcforMod$gc,
					D=gcforMod$D,daySiteID=gcforMod$SiteDayID,
					ATemp=MDatforMod$Temp,PrecipTot=MDatforMod$WeekP,
					standID=MDatforMod$standID)
samplelist<-c("gref","S","a1","a2","a3","b1","b2","b3","sig.gs")					
gcmodI<-jags.model(file="c:\\Users\\hkropp\\Documents\\GitHub\\Density_sapflow\\gc_model\\gc_model_code.r",
						data=dataformodel,
						n.adapt=5000,
						n.chains=3)
						
gcmodS<-coda.samples(gcmodI,variable.names=samplelist,
                       n.iter=60000, thin=20)
		
					
mcmcplot(gcmodS, parms=c("gref","S","a1","a2","a3","b1","b2","b3","sig.gs"),
			dir="c:\\Users\\hkropp\\Google Drive\\Viper_SF\\model\\AGUtest")					

gcmodOut<-summary(gcmodS)

write.table(gcmodOut$statistics,"c:\\Users\\hkropp\\Google Drive\\Viper_SF\\model\\AGUtest\\stats.csv",
				sep=",",row.names=TRUE)
				
write.table(gcmodOut$quantiles,"c:\\Users\\hkropp\\Google Drive\\Viper_SF\\model\\AGUtest\\quants.csv",
				sep=",",row.names=TRUE)
 
rall<-cbind(gcmodOut$statistics,gcmodOut$quantiles)

rsub<-data.frame(Mean=rall[,1], pc2.5=rall[,5],pc97.5=rall[,9] )