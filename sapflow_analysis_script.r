######################################
##script for sapflow calculations ####
##and subsequent analysis for     ####
##density gradient sapflow        ####
######################################


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
##### read in sapwood thickness or sensor correciton
datSW <- read.csv("sap_thick.csv")

####met data for both sites
datLmet<-read.csv("LowD_met.csv")
datHmet<-read.csv("HighD_met.csv")
####Daily air pressure data from airport
datP<-read.csv("Pressure.csv")
#airport met data
datAD<-read.table("airport.csv", sep=";", head=TRUE, skip=6, stringsAsFactors=FALSE)
#pull out precip data
#more information for most recent
datAD2<-read.table("airport2.csv", sep=";", head=TRUE, skip=6, stringsAsFactors=FALSE)


#sensors 9-16 were run at the incorrect voltage
#and clearly have incorrect dT values for it
#so excluding from calculations
datH <- datDTH[,5:(4+8)]
datL <- datDTL[,5:dim(datDTL)[2]]

#set up date
dateDH <- as.Date(datDTH$TIMESTAMP, "%m/%d/%Y %H:%M")
dateDL <- as.Date(datDTL$TIMESTAMP, "%m/%d/%Y %H:%M")

doyDH <- yday(dateDH)
doyDL <- yday(dateDL)

#convert time
timeDH <- ifelse(datDTH$JHM/100-floor(datDTH$JHM/100)==0,datDTH$JHM/100,floor(datDTH$JHM/100)+.5)
timeDL <- ifelse(datDTL$JHM/100-floor(datDTL$JHM/100)==0,datDTL$JHM/100,floor(datDTL$JHM/100)+.5)


#need to create new doy index that defines day of year between 5am and 5am
datL$doy <- ifelse(timeDL<5,doyDL-1,doyDL)
datH$doy <- ifelse(timeDH<5,doyDH-1,doyDH)

#get the unique dayid for each stand
#get the number of observations in a day for each stand
DaysL <- aggregate(datL$doy, by=list(datL$doy), FUN="length")
colnames(DaysL) <- c("doy", "nobs")
DaysL$dayid <- seq(1,dim(DaysL)[1])

DaysH <- aggregate(datH$doy, by=list(datH$doy), FUN="length")
colnames(DaysH) <- c("doy", "nobs")
DaysH$dayid <-  seq(1, dim(DaysH)[1])	


###########
####calculate the max dT in a day
#low density
maxDTL <- matrix(rep(0,dim(DaysL)[1]*16),ncol=16)
#output matrix of the max per each day
for(j in 1:16){
	for(i in 1:dim(DaysL)[1]){
		maxDTL[i,j] <- max(na.omit(datL[datL$doy==DaysL$doy[i],j]))
	}
}
#calcuate high density
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

# now combine both
datAH <- join(datH,maxDTH, by=c("doy"), type="left")
datAL <- join(datL,maxDTL, by=c("doy"), type="left")

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
looks like sapwood thickness varies with stand
#predict the sapwood thickness for the trees that had sensors
datS$SWT <- ifelse(datS$stand=="high", coefficients(lmSWH)[1]+(coefficients(lmSWH)[2]*datS$DBH),
				coefficients(lmSWL)[1]+(coefficients(lmSWL)[2]*datS$DBH))

datS$Bark <- ifelse(datS$stand=="high", coefficients(lmBH)[1]+(coefficients(lmBH)[2]*datS$DBH),
				coefficients(lmBL)[1]+(coefficients(lmBL)[2]*datS$DBH))				
#calculate the heartwood 			
datS$Htwd <- datS$DBH-(datS$Bark*2)-(datS$SWT*2)
#calculate sapwood area
datS$sapA <- (pi*(((datS$SWT/2)+(datS$Htwd/2))^2))-(pi*((datS$Htwd/2)^2))			
								
#now calculate the porportion of the sensor in sapwood
SensDiff <- datS$Sensor.length-datS$SWT
	
#if value is negative, it means that the sapwood is thicker than the sensor length
#so it doesn't need to be corrected

#b represents the proption of the probe not in sapwood
datS$b <- ifelse(SensDiff>0,SensDiff/datS$Sensor.length,0)
datS$a <- 1-datS$b
#seperate df
datSH <- datS[datS$stand=="high",]
datSL <- datS[datS$stand=="low",]
#only sensors 1-8 were at the right voltage
datSH <- datSH[1:8,]

###########corrected dT value with sapwood
#now calcualte a corrected dt value based on sensor length
#note when b=0 and a=1 the dT corrected is equal to the raw dT
dTcorrL<-matrix(rep(NA,dim(datAL)[1]*16), ncol=16)
dTcorrH<-matrix(rep(NA,dim(datAH)[1]*8), ncol=8)
for(i in 1:16){
		dTcorrL[,i] <- (datAL[,i]-(datSL$b[i]*datAL[,16+i]))/datSL$a[i]	
}
for(i in 1:8){

		dTcorrH[,i]<- (datAH[,i]-(datSH$b[i]*datAH[,9+i]))/datSH$a[i]	
}

#T diff calculation
#Tmax-dtcor/dtcor
KL<-matrix(rep(NA,dim(datAL)[1]*16), ncol=16)
KH<-matrix(rep(NA,dim(datAH)[1]*8), ncol=8)
#low
for(i in 1:16){
		KL[,i] <- (datAL[,16+i]-dTcorrL[,i])/dTcorrL[,i]	
}
#high
for(i in 1:8){
		KH[,i] <- (datAH[,9+i]-dTcorrH[,i])/dTcorrH[,i]	
}
#calculate velocity in cm/ s
V.h<-matrix(rep(0,dim(KH)[1]*8), ncol=8)
V.l<-matrix(rep(0,dim(KL)[1]*16), ncol=16)
#high
for(i in 1:8){
	V.h[,i] <- ifelse(KH[,i]>=0,0.0119*(KH[,i]^1.231),NA)
}
#low	
for(i in 1:16){
	V.l[,i] <- ifelse(KL[,i]>=0,0.0119*(KL[,i]^1.231),NA)
}

######END Corrected velocity done with V.h and V.l ##############


#################################################################
###### calculate transpiration                  #################
#################################################################

#now calculate flow in  in g per s

F.h<-matrix(rep(0,dim(KH)[1]*8), ncol=8)
F.hf<-matrix(rep(0,dim(KH)[1]*8), ncol=8)
F.lf<-matrix(rep(0,dim(KL)[1]*16), ncol=16)
F.l<-matrix(rep(0,dim(KL)[1]*16), ncol=16)
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
#high density calcs
datSH$leafwt<-leaf.bio(datSH$DBH,40.5,1.41)	
datSH$leaf<-datSH$leafwt*143
datSH$leafm2<-datSH$leaf*.0001

########################################
########################################
########FILTER POINT2    ###############
########Quantile filter  ###############
########################################

#now calculate in g m-2 s
T.gL<-matrix(rep(0,dim(KL)[1]*16), ncol=16)
T.gH<-matrix(rep(0,dim(KH)[1]*8), ncol=8)
T.gLf<-matrix(rep(0,dim(KL)[1]*16), ncol=16)
T.gLf2<-matrix(rep(0,dim(KL)[1]*16), ncol=16)
T.gHf<-matrix(rep(0,dim(KH)[1]*8), ncol=8)
for(i in 1:8){
	T.gH[,i]<-F.hf[,i]/datSH$leafm2[i]
	T.gHf[,i]<-ifelse(T.gH[,i]<quantile(T.gH[,i],probs=c(0.95),na.rm=TRUE),T.gH[,i],NA)
	
}
for(i in 1:16){
	T.gL[,i]<-F.lf[,i]/datSL$leafm2[i]	
	T.gLf[,i]<-ifelse(T.gL[,i]<quantile(T.gL[,i],probs=c(0.95),na.rm=TRUE),T.gL[,i],NA)

}
#########End Transpiration (T) calcs ####################

#########################################################
######calculate stomatal conductance ####################
#########################################################

#join air pressure to each met table
datP2016<-datP[datP$year==2016,]
datPj<-data.frame(doy=datP2016$doy, Pkpa=datP2016$PdayGap)
datLmet<-join(datLmet, datPj, by="doy", type="left")
datHmet<-join(datHmet, datPj, by="doy", type="left")

#data frame of T in kg m-2 s-1
datLc1<-data.frame(doy=doyDL,hour=timeDL,T.gLf/1000)
datHc1<-data.frame(doy=doyDH,hour=timeDH,T.gHf/1000)

#join met data to T
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

########################################
########################################
########FILTER POINT 3   ###############
########Range filter     ###############
########################################
for(i in 1:8){
	Gshigh[,i]<-Gs.convert1(datHtkg$Kg,datHtkg[,i+2],datHtkg$D, datHtkg$Pkpa)
	Gshighmm[,i]<-unit.conv(Gshigh[,i],datHtkg$temp, datHtkg$Pkpa)*1000
	Gshighf[,i]<-ifelse(Gshighmm[,i]<400,Gshighmm[,i],NA)
}	
for(i in 1:16){	
	Gslow[,i]<-Gs.convert1(datLtkg$Kg,datLtkg[,i+2],datLtkg$D, datLtkg$Pkpa)
	Gslowmm[,i]<-unit.conv(Gslow[,i],datLtkg$temp, datLtkg$Pkpa)*1000
	Gslowf[,i]<-ifelse(Gslowmm[,i]<400,Gslowmm[,i],NA)
}

###########END of canopy stomatal conductance calc ############

###############################################################
###Filter for unreliable sap flow days ########################
###d

#header is shifted over 1 
#precip is RRR in reliable prognosis, but here it is in the Td column
Precip1 <- datAD$Td
Precip2 <- datAD2$Td
#no convert precip to numbers
#calling trace precipitation 0.01
PrecipF1 <- ifelse(Precip1 == "Trace of precipitation" ,"0.01" ,
			ifelse(Precip1 == "No precipitation", "0",
				ifelse(Precip1 == "",0 ,Precip1)))
				
PrecipN1<-as.numeric(PrecipF1)

PrecipF2 <- ifelse(Precip2 == "Trace of precipitation" ,"0.01" ,
			ifelse(Precip2 == "No precipitation", "0",
				ifelse(Precip2 == "",0 ,Precip1)))
				
PrecipN2 <- as.numeric(PrecipF2)

#now turn into a dataframe
PrecipDat <- data.frame(dateT=c(rownames(datAD2),rownames(datAD)),


