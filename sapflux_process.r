###########################################################################
###########################################################################
############## Created by Heather Kropp in October 2017      ##############
############## This script is to be run for all analyses     ##############
############## using transpriation (T) or canopy stomatal    ##############
############## conductance (gc)                              ##############
###########################################################################
###########################################################################
###########################################################################
############## Output data files:                            ##############
############## Transpiration: El.L,El.L17,El.H,El.H17        ##############
############## stomatal conductance:gc.L, gc.L17, gc.H, gc.H17#############
############## tree info: datTreeL, datTreeL17, datTreeH,     #############
##############            datTreeH17                          #############
###########################################################################
###########################################################################
library(lubridate)
library(plyr)
library(caTools)

#################################################################
## IMPORTANT: switch to flip on plot diagnostics               ##
##  and table summary of allometry for the manuscript          ##
## set to 1 to run the code to generate all diagnostic plots   ##
## set to 0 to skip plots if they have already been generated  ##
#################################################################
plotcheck <- 0
tableout <- 0


#################################################################
####specify directories                                   #######
#################################################################
#directory to save plot checks
diagP <- "c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\sapflux_diag"
#sub folders in diagnostics: maxT, aspectV, allometry, El, gc
#table directory
tableP <- "c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\sapflux_diag\\tables"

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

# read in larch SLA from stands
datSLA <- read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\individual_data\\sla.csv")

#density allometry
datAllom <- read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\individual_data\\larix_allom.csv")

# airport pressure and precip data
datAirP <- read.csv("c:\\Users\\hkropp\\Google Drive\\viperSensor\\airport\\airport.csv")

#canopy rh and temperature and PAR
datRH <- read.csv("c:\\Users\\hkropp\\Google Drive\\viperSensor\\met\\RH.VP4.csv")
datTC <- read.csv("c:\\Users\\hkropp\\Google Drive\\viperSensor\\met\\TempC.VP4.csv")

#read in tree information
datTree <- read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\individual_data\\treeIDmatch.csv")

#################################################################
####calculate maxdT                                       #######
#################################################################

#add time
datH$hour <- datDTH[,3]
datL$hour <- datDTL[,3]
datH17$hour <- datDTH17[,3]
datL17$hour <- datDTL17[,3]

#there were power isses with the low density around mid June-July 2017
#make sure that too little data isn't going into the maximum
#calculations because shut down at night
#so exclude data where there isn't at least 6 measurements at night
#because those days can sample too little of the maximum period

#create an index that indicates nighttime or daytime
datLdaytimeInd <-ifelse(datDTL$hour<5|datDTL$hour>=21,1,2)
datHdaytimeInd <-ifelse(datDTH$hour<5|datDTH$hour>=21,1,2)
datL17daytimeInd <-ifelse(datDTL17$hour<5|datDTL17$hour>=21,1,2)
datH17daytimeInd <-ifelse(datDTH17$hour<5|datDTH17$hour>=21,1,2)

#get maximum for each sesnor
LmaxTemp <- list()
LmaxDTA <- list()
LmaxTemp17 <- list()
LmaxDTA17 <- list()
HmaxTemp17 <- list()
HmaxDTA17 <- list()
LmaxtimeDayL <- list()
LmaxtimeDayL17 <- list()
HmaxtimeDayL <- list()
HmaxtimeDayL17 <- list()

LnightN <- list()
LnightN17 <- list()
HnightN <- list()
HnightN17 <- list()

LdaysAllN <- list()
LdaysAllN17 <- list()
HdaysAllN <- list()
HdaysAllN17 <- list()

LmaxDTAb <-list()
LmaxDTA17b <-list()
HmaxDTAb <-list()
HmaxDTA17b <-list()
for(i in 1:16){
	#ld 2016 omit NA
	LmaxTemp[[i]]<- na.omit(data.frame(dT=datL[,i],doy5=datL$doy5, daytimeInd=datLdaytimeInd))
	#get daily maximum
	LmaxDTA[[i]] <- aggregate(LmaxTemp[[i]]$dT, by=list(LmaxTemp[[i]]$doy5), FUN="max")
	colnames(LmaxDTA[[i]]) <- c("doy5", "mdT")
	#count number of day and night measurements
	LmaxtimeDayL[[i]] <- aggregate(LmaxTemp[[i]]$dT, by=list(LmaxTemp[[i]]$doy5, LmaxTemp[[i]]$daytimeInd), FUN="length")
	colnames(LmaxtimeDayL[[i]]) <- c("doy5","daytimeInd", "ndaytime")
	#join count of night measurements to all days
	LnightN[[i]] <- data.frame(doy5=LmaxtimeDayL[[i]]$doy5[LmaxtimeDayL[[i]]$daytimeInd==1],
								Nnight=LmaxtimeDayL[[i]]$ndaytime[LmaxtimeDayL[[i]]$daytimeInd==1])
	#join with full list of days
	LdaysAllN[[i]]<- join(LmaxDTA[[i]],LnightN[[i]], by="doy5", type="left")
	#reformat and exclude days with less than 6 obs at night
	LmaxDTAb[[i]] <- data.frame(doy5=LdaysAllN[[i]]$doy5,
								mdT=ifelse(LdaysAllN[[i]]$Nnight<6,NA,
								LdaysAllN[[i]]$mdT))
	colnames(LmaxDTAb[[i]]) <- c("doy5",paste0("mdT",i))
	
	#ld 2017
	LmaxTemp17[[i]]<- na.omit(data.frame(dT=datL17[,i],doy5=datL17$doy5, daytimeInd=datL17daytimeInd))
	#get daily maximum
	LmaxDTA17[[i]] <- aggregate(LmaxTemp17[[i]]$dT, by=list(LmaxTemp17[[i]]$doy5), FUN="max")
	colnames(LmaxDTA17[[i]]) <- c("doy5",  "mdT")
	#count number of day and night measurements
	LmaxtimeDayL17[[i]] <- aggregate(LmaxTemp17[[i]]$dT, by=list(LmaxTemp17[[i]]$doy5,LmaxTemp17[[i]]$daytimeInd), FUN="length")
	colnames(LmaxtimeDayL17[[i]]) <- c("doy5","daytimeInd", paste0("ndaytime",i))
	#join count of night measurements to all days
	LnightN17[[i]] <- data.frame(doy5=LmaxtimeDayL17[[i]]$doy5[LmaxtimeDayL17[[i]]$daytimeInd==1],
								Nnight=LmaxtimeDayL17[[i]]$ndaytime[LmaxtimeDayL17[[i]]$daytimeInd==1])
	#join with full list of days
	LdaysAllN17[[i]]<- join(LmaxDTA17[[i]],LnightN17[[i]], by="doy5", type="left")
	#reformat and exclude days with less than 6 obs at night
	LmaxDTA17b[[i]] <- data.frame(doy5=LdaysAllN17[[i]]$doy5,
								mdT=ifelse(LdaysAllN17[[i]]$Nnight<6,NA,
								LdaysAllN17[[i]]$mdT))
	colnames(LmaxDTA17b[[i]]) <- c("doy5",paste0("mdT",i))
	#hd 2017
	HmaxTemp17[[i]]<- na.omit(data.frame(dT=datH17[,i],doy5=datH17$doy5, daytimeInd=datH17daytimeInd))
	#get daily maximum
	HmaxDTA17[[i]] <- aggregate(HmaxTemp17[[i]]$dT, by=list(HmaxTemp17[[i]]$doy5), FUN="max")
	colnames(HmaxDTA17[[i]]) <- c("doy5",  "mdT")
	#count number of day and night measurements
	HmaxtimeDayL17[[i]] <- aggregate(HmaxTemp17[[i]]$dT, by=list(HmaxTemp17[[i]]$doy5, HmaxTemp17[[i]]$daytimeInd), FUN="length")
	colnames(HmaxtimeDayL17[[i]]) <- c("doy5","daytimeInd", paste0("ndaytime",i))
	#join count of night measurements to all days
	HnightN17[[i]] <- data.frame(doy5=HmaxtimeDayL17[[i]]$doy5[HmaxtimeDayL17[[i]]$daytimeInd==1],
								Nnight=HmaxtimeDayL17[[i]]$ndaytime[HmaxtimeDayL17[[i]]$daytimeInd==1])
	#join with full list of days
	HdaysAllN17[[i]]<- join(HmaxDTA17[[i]],HnightN17[[i]], by="doy5", type="left")
	#reformat and exclude days with less than 6 obs at night
	HmaxDTA17b[[i]] <- data.frame(doy5=HdaysAllN17[[i]]$doy5,
								mdT=ifelse(HdaysAllN17[[i]]$Nnight<6,NA,
								HdaysAllN17[[i]]$mdT))
	colnames(HmaxDTA17b[[i]]) <- c("doy5",paste0("mdT",i))
	
	}
#hd 2016
#get maximum for each sesnor
HmaxTemp <- list()
HmaxDTA <- list()
for(i in 1:8){	
	HmaxTemp[[i]]<- na.omit(data.frame(dT=datH[,i],doy5=datH$doy5, daytimeInd=datHdaytimeInd))
	#get daily maximum
	HmaxDTA[[i]] <- aggregate(HmaxTemp[[i]]$dT, by=list(HmaxTemp[[i]]$doy5), FUN="max")
	colnames(HmaxDTA[[i]]) <- c("doy5",   "mdT")
	#count number of day and night measurements
	HmaxtimeDayL[[i]] <- aggregate(HmaxTemp[[i]]$dT, by=list(HmaxTemp[[i]]$doy5, HmaxTemp[[i]]$daytimeInd), FUN="length")
	colnames(HmaxtimeDayL[[i]]) <- c("doy5","daytimeInd", paste0("ndaytime",i))	
	#join count of night measurements to all days
	HnightN[[i]] <- data.frame(doy5=HmaxtimeDayL[[i]]$doy5[HmaxtimeDayL[[i]]$daytimeInd==1],
								Nnight=HmaxtimeDayL[[i]]$ndaytime[HmaxtimeDayL[[i]]$daytimeInd==1])
	#join with full list of days
	HdaysAllN[[i]]<- join(HmaxDTA[[i]],HnightN[[i]], by="doy5", type="left")
	#reformat and exclude days with less than 6 obs at night
	HmaxDTAb[[i]] <- data.frame(doy5=HdaysAllN[[i]]$doy5,
								mdT=ifelse(HdaysAllN[[i]]$Nnight<6,NA,
								HdaysAllN[[i]]$mdT))
	colnames(HmaxDTAb[[i]]) <- c("doy5",paste0("mdT",i))
}
	
#join the daily maximums back into a dataframe
LmaxDTA2 <- join_all(LmaxDTAb, by="doy5", type="full")	
LmaxDTA172 <- join_all(LmaxDTA17b, by="doy5", type="full")
HmaxDTA2 <- join_all(HmaxDTAb, by="doy5", type="full")
HmaxDTA172 <- join_all(HmaxDTA17b, by="doy5", type="full")	
	
	
	
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

#################################################################
####match up trees by year                                #######
#################################################################
#change treeID colnames

colnames(datS)[2] <- "treeID.original"
datS$stand <- ifelse(datS$stand=="high","hd","ld")

#reformat datS17 to match datS
datS17b <- data.frame(Sensor=datS17$Sensor.., treeID.original=datS17$Tree.., DBH=datS17$DBH..cm.,
			Aspect=datS17$Aspect, Sensor.length=datS17$Sensor.length, stand=datS17$stand)


datS$year <- rep(2016, dim(datS)[1])
datS17b$year <- rep(2017, dim(datS17b)[1])
#combine into one
datSALL <- rbind(datS, datS17b)
#subset to exclude sensors that sapflux wasn't used on in 2016 hd


Toomit <- ifelse(datSALL$stand=="hd"&datSALL$year==2016&datSALL$treeID.original>7,1,0)
datSALL <- datSALL[Toomit==0,]

#now join with the matching table
datTreeALL <- join( datSALL,datTree, by=c("year","stand","treeID.original"), type="left")

#now aggregate the for the new dbH on trees that may have different measurements
#from different people
TreeDBHmean <- aggregate(datTreeALL$DBH[datTreeALL$Aspect=="N"], 
						by=list(datTreeALL$treeID.new[datTreeALL$Aspect=="N"],datTreeALL$stand[datTreeALL$Aspect=="N"]), FUN="mean")
colnames(TreeDBHmean) <-c("treeID.new","stand","DBHt")

datTreeDF <- join(datTreeALL, TreeDBHmean, by=c("treeID.new","stand"), type="left")




#relationship is not significant for low density sap thickness so just use mean
#looks like sapwood thickness varies with stand
#predict the sapwood thickness for the trees that had sensors
datTreeDF $SWT <- ifelse(datTreeDF$stand=="hd", coefficients(lmSWH)[1]+(coefficients(lmSWH)[2]*datTreeDF$DBHt),
				mean(datSW$SWT[datSW$stand=="LDF2"]))

datTreeDF $Bark <- ifelse(datTreeDF$stand=="hd", coefficients(lmBH)[1]+(coefficients(lmBH)[2]*datTreeDF$DBHt),
				coefficients(lmBL)[1]+(coefficients(lmBL)[2]*datTreeDF$DBHt))		

				
#calculate the heartwood 			
datTreeDF$Htwd <- datTreeDF$DBHt-(datTreeDF$Bark*2)-(datTreeDF$SWT*2)


#calculate sapwood area
datTreeDF$sapA <- (pi*(((datTreeDF$SWT/2)+(datTreeDF$Htwd/2))^2))-(pi*((datTreeDF$Htwd/2)^2))

	
								
#now calculate the porportion of the sensor in sapwood
SensDiff <- datTreeDF$Sensor.length-datTreeDF$SWT
	

#if value is negative, it means that the sapwood is thicker than the sensor length
#so it doesn't need to be corrected

#b represents the proption of the probe not in sapwood
datTreeDF$b <- ifelse(SensDiff>0,SensDiff/datTreeDF$Sensor.length,0)
datTreeDF$a <- 1-datTreeDF$b


#seperate df
datSH <- datTreeDF[datTreeDF$stand=="hd"&datTreeDF$year==2016,]
datSL <- datTreeDF[datTreeDF$stand=="ld"&datTreeDF$year==2016,]

datSH17 <- datTreeDF[datTreeDF$stand=="hd"&datTreeDF$year==2017,]
datSL17 <- datTreeDF[datTreeDF$stand=="ld"&datTreeDF$year==2017,]


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

# now calculate flow in  in g per s by accounting for sapwood area
# that the flow is occuring over and include the correction for 
# different sapflow on the south side. Here we assume
# that half of the sapwood area is a flow rate that is given by the
# the south side

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
	F.l[,i]<-(V.l[,i]*(0.5*datSL$sapA[i])) + ((AspNL$coefficients[1]+(AspNL$coefficients[2]*V.l[,i]))*(0.5*datSL$sapA[i]))
	F.lf[,i]<-ifelse(F.l[,i]<quantile(F.l[,i],probs=c(0.95),na.rm=TRUE),F.l[,i],NA)
	F.l17[,i]<-(V.l17[,i]*(0.5*datSL$sapA[i])) + ((AspNL$coefficients[1]+(AspNL$coefficients[2]*V.l17[,i]))*(0.5*datSL$sapA[i]))
	F.lf17[,i]<-ifelse(F.l17[,i]<quantile(F.l17[,i],probs=c(0.95),na.rm=TRUE),F.l17[,i],NA)	
	F.h17[,i]<-(V.h17[,i]*(0.5*datSH17$sapA[i])) + ((AspNH$coefficients[1]+ (AspNH$coefficients[1]*V.h17[,i]))*(0.5*datSH17$sapA[i]))
	F.hf17[,i]<-ifelse(F.h17[,i]<quantile(F.h17[,i],probs=c(0.95),na.rm=TRUE),F.h17[,i],NA)
}	
for(i in 1:8){

	F.h[,i]<-(V.h[,i]*(0.5*datSH$sapA[i])) + ((AspNH$coefficients[1]+ (AspNH$coefficients[1]*V.h[,i]))*(0.5*datSH$sapA[i]))
	F.hf[,i]<-ifelse(F.h[,i]<quantile(F.h[,i],probs=c(0.95),na.rm=TRUE),F.h[,i],NA)
	}
#################################################################
###### normalize by leaf area                   #################
#################################################################	
#leaf allometry function 
leaf.bio<-function(DBH,a.leaf,b.leaf){a.leaf*(DBH^b.leaf)}
#fit nonlinear function
nlsLow <- nls(leaf~a.leaf*(DBH^b.leaf), data=list(DBH=datAllom$dbh[datAllom$density=="Low"],
				leaf=datAllom$foliage[datAllom$density=="Low"]),
				start=list(a.leaf=40.5, b.leaf=1.41))
nlsHigh <- nls(leaf~a.leaf*(DBH^b.leaf), data=list(DBH=datAllom$dbh[datAllom$density=="High"],
				leaf=datAllom$foliage[datAllom$density=="High"]),
				start=list(a.leaf=40.5, b.leaf=1.41))


#plot stand allometry
#starting values from Alexander 2012
if(plotcheck==1){
	jpeg(file=paste0(diagP, "\\allometry\\dbhL.jpg"), width=1500, height=1000, units="px")
	par(mfrow=c(1,2))
	plot(datAllom$dbh[datAllom$density=="High"],datAllom$foliage[datAllom$density=="High"],
			pch=19, xlab = "dbh (cm)", ylab=" Canopy leaf mass (g)", main="High")
	points(seq(0,30, by=.1), leaf.bio(seq(0,30, by=.1), summary(nlsHigh)$coefficients[1,1],
				summary(nlsHigh)$coefficients[2,1]), type="l", col="red",lwd=2)
			
	plot(datAllom$dbh[datAllom$density=="Low"],datAllom$foliage[datAllom$density=="Low"],
			pch=19, xlab = "dbh (cm)", ylab=" Canopy leaf mass (g)", main="Low")
	points(seq(0,30, by=.1), leaf.bio(seq(0,30, by=.1), summary(nlsLow)$coefficients[1,1],
				summary(nlsLow)$coefficients[2,1]), type="l", col="red",lwd=2)		
			
dev.off()
}


#calculated expected canopy mass in g based on dbh of tree
datSL$leafwt<-leaf.bio(datSL$DBHt,summary(nlsLow)$coefficients[1,1],summary(nlsLow)$coefficients[2,1])	
datSL17$leafwt<-leaf.bio(datSL17$DBHt,summary(nlsLow)$coefficients[1,1],summary(nlsLow)$coefficients[2,1])	
datSH$leafwt<-leaf.bio(datSH$DBHt,summary(nlsHigh)$coefficients[1,1],summary(nlsHigh)$coefficients[2,1])	
datSH17$leafwt<-leaf.bio(datSH17$DBHt,summary(nlsHigh)$coefficients[1,1],summary(nlsHigh)$coefficients[2,1])	

#calculate stand specific sla
datSLA$SLA <- datSLA$leaf.area/datSLA$mass
lowSLA <-mean(datSLA$SLA[datSLA$stand=="ld"])
highSLA <-mean(datSLA$SLA[datSLA$stand=="hd"])
#larch cm2/g
datSL$leaf<-datSL$leafwt*lowSLA
datSL17$leaf<-datSL17$leafwt*lowSLA
datSH$leaf<-datSH$leafwt*highSLA
datSH17$leaf<-datSH17$leafwt*highSLA

#convert to m2 
datSL$leafm2<-datSL$leaf*.0001	
datSL17$leafm2<-datSL17$leaf*.0001
datSH$leafm2<-datSH$leaf*.0001
datSH17$leafm2<-datSH17$leaf*.0001	

#################################################################
###### create output table for allometry and     ################
###### other canopy and tree metrics             ################
#################################################################	


if(plotcheck==1){
	#plot sapwood allometry values 
	jpeg(file=paste0(diagP, "\\allometry\\sapwood.jpg"), width=1500, height=1500, units="px")
	par(mfrow=c(2,2), mai=c(1,1.5,1,1))
	plot(datSW$DBH[datSW$stand=="LDF2"],datSW$SWT[datSW$stand=="LDF2"],pch=19, xlab="Diameter breast height (cm)",
			ylab="sapwood thickness (cm)",col="grey0", cex=2, cex.axis=2, cex.lab=2.5, cex.main=2.5,main="low density") 
	abline(lmSWL)
	text(10,1.25, paste("sap =",round(coefficients(lmSWL)[1],3),"+",round(coefficients(lmSWL)[2],3),"*DBH"), cex=3)
	text(10,1.15, paste("Rsquared=", round(summary(lmSWL)$r.squared,3)), cex=3)
	plot(datSW$DBH[datSW$stand=="DAV"],datSW$SWT[datSW$stand=="DAV"],pch=19, xlab="Diameter breast height (cm)",
			ylab="sapwood thickness (cm)", cex=2, cex.axis=2, cex.lab=2.5,cex.main=2.5, main="high density") 	
	text(6,1.2, paste("sap =",round(coefficients(lmSWH)[1],3),"+",round(coefficients(lmSWH)[2],3),"*DBH"), cex=3)
	text(6,1.1, paste("Rsquared=", round(summary(lmSWH)$r.squared,3)), cex=3)
	abline(lmSWH)
	plot(datSW$DBH[datSW$stand=="LDF2"],datSW$Bark[datSW$stand=="LDF2"],pch=19, xlab="Diameter breast height (cm)",
			ylab="bark thickness (cm)", cex=2, cex.axis=2, cex.lab=2.5,cex.main=2.5, main="low density")

	abline(lmBL)
	text(10,.7, paste("sap =",round(coefficients(lmBL)[1],3),"+",round(coefficients(lmBL)[2],3),"*DBH"), cex=3)
	text(10,.6, paste("Rsquared=", round(summary(lmBL)$r.squared,3)), cex=3)		
	
	plot(datSW$DBH[datSW$stand=="DAV"],datSW$Bark[datSW$stand=="DAV"],pch=19, xlab="Diameter breast height (cm)",
			ylab="bark thickness (cm)", cex=2, cex.axis=2, cex.lab=2.5,cex.main=2.5, main="high density") 	
	abline(lmBH)
	text(6,.7, paste("sap =",round(coefficients(lmBH)[1],3),"+",round(coefficients(lmBH)[2],3),"*DBH"), cex=3)
	text(6,.6, paste("Rsquared=", round(summary(lmBH)$r.squared,3)), cex=3)	
	
	dev.off()
	#output the allometry values used
	

}
#output a table of allometry coefficients
if(tableout==1){
	#sapwood thickness
	tabSH <- data.frame(summary(lmSWH)$coefficients)
	tabSH$site <- rep("hd", dim(tabSH)[1])
	tabSH$r.sq <- rep(summary(lmSWH)$r.squared, dim(tabSH)[1])
	tabSL <- data.frame(summary(lmSWL)$coefficients)
	tabSL$site <- rep("ld", dim(tabSL)[1])	
	tabSL$r.sq <- rep(summary(lmSWL)$r.squared, dim(tabSL)[1])
	sapThickC <- rbind(tabSH,tabSL)

	write.table(sapThickC,paste0(tableP,"\\sapwoodAllom.csv"),sep=",", row.names=TRUE)
	#sapwood to leaf mass
	tabLH <- data.frame(summary(nlsHigh)$parameters)
	tabLH$site <- rep("hd", dim(tabLH)[1])
	tabLH$iterN <- rep(summary(nlsHigh)$convInfo$finIter, dim(tabLH)[1])
	tabLH$finTol <- rep(summary(nlsHigh)$convInfo$finTol, dim(tabLH)[1])
	tabLL <- data.frame(summary(nlsLow)$parameters)
	tabLL$site <- rep("hd", dim(tabLL)[1])
	tabLL$iterN <- rep(summary(nlsLow)$convInfo$finIter, dim(tabLL)[1])
	tabLL$finTol <- rep(summary(nlsLow)$convInfo$finTol, dim(tabLL)[1])	
	
	leafWeightC <- rbind(tabLH,tabLL)
	write.table(leafWeightC,paste0(tableP,"\\leafAllom.csv"),sep=",", row.names=TRUE)
	
	
}


#summarize the sensor trees
# just focus on 2017
#many of the trees should be the same
#in high density, but there are a few that
#likely aren't. Low density all but 1 should be the same
#since there aren't more than 14 trees in distance of the sensors
if(tableout==1){
	#get the average sapoowd thickness
	#don't double average trees with two sensors
	canopySummLt <- datSL17[datSL17$Aspect=="N",]
	canopySummHt <- datSH17[datSH17$Aspect=="N",]
	canopySummLt$S.Lrat <- canopySummLt$leafm2/canopySummLt$sapA
	canopySummHt$S.Lrat <- canopySummHt$leafm2/canopySummHt$sapA
	#leaf SLA relationships cm2/g: lowSLA, highSLA, mean(datSLA$SLA[datSLA$stand=="ld"])
	treeMetric <- data.frame(site=c("LD","HD"),SLA.mean.cm.g=c(lowSLA,highSLA), SLA.sd = c(sd(datSLA$SLA[datSLA$stand=="ld"]),
					sd(datSLA$SLA[datSLA$stand=="hd"])), SLA.n =c(length(datSLA$SLA[datSLA$stand=="ld"]),
					length(datSLA$SLA[datSLA$stand=="hd"])),
					canLeaf.m2 = c(mean(canopySummLt$leafm2),mean(canopySummHt$leafm2)),
					canLeaf.sd =c(sd(canopySummLt$leafm2),sd(canopySummHt$leafm2)),
					canLeaf.n = c(length(canopySummLt$leafm2),length(canopySummHt$leafm2)),
					sapA.cm2 =c(mean(canopySummLt$sapA),mean(canopySummHt$sapA)),
					sapA.sd=c(sd(canopySummLt$sapA),sd(canopySummHt$sapA)),
					sapA.n=c(length(canopySummLt$sapA),length(canopySummHt$sapA)),
					LSrat =c(mean(canopySummLt$S.Lrat),mean(canopySummHt$S.Lrat)),
					LSrat.sd =c(sd(canopySummLt$S.Lrat),sd(canopySummHt$S.Lrat)),
					LSrat.n=c(length(canopySummLt$S.Lrat),length(canopySummHt$S.Lrat)))
					
	write.table(treeMetric,paste0(tableP,"\\treeSummary.csv"),sep=",", row.names=FALSE)
}


#################################################################
###### calculate flow in g m-2 s-1              #################
#################################################################


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

#calculate T in g m-2 s-1
for(i in 1:8){
	T.gH[,i]<-F.hf[,i]/datSH$leafm2[i]
}	
	
for(i in 1:16){
	T.gL[,i]<-F.lf[,i]/datSL$leafm2[i]	
	T.gL17[,i]<-F.lf17[,i]/datSL17$leafm2[i]	
	T.gH17[,i]<-F.hf17[,i]/datSH17$leafm2[i]	
}	
#get quantile filters
HQ <- quantile(c(as.vector(T.gH),as.vector(T.gH17)), probs=c(0.95),na.rm=TRUE)
LQ <- quantile(c(as.vector(T.gL),as.vector(T.gL17)), probs=c(0.95),na.rm=TRUE)
for(i in 1:8){	
	T.gHf[,i]<-ifelse(T.gH[,i]<HQ,T.gH[,i],NA)

}

for(i in 1:16){
	T.gLf17[,i]<-ifelse(T.gL17[,i]<LQ,T.gL17[,i],NA)		
	T.gLf[,i]<-ifelse(T.gL[,i]<LQ,T.gL[,i],NA)	
	T.gHf17[,i]<-ifelse(T.gH17[,i]<HQ,T.gH17[,i],NA)
}

#remove south facing measurements
AllSeqSens <- seq(1,16)
AllSeqSensH <- seq(1,8)
NorthLkeep <- AllSeqSens[!AllSeqSens%in%LSouthS]
NorthL17keep <- AllSeqSens[!AllSeqSens%in%LSouth17S]
NorthHkeep <- AllSeqSensH[!AllSeqSensH%in%HSouthS]
NorthH17keep <- AllSeqSens[!AllSeqSens%in%HSouth17S]

#create a new sensor list to accompany all of these changes
datTreeL <- datSL[datSL$Aspect=="N",]
datTreeH <- datSH[datSH$Aspect=="N"&datSH$Sensor<=8,]
datTreeL17 <- datSL17[datSL17$Aspect=="N",]
datTreeH17 <- datSH17[datSH17$Aspect=="N",]
#create a dataframe
El.H<-data.frame(doy=datDTH$doy, year=rep(2016,length(datDTH$doy)),hour=datDTH$hour, T.gHf[,NorthHkeep])

El.L<-data.frame(doy=datDTL$doy, year=rep(2016,length(datDTL$doy)),hour=datDTL$hour,T.gLf[,NorthLkeep])

El.H17<-data.frame(doy=datDTH17$doy, year=rep(2017,length(datDTH17$doy)),hour=datDTH17$hour, T.gHf17[,NorthH17keep])

El.L17<-data.frame(doy=datDTL17$doy, year=rep(2017,length(datDTL17$doy)),hour=datDTL17$hour,T.gLf17[,NorthL17keep])

if(plotcheck==1){
	for(i in 1:length(NorthHkeep)){
		jpeg(file=paste0(diagP, "\\El\\high2016sensor",NorthHkeep[i],".jpg"), width=4000, height=1000, units="px")
			plot(El.H$doy+(El.H$hour/24),El.H[,(i+3)], type="b", pch=19, xlab= "Doy", ylab="Transpiration (g m-2 s-1)",
			main=paste("sensor",NorthHkeep[i]))
		dev.off()
	}
		for(i in 1:length(NorthH17keep)){
		jpeg(file=paste0(diagP, "\\El\\high2017sensor",NorthH17keep[i],".jpg"), width=4000, height=1000, units="px")
			plot(El.H17$doy+(El.H17$hour/24),El.H17[,(i+3)], type="b", pch=19, xlab= "Doy", ylab="Transpiration (g m-2 s-1)",
			main=paste("sensor",NorthH17keep[i]))
		dev.off()
	}
	
	for(i in 1:length(NorthL17keep)){
		jpeg(file=paste0(diagP, "\\El\\low2017sensor",NorthL17keep[i],".jpg"), width=4000, height=1000, units="px")
			plot(El.L17$doy+(El.L17$hour/24),El.L17[,(i+3)], type="b", pch=19, xlab= "Doy", ylab="Transpiration (g m-2 s-1)",
			main=paste("sensor",NorthL17keep[i]))
		dev.off()
	}
	for(i in 1:length(NorthLkeep)){
		jpeg(file=paste0(diagP, "\\El\\low2016sensor",NorthLkeep[i],".jpg"), width=4000, height=1000, units="px")
			plot(El.L$doy+(El.L$hour/24),El.L[,(i+3)], type="b", pch=19, xlab= "Doy", ylab="Transpiration (g m-2 s-1)",
			main=paste("sensor",NorthLkeep[i]))
		dev.off()
	}
}



#########End Transpiration (T) calcs ####################



#################################################################
###### calculate canopy stomatal conductance    #################
#################################################################

#convert transpiration to kg m-2 
datLc1<-data.frame(El.L[,1:3], El.L[,4:(3+length(NorthLkeep))]/1000)
datL17c1<-data.frame(El.L17[,1:3], El.L17[,4:(3+length(NorthL17keep))]/1000)
datHc1<-data.frame(El.H[,1:3], El.H[,4:(3+length(NorthHkeep))]/1000)
datH17c1<-data.frame(El.H17[,1:3], El.H17[,4:(3+length(NorthH17keep))]/1000)

#only canopy met was pulled
#subset and match
datLRHmet <- data.frame(datRH[datRH$site=="ld",1:3], RH=datRH$RH.VP4[datRH$site=="ld"])
datLTCmet<- data.frame(datTC[datTC$site=="ld",1:3], Temp=datTC$TempC.VP4[datTC$site=="ld"])

datHRHmet <- data.frame(datRH[datRH$site=="hd",1:3], RH=datRH$RH.VP4[datRH$site=="hd"])
datHTCmet <- data.frame(datTC[datTC$site=="hd",1:3], Temp=datTC$TempC.VP4[datTC$site=="hd"])
#join temp and RH
datLmet <- join(datLRHmet, datLTCmet, by=c("doy","year","hour"),type="inner")
datHmet <- join(datHRHmet, datHTCmet, by=c("doy","year","hour"),type="inner")


#join airport data to each table

datLmet <- join(datLmet, datAirP, by=c("doy","year"), type="left")
datHmet <- join(datHmet, datAirP, by=c("doy","year"), type="left")


datLmet$RH.fix <-ifelse(datLmet$RH>=1,.999,datLmet$RH)
datLmet$e.sat<-0.611*exp((17.502*datLmet$Temp)/(datLmet$Temp+240.97))
datLmet$D<-(datLmet$e.sat-(datLmet$RH.fix*datLmet$e.sat))

datHmet$RH.fix <-ifelse(datHmet$RH>=1,.999,datHmet$RH)
datHmet$e.sat<-0.611*exp((17.502*datHmet$Temp)/(datHmet$Temp+240.97))
datHmet$D<-(datHmet$e.sat-(datHmet$RH.fix*datHmet$e.sat))






#################################################################
###### check for hysteresis                     #################
#################################################################


if(plotcheck==1){
datLmetD1 <- data.frame(doy=ifelse(datLmet$hour==0,datLmet$doy-1,datLme$doy), year=datLmet$year, hour=datLmet$hour-.5, D.D1=datLmet$D)
datLmetD2 <- data.frame(doy=ifelse(datLme$hour<=1,datLmet$doy-1,datLmet$doy), year=datLmet$year, hour=datLmet$hour-1, D.D2=datLmet$D)
datLmetD3 <- data.frame(doy=ifelse(datLme$hour<=1.5,datLmet$doy-1,datLmet$doy), year=datLmet$year, hour=datLmet$hour-1.5, D.D3=datLmet$D)

datHmetD1 <- data.frame(doy=ifelse(datHmet$hour==0,datHmet$doy-1,datHmet$doy), year=datHmet$year, hour=datHmet$hour-.5, D.D1=datHmet$D)
datHmetD2 <- data.frame(doy=ifelse(datHmet$hour<=1,datHmet$doy-1,datHmet$doy), year=datHmet$year, hour=datHmet$hour-1, D.D2=datHmet$D)
datHmetD3 <- data.frame(doy=ifelse(datHmet$hour<=1.5,datHmet$doy-1,datHmet$doy), year=datHmet$year, hour=datHmet$hour-1.5, D.D3=datHmet$D)

datLhystT <- list(datLmet,datLmetD1,datLmetD2,datLmetD3 )
datHhystT <- list(datHmet,datHmetD1,datHmetD2,datHmetD3 )

datLhyst <- join_all(datLhystT, by=c("doy","year","hour"), type="left")
datHhyst <- join_all(datHhystT, by=c("doy","year","hour"), type="left")

#join met data into each
datELmet <- join(El.L,datLhyst, by=c("doy","year","hour"), type="left")
datEHmet <- join(El.H,datHhyst, by=c("doy","year","hour"), type="left")
datELmet17 <- join(El.L17,datLhyst, by=c("doy","year","hour"), type="left")
datEHmet17 <- join(El.H17,datHhyst, by=c("doy","year","hour"), type="left")

ElUdays <- unique(datELmet$doy)


j<-7	
	
	for(i in 1:13){
	#look at an example for a hystereses loop
	jpeg(file=paste0(diagP, "\\hysteresis\\low2016day",ElUdays[j],"sensor",i,".jpg"), width=1500, height=1000, units="px")
	plot(c(0,1),c(0,1), ylim=c(0,0.01), xlim=c(0,1.5),type="n", pch=19, xlab= "D", ylab="Transpiration (g m-2 s-1)",
			main=paste("doy",ElUdays[i]))
	
		points(datELmet$D[datELmet$doy==ElUdays[j]],datELmet[datELmet$doy==ElUdays[j],(i+3)], col="black", pch=19, type="b")
		points(datELmet$D.D1[datELmet$doy==ElUdays[j]],datELmet[datELmet$doy==ElUdays[j],(i+3)], col="red", pch=19)
		points(datELmet$D.D2[datELmet$doy==ElUdays[j]],datELmet[datELmet$doy==ElUdays[j],(i+3)], col="cornflowerblue", pch=19)	
		points(datELmet$D.D3[datELmet$doy==ElUdays[j]],datELmet[datELmet$doy==ElUdays[j],(i+3)], col="darkgreen", pch=19)
			
		

	dev.off()
	}
	jpeg(file=paste0(diagP, "\\hysteresis\\allLLow.jpg"), width=1500, height=1000, units="px")
	par(mfrow=c(1,2))
	plot(datELmet$D,rowMeans(datELmet[,4:17], na.rm=TRUE), pch=19)
	points(datELmet$D.D1,rowMeans(datELmet[,4:17], na.rm=TRUE), pch=19, col="orangered3")
	points(datELmet$D.D2,rowMeans(datELmet[,4:17], na.rm=TRUE), pch=19, col="cornflowerblue")
	points(datELmet$D.D3,rowMeans(datELmet[,4:17], na.rm=TRUE), pch=19, col="darkgreen")	
	
	plot(log(datELmet$D[datELmet$D>.6]),rowMeans(datELmet[,4:17], na.rm=TRUE)[datELmet$D>.6], pch=19)
	points(log(datELmet$D.D1[datELmet$D.D1>.6]),rowMeans(datELmet[,4:17], na.rm=TRUE)[datELmet$D.D1>.6], pch=19, col="orangered3")
	points(log(datELmet$D.D2[datELmet$D.D2>.6]),rowMeans(datELmet[,4:17], na.rm=TRUE)[datELmet$D.D2>.6], pch=19, col="cornflowerblue")
	points(log(datELmet$D.D3[datELmet$D.D3>.6]),rowMeans(datELmet[,4:17], na.rm=TRUE)[datELmet$D.D3>.6], pch=19, col="darkgreen")
	dev.off()
	
	
		jpeg(file=paste0(diagP, "\\hysteresis\\allHigh.jpg"), width=1500, height=1000, units="px")
	par(mfrow=c(1,2))
	plot(datEHmet$D,rowMeans(datEHmet[,4:10]), pch=19)
	points(datEHmet$D.D1,rowMeans(datEHmet[,4:10]), pch=19, col="orangered3")
	points(datEHmet$D.D2,rowMeans(datEHmet[,4:10]), pch=19, col="cornflowerblue")
	points(datEHmet$D.D3,rowMeans(datEHmet[,4:10]), pch=19, col="darkgreen")	
	
	plot(log(datEHmet$D[datEHmet$D>.6]),rowMeans(datEHmet[,4:10], na.rm=TRUE)[datEHmet$D>.6], pch=19)
	points(log(datEHmet$D.D1[datEHmet$D.D1>.6]),rowMeans(datEHmet[,4:10], na.rm=TRUE)[datEHmet$D.D1>.6], pch=19, col="orangered3")
	points(log(datEHmet$D.D2[datEHmet$D.D2>.6]),rowMeans(datEHmet[,4:10], na.rm=TRUE)[datEHmet$D.D2>.6], pch=19, col="cornflowerblue")
	points(log(datEHmet$D.D3[datEHmet$D.D3>.6]),rowMeans(datEHmet[,4:10], na.rm=TRUE)[datEHmet$D.D3>.6], pch=19, col="darkgreen")
	dev.off()
	
	
		jpeg(file=paste0(diagP, "\\hysteresis\\allHigh17.jpg"), width=1500, height=1000, units="px")
	par(mfrow=c(1,2))
	plot(datEHmet17$D,rowMeans(datEHmet17[,4:17]), pch=19)
	points(datEHmet17$D.D1,rowMeans(datEHmet17[,4:17], na.rm=TRUE), pch=19, col="orangered3")
	points(datEHmet17$D.D2,rowMeans(datEHmet17[,4:17], na.rm=TRUE), pch=19, col="cornflowerblue")
	points(datEHmet17$D.D3,rowMeans(datEHmet17[,4:17], na.rm=TRUE), pch=19, col="darkgreen")	
	
	plot(log(datEHmet17$D[datEHmet17$D>.6]),rowMeans(datEHmet17[,4:17], na.rm=TRUE)[datEHmet17$D>.6], pch=19)
	points(log(datEHmet17$D.D1[datEHmet17$D.D1>.6]),rowMeans(datEHmet17[,4:17], na.rm=TRUE)[datEHmet17$D.D1>.6], pch=19, col="orangered3")
	points(log(datEHmet17$D.D2[datEHmet17$D.D2>.6]),rowMeans(datEHmet17[,4:17], na.rm=TRUE)[datEHmet17$D.D2>.6], pch=19, col="cornflowerblue")
	points(log(datEHmet17$D.D3[datEHmet17$D.D3>.6]),rowMeans(datEHmet17[,4:17], na.rm=TRUE)[datEHmet17$D.D3>.6], pch=19, col="darkgreen")
	dev.off()	
	
	jpeg(file=paste0(diagP, "\\hysteresis\\allHigh17.jpg"), width=1500, height=1000, units="px")
	par(mfrow=c(1,2))
	plot(datELmet17$D,rowMeans(datELmet17[,4:17]), pch=19)
	points(datELmet17$D.D1,rowMeans(datELmet17[,4:17], na.rm=TRUE), pch=19, col="orangered3")
	points(datELmet17$D.D2,rowMeans(datELmet17[,4:17], na.rm=TRUE), pch=19, col="cornflowerblue")
	points(datELmet17$D.D3,rowMeans(datELmet17[,4:17], na.rm=TRUE), pch=19, col="darkgreen")	
	
	plot(log(datELmet17$D[datELmet17$D>.6]),rowMeans(datELmet17[,4:17], na.rm=TRUE)[datELmet17$D>.6], pch=19)
	points(log(datELmet17$D.D1[datELmet17$D.D1>.6]),rowMeans(datELmet17[,4:17], na.rm=TRUE)[datELmet17$D.D1>.6], pch=19, col="orangered3")
	points(log(datELmet17$D.D2[datELmet17$D.D2>.6]),rowMeans(datELmet17[,4:17], na.rm=TRUE)[datELmet17$D.D2>.6], pch=19, col="cornflowerblue")
	points(log(datELmet17$D.D3[datELmet17$D.D3>.6]),rowMeans(datELmet17[,4:17], na.rm=TRUE)[datELmet17$D.D3>.6], pch=19, col="darkgreen")
	dev.off()	
	
	fitLD <- lm(rowMeans(datELmet[,4:10], na.rm=TRUE)[datELmet$D>.6]~log(datELmet$D[datELmet$D>.6]))
	summary(fitLD)
	fitLD1 <- lm(rowMeans(datELmet[,4:10], na.rm=TRUE)[datELmet$D.D1>.6]~log(datELmet$D.D1[datELmet$D.D1>.6]))
	summary(fitLD1)
	fitLD2 <- lm(rowMeans(datELmet[,4:10], na.rm=TRUE)[datELmet$D.D1>.6]~log(datELmet$D.D2[datELmet$D.D1>.6]))
	summary(fitLD2)
	# same fit between 0 and .5 lag, slightly worse 1 hour
	
	fitHD <- lm(rowMeans(datEHmet[,4:10], na.rm=TRUE)[datEHmet$D>.6]~log(datEHmet$D[datEHmet$D>.6]))
	summary(fitHD)
	fitHD1 <- lm(rowMeans(datEHmet[,4:10], na.rm=TRUE)[datEHmet$D.D1>.6]~log(datEHmet$D.D1[datEHmet$D.D1>.6]))
	summary(fitHD1)
	fitHD2 <- lm(rowMeans(datEHmet[,4:10], na.rm=TRUE)[datEHmet$D.D2>.6]~log(datEHmet$D.D2[datEHmet$D.D2>.6]))
	summary(fitHD2)
	# exact same fit between all 3
	
	fitH17D <- lm(rowMeans(datEHmet17[,4:10], na.rm=TRUE)[datEHmet17$D>.6]~log(datEHmet17$D[datEHmet17$D>.6]))
	summary(fitH17D)
	fitH17D1 <- lm(rowMeans(datEHmet17[,4:10], na.rm=TRUE)[datEHmet17$D.D1>.6]~log(datEHmet17$D.D1[datEHmet17$D.D1>.6]))
	summary(fitH17D1)
	fitHD2 <- lm(rowMeans(datEHmet17[,4:10], na.rm=TRUE)[datEHmet17$D.D2>.6]~log(datEHmet17$D.D2[datEHmet17$D.D2>.6]))
	summary(fitHD2)
	#same fit all three
}



#join met data to T
datLtkg<-join(datLc1,datLmet, by=c("doy","year","hour"), type="left")
datHtkg<-join(datHc1,datHmet, by=c("doy","year","hour"), type="left")

datLtkg17<-join(datL17c1,datLmet, by=c("doy","year","hour"), type="left")
datHtkg17<-join(datH17c1,datHmet, by=c("doy","year","hour"), type="left")

#calculate saturated vapor pressure
datLe.sat<-0.611*exp((17.502*datLtkg$Temp)/(datLtkg$Temp+240.97))
datHe.sat<-0.611*exp((17.502*datHtkg$Temp)/(datHtkg$Temp+240.97))
datL17e.sat<-0.611*exp((17.502*datLtkg17$Temp)/(datLtkg17$Temp+240.97))
datH17e.sat<-0.611*exp((17.502*datHtkg17$Temp)/(datHtkg17$Temp+240.97))

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
datHtkg$Kg<-Kg.coeff(datHtkg$Temp)

datLtkg17$Kg<-Kg.coeff(datLtkg17$Temp)
datHtkg17$Kg<-Kg.coeff(datHtkg17$Temp)
#convert to gs
Gs.convert1<-function(Kg,El,D,P){((Kg*El)/D)*P}
#change units to moles
unit.conv<-function(gs,T,P){gs*.446*(273/(T+273))*(P/101.3)}


Gshigh<-matrix(rep(0,dim(datHtkg)[1]*length(NorthHkeep)), ncol=length(NorthHkeep))
Gslow<-matrix(rep(0,dim(datLtkg)[1]*length(NorthLkeep)), ncol=length(NorthLkeep))
Gshighmm<-matrix(rep(0,dim(datHtkg)[1]*length(NorthHkeep)), ncol=length(NorthHkeep))
Gslowmm<-matrix(rep(0,dim(datLtkg)[1]*length(NorthLkeep)), ncol=length(NorthLkeep))
Gshighf<-matrix(rep(0,dim(datHtkg)[1]*length(NorthHkeep)), ncol=length(NorthHkeep))
Gslowf<-matrix(rep(0,dim(datLtkg)[1]*length(NorthLkeep)), ncol=length(NorthLkeep))


Gshigh17<-matrix(rep(0,dim(datHtkg17)[1]*length(NorthH17keep)), ncol=length(NorthH17keep))
Gslow17<-matrix(rep(0,dim(datLtkg17)[1]*length(NorthL17keep)), ncol=length(NorthL17keep))
Gshighmm17<-matrix(rep(0,dim(datHtkg17)[1]*length(NorthH17keep)), ncol=length(NorthH17keep))
Gslowmm17<-matrix(rep(0,dim(datLtkg17)[1]*length(NorthL17keep)), ncol=length(NorthL17keep))
Gshighf17<-matrix(rep(0,dim(datHtkg17)[1]*length(NorthH17keep)), ncol=length(NorthH17keep))
Gslowf17<-matrix(rep(0,dim(datLtkg17)[1]*length(NorthL17keep)), ncol=length(NorthL17keep))

########################################
########################################
########FILTER POINT 3   ###############
########Range filter     ###############
########################################
for(i in 1:length(NorthHkeep)){
	Gshigh[,i]<-Gs.convert1(datHtkg$Kg,datHtkg[,(i+3)],datHtkg$D, datHtkg$Pkpa.gap)
	Gshighmm[,i]<-unit.conv(Gshigh[,i],datHtkg$Temp, datHtkg$Pkpa.gap)*1000
	Gshighf[,i]<-ifelse(Gshighmm[,i]<400,Gshighmm[,i],NA)
}	
for(i in 1:length(NorthLkeep)){	
	Gslow[,i]<-Gs.convert1(datLtkg$Kg,datLtkg[,i+3],datLtkg$D, datLtkg$Pkpa.gap)
	Gslowmm[,i]<-unit.conv(Gslow[,i],datLtkg$Temp, datLtkg$Pkpa.gap)*1000
	Gslowf[,i]<-ifelse(Gslowmm[,i]<400,Gslowmm[,i],NA)
	}
for(i in 1:length(NorthH17keep)){		
	Gshigh17[,i]<-Gs.convert1(datHtkg17$Kg,datHtkg17[,i+3],datHtkg17$D, datHtkg17$Pkpa.gap)
	Gshighmm17[,i]<-unit.conv(Gshigh17[,i],datHtkg17$Temp, datHtkg17$Pkpa.gap)*1000
	Gshighf17[,i]<-ifelse(Gshighmm17[,i]<400,Gshighmm17[,i],NA)
}	
for(i in 1:length(NorthL17keep)){		
	Gslow17[,i]<-Gs.convert1(datLtkg17$Kg,datLtkg17[,i+3],datLtkg17$D, datLtkg17$Pkpa.gap)
	Gslowmm17[,i]<-unit.conv(Gslow17[,i],datLtkg17$Temp, datLtkg17$Pkpa.gap)*1000
	Gslowf17[,i]<-ifelse(Gslowmm17[,i]<400,Gslowmm17[,i],NA)
}

#create a dataframe
gc.H<-data.frame(doy=datHtkg$doy, year=rep(2016,length(datHtkg$doy)),hour=datHtkg$hour,Gshighf[,1:length(NorthHkeep)])
#exclude sensor 1 because it is very high and can't be verified 
gc.L<-data.frame(doy=datLtkg$doy, year=rep(2016,length(datLtkg$doy)),hour=datLtkg$hour,Gslowf[,1:length(NorthLkeep)])

gc.H17<-data.frame(doy=datHtkg17$doy, year=rep(2017,length(datHtkg17$doy)),hour=datHtkg17$hour,Gshighf17[,1:length(NorthH17keep)])
#exclude sensor 1 because it is very high and can't be verified 
gc.L17<-data.frame(doy=datLtkg17$doy, year=rep(2017,length(datLtkg17$doy)),hour=datLtkg17$hour,Gslowf17[,1:length(NorthL17keep)])




if(plotcheck==1){
	for(i in 1:length(NorthHkeep)){
		jpeg(file=paste0(diagP, "\\gc\\high2016sensor",NorthHkeep[i],".jpg"), width=4000, height=1000, units="px")
			plot(gc.H$doy+(gc.H$hour/24),gc.H[,(i+3)], type="b", pch=19, xlab= "Doy", ylab="gc (mmol m-2 s-1)",
			main=paste("sensor",NorthHkeep[i]))
		dev.off()
	}
	for(i in 1:length(NorthH17keep)){
		jpeg(file=paste0(diagP, "\\gc\\high2017sensor",NorthH17keep[i],".jpg"), width=4000, height=1000, units="px")
			plot(gc.H17$doy+(gc.H17$hour/24),gc.H17[,(i+3)], type="b", pch=19, xlab= "Doy", ylab="gc (mmol m-2 s-1)",
			main=paste("sensor",NorthH17keep[i]))
		dev.off()
	}
	for(i in 1:length(NorthLkeep)){
		jpeg(file=paste0(diagP, "\\gc\\low2016sensor",NorthLkeep[i],".jpg"), width=4000, height=1000, units="px")
			plot(gc.L$doy+(gc.L$hour/24),gc.L[,(i+3)], type="b", pch=19, xlab= "Doy", ylab="gc (mmol m-2 s-1)",
			main=paste("sensor",NorthLkeep[i]))
		dev.off()
	}
	for(i in 1:length(NorthL17keep)){
		jpeg(file=paste0(diagP, "\\gc\\low2017sensor",NorthL17keep[i],".jpg"), width=4000, height=1000, units="px")
			plot(gc.L17$doy+(gc.L17$hour/24),gc.L17[,(i+3)], type="b", pch=19, xlab= "Doy", ylab="gc (mmol m-2 s-1)",
			main=paste("sensor",NorthLkeep[i]))
		dev.off()
	}
}

#########End canopy stomatal conductance(gc) calcs ####################

#################################################################
###### clear workspace so workspace is filled with too     ######
###### many variables when the script is called            ######
#################################################################



#clear all variables except for T and gc outptut
rm(list=setdiff(ls(), c("El.L", "El.H", "El.H17","El.L17", "gc.H","gc.H17","gc.L", "gc.L17", 
							"datTreeH", "datTreeL", "datTreeH17", "datTreeL17")))