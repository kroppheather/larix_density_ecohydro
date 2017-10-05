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
datL$daytimeInd <-ifelse(datDTL$hour<5|datDTL$hour>=9,1,2)
datH$daytimeInd <-ifelse(datDTH$hour<5|datDTH$hour>=9,1,2)
datL17$daytimeInd <-ifelse(datDTL17$hour<5|datDTL17$hour>=9,1,2)
datH17$daytimeInd <-ifelse(datDTH17$hour<5|datDTH17$hour>=9,1,2)
#add time
datH$hour <- datDTH[,3]
datL$hour <- datDTL[,3]
datH17$hour <- datDTH17[,3]
datL17$hour <- datDTL17[,3]

LmaxTemp <- list()
LmaxDTA <- list()
LlengthDTA <- list()
for(i in 1:16){
	LmaxTemp[[i]]<- na.omit(data.frame(dT=datL[,i],doy5=datL$doy5,daytimeInd=datL$daytimeInd))
	LmaxDTA[[i]] <- aggregate(LmaxTemp[[i]]$dT, by=list(LmaxTemp[[i]]$doy5,LmaxTemp[[i]]$daytimeInd), FUN="max")
	colnames(LmaxDTA[[i]]) <- c("doy5", "daytimeInd", "dT")
	LlengthDTA[[i]] <- aggregate(LmaxTemp[[i]]$dT, by=list(LmaxTemp[[i]]$doy5,LmaxTemp[[i]]$daytimeInd), FUN="length")
colnames(LmaxDTA[[i]]) <- c("doy5", "daytimeInd", "n")
	}
#diagnostic plots to see what 
if(plotcheck==1){
	for(i in 1:16){
	jpeg(file=paste0(diagP, "\\maxT\\sensor",i,".jpg"), width=1500, height=1000, units="px")
		plot(LmaxDTA[[i]]$doy5[LmaxDTA[[i]]$daytimeInd==1],LmaxDTA[[i]]$dT[LmaxDTA[[i]]$daytimeInd==1],
				xlim=c(min(LmaxDTA[[i]]$doy5)-.5,max(LmaxDTA[[i]]$doy5)+.5),
				ylim=c(0,15),
				xlab="Day of year", ylab="dT (C)", main=paste("sensor",i),
				pch=19, cex=1.5)
		text(LmaxDTA[[i]]$doy5[LmaxDTA[[i]]$daytimeInd==1]-.5,
			LmaxDTA[[i]]$dT[LmaxDTA[[i]]$daytimeInd==1]-.5,
			paste(LlengthDTA[[i]]$n[LlengthDTA[[i]]$daytimeInd==1]) )		
		points(LmaxDTA[[i]]$doy5[LmaxDTA[[i]]$daytimeInd==2],LmaxDTA[[i]]$dT[LmaxDTA[[i]]$daytimeInd==2],
				pch=19, col="tomato3", cex=1.5)
		
		text(LmaxDTA[[i]]$doy5[LmaxDTA[[i]]$daytimeInd==2]-.5,
			LmaxDTA[[i]]$dT[LmaxDTA[[i]]$daytimeInd==2]-.5,
			paste(LlengthDTA[[i]]$n[LlengthDTA[[i]]$daytimeInd==2]) )	
	dev.off()
	}
}

