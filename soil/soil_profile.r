###########################################################################
###########################################################################
############## Created by Heather Kropp in October 2017      ##############
############## This script is to be run for all analyses     ##############
############## associated with patterns in vwc between sites ##############
###########################################################################
###########################################################################
library(plyr)

#directory to plot
pdir <- "c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\soil_plot\\"

#################################################################
####read in datafiles                                     #######
#################################################################

#read in vwc profiles
datPR <- read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\individual_data\\vwc_profile.csv")
#read in precip data
datAirP <- read.csv("c:\\Users\\hkropp\\Google Drive\\viperSensor\\airport\\airport.csv")

#read in continuous soil data
datSW <- read.csv("c:\\Users\\hkropp\\Google Drive\\viperSensor\\soil\\vwc.GS3.csv")

#vwc profile use the mineral setting since it is the setting
#on all the sensors in the field
#only use mineral now
#subset into a more workable dataframe

datP <- data.frame(doy=datPR$doy, year=datPR$year, stand=datPR$stand,
					mossD=datPR$depth.moss, vwc.moss=datPR$moss.vwc.mineral,
					 vwc.org=datPR$organic.vwc.mineral,
					DsensorO=datPR$sensor.depth.organic, OsensorO=datPR$orientation.organic,
					DsensorM=datPR$sensor.depth.moss, OsensorM=datPR$orientation.moss)
datP <- datP[is.na(datP$vwc.org)==FALSE,]

#sensor prongs are 5.5
datP$moss.mid <- ifelse(datP$OsensorM=="side",datP$DsensorM, datP$DsensorM+(5.5/2))
datP$org.mid <- ifelse(datP$OsensorO=="side",datP$DsensorO, datP$DsensorO+(5.5/2))

#calculate the depth of the midpoint from the top of the moss for organic
datP$org.relD <- datP$org.mid- datP$mossD

subAirP <- datAirP[datAirP$doy>=165&datAirP$year==2017&datAirP$doy<=195,]

#get the total precip in the past days before sampling
daysG <- unique(datP$doy)
precipG <- matrix(rep(NA, length(daysG)*3), ncol=3)
for(i in 1:length(daysG)){
	for(j in 1:3){
	precipG[i,j] <- subAirP$Pr.mm[subAirP$doy==(daysG[i]-j)]
	}
}
precipDF <- data.frame(doy=daysG, pr.mm=rowSums(precipG))

#join to datP
datP <-join(datP, precipDF, by="doy", type="left")

#only focus on 5cm soil moisture data
datSW <- datSW[datSW$sensorZ==5,]
#omit  NA
datSW <- na.omit(datSW)
#aggregate continuous soil data

#daySW <- aggregate(datSW$vwc.GS3, by=list(datSW$doy,datSW$year,datSW$site,datSW$sensorLoc), FUN="mean")
#colnames(daySW) <- c("doy", "year", "site", "loc", "swc")
daySWsub <-datSW[datSW$doy>=165&datSW$year==2017&datSW$doy<=195,]
#################################################################
####plot all profile data                                 #######
#################################################################

#start by making a simple plot that doesn't deal with depth but rather just 
#soil range is 176-193
#plot 165-193
jpeg(paste0(pdir, "all_profile.jpg"), , width=1000, height=1000, units="px")
par(mfrow=c(2,1))
plot(c(0,1),c(0,1), xlim=c(165,195), ylim=c(0,1), xlab="Day of Year", ylab ="swc (cm3/cm3) and Precip(cm) ", xaxs="i", yaxs="i", 
		type="n")
for(i in 1:dim(subAirP)[1]){
	polygon(c(subAirP$doy[i]-1,subAirP$doy[i]-1,subAirP$doy[i],subAirP$doy[i]), 
			c(0,subAirP$Pr.mm[i]/10,subAirP$Pr.mm[i]/10,0), col="grey60", border=FALSE)

}		
		
points(datP$doy[datP$stand=="ld"],datP$vwc.org[datP$stand=="ld"],col="tomato3",pch=19)		
points(datP$doy[datP$stand=="ld"],datP$vwc.moss[datP$stand=="ld"],col="palegreen4",pch=19)	

points(daySWsub$doy[daySWsub$site=="ld"&daySWsub$sensorLoc=="tree"]+
		(daySWsub$hour[daySWsub$site=="ld"&daySWsub$sensorLoc=="tree"]/24),
		daySWsub$vwc.GS3[daySWsub$site=="ld"&daySWsub$sensorLoc=="tree"],
		type="l", lwd=2, col="cornflowerblue")

		points(daySWsub$doy[daySWsub$site=="ld"&daySWsub$sensorLoc=="shrub"]+
		(daySWsub$hour[daySWsub$site=="ld"&daySWsub$sensorLoc=="shrub"]/24),
		daySWsub$vwc.GS3[daySWsub$site=="ld"&daySWsub$sensorLoc=="shrub"],
		type="l", lwd=2, col="plum3")
		
plot(c(0,1),c(0,1), xlim=c(165,195), ylim=c(0,1), xlab="Day of Year", ylab =" ", xaxs="i", yaxs="i", 
		type="n")
for(i in 1:dim(subAirP)[1]){
	polygon(c(subAirP$doy[i]-1,subAirP$doy[i]-1,subAirP$doy[i],subAirP$doy[i]), 
			c(0,subAirP$Pr.mm[i]/10,subAirP$Pr.mm[i]/10,0), col="grey60", border=FALSE)

}		
		
points(datP$doy[datP$stand=="hd"],datP$vwc.org[datP$stand=="hd"],col="tomato3",pch=19)		
points(datP$doy[datP$stand=="hd"],datP$vwc.moss[datP$stand=="hd"],col="palegreen4",pch=19)	



points(daySWsub$doy[daySWsub$site=="hd"&daySWsub$sensorLoc=="moss"]+
		(daySWsub$hour[daySWsub$site=="hd"&daySWsub$sensorLoc=="moss"]/24),
		daySWsub$vwc.GS3[daySWsub$site=="hd"&daySWsub$sensorLoc=="moss"],
		type="l", lwd=2, col="cornflowerblue")

		points(daySWsub$doy[daySWsub$site=="hd"&daySWsub$sensorLoc=="organic"]+
		(daySWsub$hour[daySWsub$site=="hd"&daySWsub$sensorLoc=="organic"]/24),
		daySWsub$vwc.GS3[daySWsub$site=="hd"&daySWsub$sensorLoc=="organic"],
		type="l", lwd=2, col="plum3")		
dev.off()		


#################################################################
####aggregate profile data                                #######
#################################################################
#make a column that indicates no moss layer
datP$mossI <- ifelse(datP$mossD==0,1,2)
datM <- na.omit(data.frame(doy=datP$doy, stand=datP$stand,vwc.moss=datP$vwc.moss))
datO <- na.omit(data.frame(doy=datP$doy, stand=datP$stand,vwc.org=datP$vwc.org, mossI=datP$mossI))
datDayM <- aggregate(datM$vwc.moss, by=list(datM$doy, datM$stand), FUN="mean")
colnames(datDayM) <- c("doy", "stand", "vwc")
datDayO<- aggregate(datO$vwc.org, by=list(datO$doy, datO$mossI, datO$stand), FUN="mean")
colnames(datDayO) <- c("doy","mossI", "stand", "vwc")
#################################################################
####plot aggregate profile data                           #######
#################################################################

jpeg(paste0(pdir, "summ_profile.jpg"), , width=1000, height=1000, units="px")
par(mfrow=c(2,1))
plot(c(0,1),c(0,1), xlim=c(165,195), ylim=c(0,1), xlab="Day of Year", ylab ="swc (cm3/cm3) and Precip(cm) ", xaxs="i", yaxs="i", 
		type="n")
for(i in 1:dim(subAirP)[1]){
	polygon(c(subAirP$doy[i]-1,subAirP$doy[i]-1,subAirP$doy[i],subAirP$doy[i]), 
			c(0,subAirP$Pr.mm[i]/10,subAirP$Pr.mm[i]/10,0), col="grey60", border=FALSE)

}		
		
points(datDayO$doy[datDayO$stand=="ld"&datDayO$mossI==1],datDayO$vwc[datDayO$stand=="ld"&datDayO$mossI==1],
			col="tomato3",pch=19, type="b")	
points(datDayO$doy[datDayO$stand=="ld"&datDayO$mossI==2],datDayO$vwc[datDayO$stand=="ld"&datDayO$mossI==2],
	col="saddlebrown",pch=19, type="b")
points(datDayM$doy[datDayM$stand=="ld"],datDayM$vwc[datDayM$stand=="ld"],
	col="palegreen4",pch=19, type="b")	
	
plot(c(0,1),c(0,1), xlim=c(165,195), ylim=c(0,1), xlab="Day of Year", ylab ="swc (cm3/cm3) and Precip(cm) ", xaxs="i", yaxs="i", 
		type="n")
for(i in 1:dim(subAirP)[1]){
	polygon(c(subAirP$doy[i]-1,subAirP$doy[i]-1,subAirP$doy[i],subAirP$doy[i]), 
			c(0,subAirP$Pr.mm[i]/10,subAirP$Pr.mm[i]/10,0), col="grey60", border=FALSE)

}		
	
points(datDayO$doy[datDayO$stand=="hd"],datDayO$vwc[datDayO$stand=="hd"],
			col="saddlebrown",pch=19, type="b")	
points(datDayM$doy[datDayM$stand=="hd"],datDayM$vwc[datDayM$stand=="hd"],
	col="palegreen4",pch=19, type="b")	
dev.off()