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
xl2016 <- 185
xh2016 <-245
xl2017 <- 165
xh2017 <- 195
subAirP <- datAirP[datAirP$doy>=xl2017&datAirP$year==2017&datAirP$doy<=xh2017 ,]
subAirP2 <- datAirP[datAirP$doy>=xl2016&datAirP$year==2016&datAirP$doy<=xh2016 ,]
wd <- 30
hd <- 25

PRmax <- 50
Prscale <- 1/PRmax

colS <- "royalblue"
colT <- "darkgreen"
colHt <- rgb(205/255,79/255,57/255, .5)
colLt <- rgb(65/255,105/255,225/255,.5)

colM <-"palegreen4"
colOM <- "tomato2"
colO <- "saddlebrown"
axisC <-3

jpeg(paste0(pdir, "summ_profile_all.jpg"), , width=2200, height=2200, units="px")
ab <- layout(matrix(seq(1,4), ncol=2, byrow=FALSE), width=rep(lcm(wd),4), height=rep(lcm(hd),4))

par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), xlim=c(xl2016,xh2016), ylim=c(0,1), xlab=" ", ylab ="  ", xaxs="i", yaxs="i", 
		type="n", axes=FALSE)
	
	for(i in 1:dim(subAirP2)[1]){
		polygon(c(subAirP2$doy[i]-1,subAirP2$doy[i]-1,subAirP2$doy[i],subAirP2$doy[i]), 
			c(0,subAirP2$Pr.mm[i]*Prscale,subAirP2$Pr.mm[i]*Prscale,0), col="grey60", border=FALSE)

}	
	
	
	points(datSW$doy[datSW$site=="ld"&datSW$sensorLoc=="tree"&datSW$sensorZ==5&datSW$year==2016&datSW$doy>195]+
		(datSW$hour[datSW$site=="ld"&datSW$sensorLoc=="tree"&datSW$sensorZ==5&datSW$year==2016&datSW$doy>195]/24),
		datSW$vwc.GS3[datSW$site=="ld"&datSW$sensorLoc=="tree"&datSW$sensorZ==5&datSW$year==2016&datSW$doy>195],
		type="l",  col=colT, lwd=6)
	points(datSW$doy[datSW$site=="ld"&datSW$sensorLoc=="shrub"&datSW$sensorZ==5&datSW$year==2016&datSW$doy>195]+
		(datSW$hour[datSW$site=="ld"&datSW$sensorLoc=="shrub"&datSW$sensorZ==5&datSW$year==2016&datSW$doy>195]/24),
		datSW$vwc.GS3[datSW$site=="ld"&datSW$sensorLoc=="shrub"&datSW$sensorZ==5&datSW$year==2016&datSW$doy>195],
		type="l", lty=1, col=colS, lwd=6)	
	legend(xl2016+5, 1, c("5 cm organic under tree","5 cm organic under shrub","Precipitation"),
		col=c(colT,colS,"grey60"), lwd=c(6,6,NA), pch=c(NA,NA,15),
			lty=c(1,1,NA), bty="n", cex=4)
			
	axis(2, seq(0,1, by=.2), las=2,  cex.axis=axisC, lwd.ticks=3)
	mtext("Volumetric soil water (m3/m3)", side=2, outer=TRUE, line=-9, cex=4)	
	mtext("Precipitation (mm)", side=4, outer=TRUE, line=-9, cex=4)	
box(which="plot")


par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), xlim=c(xl2016,xh2016), ylim=c(0,1), xlab=" ", ylab ="  ", xaxs="i", yaxs="i", 
		type="n", axes=FALSE)
	#points(datSW$doy[datSW$site=="hd"&datSW$sensorLoc=="moss"&datSW$sensorZ==5&datSW$year==2016]+
	#	(datSW$hour[datSW$site=="hd"&datSW$sensorLoc=="moss"&datSW$sensorZ==5&datSW$year==2016]/24),
	#	datSW$vwc.GS3[datSW$site=="hd"&datSW$sensorLoc=="moss"&datSW$sensorZ==5&datSW$year==2016],
	#	type="l",  col=colT, lwd=6)
		for(i in 1:dim(subAirP2)[1]){
		polygon(c(subAirP2$doy[i]-1,subAirP2$doy[i]-1,subAirP2$doy[i],subAirP2$doy[i]), 
			c(0,subAirP2$Pr.mm[i]*Prscale,subAirP2$Pr.mm[i]*Prscale,0), col="grey60", border=FALSE)

}		
	points(datSW$doy[datSW$site=="hd"&datSW$sensorLoc=="organic"&datSW$sensorZ==5&datSW$year==2016]+
		(datSW$hour[datSW$site=="hd"&datSW$sensorLoc=="organic"&datSW$sensorZ==5&datSW$year==2016]/24),
		datSW$vwc.GS3[datSW$site=="hd"&datSW$sensorLoc=="organic"&datSW$sensorZ==5&datSW$year==2016],
		type="l", lty=1, col=colT, lwd=6)			
	axis(1,seq(xl2016,xh2016-5,by=5) ,rep(" ", length(seq(xl2016,xh2016-5,by=5))),   cex.axis=axisC, lwd.ticks=3)	
	mtext(seq(xl2016,xh2016-5, by=5),at=seq(xl2016,xh2016-5, by=5), line=3, side=1, cex=2.5)
	axis(2, seq(0,.8, by=.2), las=2,  cex.axis=axisC, lwd.ticks=3)	
legend(xl2016+5, 1, c("5 cm organic under tree", "Precipitation"), col=c(colT, "grey60"), lwd=c(6,NA),
			lty=c(1,NA), pch=c(NA,15), bty="n", cex=4)	
	
box(which="plot")

par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), xlim=c(xl2017,xh2017), ylim=c(0,1), xlab=" ", ylab ="  ", xaxs="i", yaxs="i", 
		type="n", axes=FALSE)
for(i in 1:dim(subAirP)[1]){
	polygon(c(subAirP$doy[i]-1,subAirP$doy[i]-1,subAirP$doy[i],subAirP$doy[i]), 
			c(0,subAirP$Pr.mm[i]*Prscale,subAirP$Pr.mm[i]*Prscale,0), col="grey60", border=FALSE)

}		
		
points(datDayO$doy[datDayO$stand=="ld"&datDayO$mossI==1],datDayO$vwc[datDayO$stand=="ld"&datDayO$mossI==1],
			col=colOM,pch=19, type="b", cex=4, lwd=3)	
points(datDayO$doy[datDayO$stand=="ld"&datDayO$mossI==2],datDayO$vwc[datDayO$stand=="ld"&datDayO$mossI==2],
	col="saddlebrown",pch=19, type="b", cex=4, lwd=3)
points(datDayM$doy[datDayM$stand=="ld"],datDayM$vwc[datDayM$stand=="ld"],
	col=colM,pch=19, type="b", cex=4, lwd=3)	
axis(4, seq(0,1, by=.2),seq(0,1, by=.2)*(1/Prscale) , las=2,  cex.axis=axisC, lwd.ticks=3)
legend(xl2017+12, 1, c("organic below moss","moss","organic", "Precipitation"), col=c(colOM,colM,  colO, "grey60"), 
			lwd=c(6,6,6,NA),
			lty=c(1,1,1,NA), pch=c(NA,NA,NA,15), bty="n", cex=4)		
box(which="plot")


par(mai=c(0,0,0,0))	
plot(c(0,1),c(0,1), xlim=c(xl2017,xh2017), ylim=c(0,1), xlab=" ", ylab ="  ", xaxs="i", yaxs="i", 
		type="n", axes=FALSE)
for(i in 1:dim(subAirP)[1]){
	polygon(c(subAirP$doy[i]-1,subAirP$doy[i]-1,subAirP$doy[i],subAirP$doy[i]), 
			c(0,subAirP$Pr.mm[i]*Prscale,subAirP$Pr.mm[i]*Prscale,0), col="grey60", border=FALSE)

}		
	
points(datDayO$doy[datDayO$stand=="hd"],datDayO$vwc[datDayO$stand=="hd"],
			col="saddlebrown",pch=19, type="b", cex=4, lwd=3)	
points(datDayM$doy[datDayM$stand=="hd"],datDayM$vwc[datDayM$stand=="hd"],
	col="palegreen4",pch=19, type="b", cex=4, lwd=3)
	axis(1,seq(xl2017,xh2017-5,by=5) ,rep(" ", length(seq(xl2017,xh2017-5,by=5))),   cex.axis=axisC, lwd.ticks=3)	
	mtext(seq(xl2017,xh2017-5, by=5),at=seq(xl2017,xh2017-5, by=5), line=3, side=1, cex=2.5)
legend(xl2017+2, 1, c("organic below moss","moss", "Precipitation"), col=c( colOM,colM, "grey60"), 
			lwd=c(6,6,NA),
			lty=c(1,1,NA), pch=c(NA,NA,15), bty="n", cex=4)			
box(which="plot")	
axis(4, seq(0,.8, by=.2),seq(0,.8, by=.2)*(1/Prscale) , las=2,  cex.axis=axisC, lwd.ticks=3)
mtext("Day of year", side=1, outer=TRUE, line=-25, cex=4)
dev.off()