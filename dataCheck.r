###############################################################################
###########This code is for checking if there was a data ######################
###########logger issue at the high density TDP ###############################
###############################################################################
###############################################################################

#load libraries
library(lubridate)
library(plyr)

#set wd
setwd("c:\\Users\\hkropp\\Google Drive\\Viper_SF")

datH<-read.table("high_density_TDP.csv", sep=",", head=TRUE, na.strings=c("NAN"))


#convert time by campbell to more continous output
dateH<-as.Date(datH$TIMESTAMP, "%m/%d/%Y %H:%M")

datH$doy<-yday(dateH)
#need to create new doy index that defines day of year between 5am and 5am

datH$doy5<-ifelse(datH$Hour<=5,datH$doy-1,datH$doy)


#make a plot of halfhourly dt by day of year

datH$dateI<-datH$doy+(datH$Hour/24)

for(i in 1:16){
	jpeg(file=paste0("c:\\Users\\hkropp\\Google Drive\\Viper_SF\\check\\dT\\TDsensor",i,".jpg"), width=1500, height=1000, units="px")
	par(mai=c(2,2,2,2))
	plot(datH$dateI, datH[,i+1], type="l", xlab=" ", ylab=" ", axes=FALSE)
	axis(1, seq(180, 230, by=5), cex.axis=2)
	axis(2, seq(floor(min(na.omit(datH[,i+1]))),floor(max(na.omit(datH[,i+1]))+1), length.out=5), cex.axis=2)
	mtext("Day of Year", side=1, line=5, cex=2)
	mtext("dT (C)", side=2, line=6, cex=2)
	dev.off()

}


datR<-read.csv("netR.csv")
#convert time by campbell to more continous output
dateR<-as.Date(datR$TIMESTAMP, "%m/%d/%Y %H:%M")

datR$doy<-yday(dateR)

#isolate out four variables to plot
datNR<-data.frame(doy=datR$doy, hour=datR$X, dateI=datR$doy+(datR$X/24), SRU=datR$SR01Up_Avg,SRD=datR$SR01Dn_Avg, LRU=datR$IR01UpCo_Avg, 
					LRD=datR$IR01DnCo_Avg)

labelsT<-c("Shortwave Upward (W/m2)","Shortwave Downward (W/m2)", "Longwave Upward (W/m2)", "Longwave Downward (W/m2)")

for(i in 1:4){
	jpeg(file=paste0("c:\\Users\\hkropp\\Google Drive\\Viper_SF\\check\\dT\\sensor",colnames(datNR)[i+3],".jpg"), width=1500, height=1000, units="px")
	par(mai=c(2,2,2,2))
	plot(datNR$dateI, datNR[,i+3], type="l", xlab=" ", ylab=" ", axes=FALSE)
	axis(1, seq(180, 230, by=5), cex.axis=2)
	axis(2, seq(floor(min(na.omit(datNR[,i+3]))),floor(max(na.omit(datNR[,i+3]))+1), length.out=5), cex.axis=2)
	mtext("Day of Year", side=1, line=5, cex=2)
	mtext(paste(labelsT[i]), side=2, line=6, cex=2)
	dev.off()

}
