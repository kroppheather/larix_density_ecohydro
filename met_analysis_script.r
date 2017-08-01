#######################################################
###organizes met data and plots it ####################
#######################################################
library(lubridate)
#set working directory
setwd("c:\\Users\\hkropp\\Google Drive\\Viper_SF")

#read in data
#airport met data
datAD<-read.table("airport.csv", sep=";", head=TRUE, skip=6, stringsAsFactors=FALSE)

#get date from each
dateAP<-as.Date(rownames(datAD), "%d.%m.%Y %H:%M")


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

PrecipDay <- aggregate(PrecipDat$Precip, 
				by=list(PrecipDat$doy,PrecipDat$year), FUN="sum")
colnames(PrecipDay)<-c("doy","year","Pr.mm")
PrecipDay$Pr.cm<-PrecipDay$Pr.mm/10
							
#thaw depth
datT<-read.csv("thawdepth.csv")

#convert date
dateT<-as.Date(datT$Date,"%m/%d/%Y")
datT$doyT<-yday(dateT)
datT$yearT<-year(dateT)

#soil moisture
datLS<-read.csv("LDF2SM.csv.csv")
datHS<-read.csv("DAV_SM.csv.csv")

datHS$times<-datHS$doy+datHS$hour/24
datLS$times<-datLS$doy+datLS$hour/24

#aggregate soil moisture to day average
HV1<-aggregate(datHS$VWC1,by=list(datHS$doy,datHS$year),FUN="mean")
HV2<-aggregate(datHS$VWC2,by=list(datHS$doy,datHS$year),FUN="mean")
HV3<-aggregate(datHS$VWC3,by=list(datHS$doy,datHS$year),FUN="mean")
colnames(HV1)<-c("doy","year","VWC")
colnames(HV2)<-c("doy","year","VWC")
colnames(HV3)<-c("doy","year","VWC")


LV1<-aggregate(datLS$Moist1,by=list(datLS$doy,datLS$year),FUN="mean")
LV2<-aggregate(datLS$Moist2,by=list(datLS$doy,datLS$year),FUN="mean")
LV3<-aggregate(datLS$Moist3,by=list(datLS$doy,datLS$year),FUN="mean")
colnames(LV1)<-c("doy","year","VWC")
colnames(LV2)<-c("doy","year","VWC")
colnames(LV3)<-c("doy","year","VWC")

#subset to get rid of bogus data days
LV1<-LV1[LV1$doy>=195|LV1$year==2017,]
LV2<-LV2[LV2$doy>=195|LV2$year==2017,]
LV3<-LV3[LV3$doy>=210|LV3$year==2017,]
#get unique dayid for each year
datHS$dayS<-ifelse(datHS$year==2016,datHS$doy,datHS$doy+366)
datLS$dayS<-ifelse(datLS$year==2016,datLS$doy,datLS$doy+366)

#aggregate the thaw depth across site
datTsub<-data.frame(TD=datT$TD,doy=datT$doyT,year=datT$yearT, site=datT$Site)
datTsub<-na.omit(datTsub)
TDave<-aggregate(datTsub$TD,by=list(datTsub$site,datTsub$doy,datTsub$year),FUN="mean")
colnames(TDave)<-c("site","doy","year","TD")
TDsd<-aggregate(datTsub$TD,by=list(datTsub$site,datTsub$doy,datTsub$year),FUN="sd")
colnames(TDsd)<-c("site","doy","year","TD.sd")
TDL<-aggregate(datTsub$TD,by=list(datTsub$site,datTsub$doy,datTsub$year),FUN="length")
colnames(TDL)<-c("site","doy","year","TD.L")
TDsd$se<-TDsd$TD.sd/sqrt(TDL$TD.L)

#plot bounds
wd<-15
hd<-9
#doy195 is when actually started
xl16<-183
xh16<-245
xl17<-160
xh17<-199
ylv<-0
yuv<-.7
ylT<-100
yuT<-0
#number of days
dc16<-xh16-xl16
dc17<-xh17-xl17
dtT<-dc16+dc17
#get 18 total cm
p16<-dc16/dtT
pp16<-round(p16*30)
pp17<-30-pp16

#subset precip
PrecipDay16<-PrecipDay[PrecipDay$doy>=xl16&PrecipDay$doy<=xh16&PrecipDay$year==2016,]
PrecipDay17<-PrecipDay[PrecipDay$doy>=xl17&PrecipDay$doy<=xh17&PrecipDay$year==2017,]
#make a plot of each moisture sensor
ac<-layout(matrix(seq(1,4), ncol=2,byrow=TRUE),
		width=c(lcm(pp16),lcm(pp17),lcm(pp16),lcm(pp17)),height=rep(lcm(hd),4))

layout.show(ac)

par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1),xlim=c(xl16,xh16), type="n", xlab=" ", ylim=c(ylv,yuv),
			ylab=" ", xaxs="i",
		yaxs="i", axes=FALSE)
for(i in 1:dim(PrecipDay16)[1]){
	polygon(c(PrecipDay16$doy[i]-.5,PrecipDay16$doy[i]-.5,
				PrecipDay16$doy[i]+.5,PrecipDay16$doy[i]+.5),
				c(0,PrecipDay16$Pr.cm[i]/10,PrecipDay16$Pr.cm[i]/10,0),
				col="grey80",border="NA")

}
points(datLS$times[datLS$doy>195&datLS$year==2016],datLS$Moist1[datLS$doy>195&datLS$year==2016], type="l",col="royalblue4",lty=2,lwd=3)
points(datLS$times[datLS$doy>195&datLS$year==2016],datLS$Moist2[datLS$doy>195&datLS$year==2016], type="l",col="royalblue1",lwd=3)


points(datHS$times[datHS$doy>195&datHS$year==2016],datHS$VWC1[datHS$doy>195&datHS$year==2016], type="l",col="palegreen2",lwd=3)
points(datHS$times[datHS$doy>195&datHS$year==2016],datHS$VWC2[datHS$doy>195&datHS$year==2016], type="l",col="palegreen4",lty=2,lwd=3)
mtext(expression(paste("Soil Moisture (m"^"3"~" m"^"-3"~")")),side=2,line=3,cex=1.75)
mtext("2016",side=3,line=1,cex=2)
box(which="plot")
axis(2,seq(0,.6,by=.1),las=2,cex.axis=1.5)
legend(183,.7,c("high 5cm","high ?cm","low 5cm","low ?cm","Precip"),
				pch=c(NA,NA,NA,NA,15),col=c("palegreen2","palegreen4","royalblue1","royalblue4","grey80"),
				lty=c(1,2,1,2,NA),lwd=c(3,3,3,3,NA), bty="n",cex=1.5)

par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1),xlim=c(xl17,xh17), type="n", xlab=" ", ylim=c(ylv,yuv),
			ylab=" ", xaxs="i",
		yaxs="i", axes=FALSE)
		
for(i in 1:dim(PrecipDay17)[1]){
	polygon(c(PrecipDay17$doy[i]-.5,PrecipDay17$doy[i]-.5,
				PrecipDay17$doy[i]+.5,PrecipDay17$doy[i]+.5),
				c(0,PrecipDay17$Pr.cm[i]/10,PrecipDay17$Pr.cm[i]/10,0),
				col="grey80",border="NA")

}
points(datLS$times[datLS$year==2017],datLS$Moist1[datLS$year==2017], type="l",col="royalblue4",lty=2,lwd=3)
points(datLS$times[datLS$year==2017],datLS$Moist2[datLS$year==2017], type="l",col="royalblue1",lwd=3)

points(datHS$times[datHS$year==2017],datHS$VWC1[datHS$year==2017], type="l",col="palegreen2",lwd=3)
points(datHS$times[datHS$year==2017],datHS$VWC2[datHS$year==2017], type="l",col="palegreen4",lty=2,lwd=3)
mtext("2017",side=3,line=1,cex=2)
mtext("Precipitation (cm)",side=4,line=3,cex=1.75)
axis(4,seq(0,.6,by=.1),seq(0,6,by=1),las=2,cex.axis=1.5)
box(which="plot")
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1),xlim=c(xl16,xh16), type="n", xlab=" ", ylim=c(ylT,yuT),
			ylab=" ", xaxs="i",
		yaxs="i", axes=FALSE)

arrows(TDsd$doy[TDave$site=="ld"&TDave$year==2016],
		TDave$TD[TDave$site=="ld"&TDave$year==2016]-TDsd$se[TDsd$site=="ld"&TDsd$year==2016],
		TDsd$doy[TDave$site=="ld"&TDave$year==2016],
		TDave$TD[TDave$site=="ld"&TDave$year==2016]+TDsd$se[TDsd$site=="ld"&TDsd$year==2016],
		code=0,lwd=2)		
arrows(TDsd$doy[TDave$site=="hd"&TDave$year==2016],
		TDave$TD[TDave$site=="hd"&TDave$year==2016]-TDsd$se[TDsd$site=="hd"&TDsd$year==2016],
		TDsd$doy[TDave$site=="hd"&TDave$year==2016],
		TDave$TD[TDave$site=="hd"&TDave$year==2016]+TDsd$se[TDsd$site=="hd"&TDsd$year==2016],
		code=0,lwd=2)		
points(TDave$doy[TDave$site=="ld"&TDave$year==2016],TDave$TD[TDave$site=="ld"&TDave$year==2016],
		type="b",pch=19,lwd=2,col="royalblue1",cex=1.5)		
points(TDave$doy[TDave$site=="hd"&TDave$year==2016],TDave$TD[TDave$site=="hd"&TDave$year==2016],
		type="b",pch=19,lwd=2,col="palegreen4",cex=1.5)	

legend(185,60,c("high","low"),pch=19, col=c("palegreen4","royalblue1"),cex=1.5,
		bty="n")
		
box(which="plot")		
axis(2,seq(100,10,by=-10),las=2,cex.axis=1.5)
axis(1, seq(185,245,by=5),cex.axis=1.5)
mtext("Thaw depth (cm)",side=2,line=3,cex=1.75)
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1),xlim=c(xl17,xh17), type="n", xlab=" ", ylim=c(ylT,yuT),
			ylab=" ", xaxs="i",
		yaxs="i", axes=FALSE)
arrows(TDsd$doy[TDave$site=="ld"&TDave$year==2017],
		TDave$TD[TDave$site=="ld"&TDave$year==2017]-TDsd$se[TDsd$site=="ld"&TDsd$year==2017],
		TDsd$doy[TDave$site=="ld"&TDave$year==2017],
		TDave$TD[TDave$site=="ld"&TDave$year==2017]+TDsd$se[TDsd$site=="ld"&TDsd$year==2017],
		code=0,lwd=2)		
arrows(TDsd$doy[TDave$site=="hd"&TDave$year==2017],
		TDave$TD[TDave$site=="hd"&TDave$year==2017]-TDsd$se[TDsd$site=="hd"&TDsd$year==2017],
		TDsd$doy[TDave$site=="hd"&TDave$year==2017],
		TDave$TD[TDave$site=="hd"&TDave$year==2017]+TDsd$se[TDsd$site=="hd"&TDsd$year==2017],
		code=0,lwd=2)		
		
points(TDave$doy[TDave$site=="ld"&TDave$year==2017],TDave$TD[TDave$site=="ld"&TDave$year==2017],
		type="b",pch=19,lwd=2,col="royalblue1",cex=1.5)		
points(TDave$doy[TDave$site=="hd"&TDave$year==2017],TDave$TD[TDave$site=="hd"&TDave$year==2017],
		type="b",pch=19,lwd=2,col="palegreen4",cex=1.5)	
		
box(which="plot")
axis(1,seq(165,200,by=5),cex.axis=1.5)
mtext("Day of year",side=1,line=-2,outer=TRUE,cex=1.75)