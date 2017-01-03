#set wd
setwd("c:\\Users\\hkropp\\Google Drive\\root_analysis")

#read in soil moisture data
datS<-read.csv("soil_moisture.csv")
#read in plant water potential data
datwp<-read.csv("density_water potential.csv")

#see how many observations of soil moisture for each date and
#stand
swc.n<-aggregate(datS$SWC,by=list(datS$site,datS$location,datS$period), 
		FUN="length")
#see how many water potential observations across day and location
wp.n<-aggregate(datwp$wp,by=list(datwp$Species,datwp$Site,datwp$DOY),
				FUN="length")
				
#read in met data for vpd to match up with water potential				
lmet<-read.csv("LDF2_canopy_all.csv")
hmet<-read.csv("davydov_canopy_all.csv")

#convert timestamp
library(lubridate)
##low density
ldate<-as.Date(lmet$time, "%m/%d/%Y %H:%M")
#get doy
ldoy<-yday(ldate)
#get year
lyear<-year(ldate)

#get time
ltime<-strptime(lmet$time,format="%m/%d/%Y %H:%M")
#get hour
lhour<-hour(ltime)
#get minute
lt<-lhour+(minute(ltime)/60)
#put into a dataframe
low.T<-data.frame(doy.E=ldoy,year=lyear,ltime=ltime,EDT.h=lt)

##high density
hdate<-as.Date(hmet$time, "%m/%d/%Y %H:%M")
#get doy
hdoy<-yday(hdate)
#get year
hyear<-year(hdate)

#get time
htime<-strptime(hmet$time,format="%m/%d/%Y %H:%M")
#get hour
hhour<-hour(htime)
#get minute
ht<-hhour+(minute(htime)/60)
#put into a dataframe
high.T<-data.frame(year=hyear,htime=htime,doy.E=hdoy,EDT.h=ht)


#now convert to siberian time which is +15 hours to EDT
CherskiyT<-function(hour,doy){
					list(hour=ifelse(hour<9,hour+15,hour-9),
						doy=ifelse(hour<9,doy,doy+1))
					}

low.T$Ch.h<-CherskiyT(low.T$EDT.h,low.T$doy.E)$hour
low.T$Ch.doy<-CherskiyT(low.T$EDT.h,low.T$doy.E)$doy

high.T$Ch.h<-CherskiyT(high.T$EDT.h,high.T$doy.E)$hour
high.T$Ch.doy<-CherskiyT(high.T$EDT.h,high.T$doy.E)$doy

#now check the days to see what is missing and if the
#duplicate day is part of a larger issue
lDays<-aggregate(low.T$Ch.h,by=list(low.T$Ch.doy), FUN="length")
hDays<-aggregate(high.T$Ch.h,by=list(high.T$Ch.doy), FUN="length")



##high
#decagon jumps ahead on 6/29/16 at 3:00am to 6/30/16 at 3:30am 
#this appears to be fixed on doy 197 (in data) 7/15/16 and results in duplicate
#days for the 15 and 16
##low 
#decagon jumps ahead on 6/29/16 at 11:00pm to 6/30/16 at 12:00am 
#this appears to be fixed on doy 198 (in data) 7/16/16 and results in duplicate
#days for the 15 and 16

##looks like the data loggers started off one day behind: 
#davydov data was downloaded on 7/15/16 and LDF2 on 7/16/16
#tower was put up on 6/28/16 and all set up at Davy wrapped up by 6/29/16
#tower at LDF2 6/30 with set up  finishing on 7/1/16

#both measurements have some measurements excluded because the sensors
#were logging continously before/during set up and appear to take a little
#bit of time to get to consistent patterns

##high density
#the data from the second 7/15 at 2:00am is the correct timestamp
#this would be 7/15 at 5pm
#the time is correct leading up to it, it is just off by a day
#this starts at line 765

##low density
#the data from the second 7/16 at 2:30am is the correct timestamp
#this would be 7/16 at 5:30 pm
#the time is correct leading up to it, it is just off by a day
#this starts at line 729

fixedTH<-data.frame(time=high.T$Ch.h,
					doy=c(high.T$Ch.doy[1:764]-1,high.T$Ch.doy[765:2964]))

fixedTL<-data.frame(time=low.T$Ch.h,
					doy=c(low.T$Ch.doy[1:728]-1,low.T$Ch.doy[729:2874]))
hDaysF<-aggregate(fixedTH$time,by=list(fixedTH$doy), FUN="length")

 lDaysF<-aggregate(fixedTL$time,by=list(fixedTL$doy), FUN="length")
 
 #add fixed days to met data
 lmet$doy<-fixedTL$doy
 lmet$hour<-fixedTL$time
 hmet$doy<-fixedTH$doy
 hmet$hour<-fixedTH$time
#calculate VPD
#saturated vapor pressure kpa
e.sat<-function(Temp){0.611*exp((17.502*Temp)/(Temp+240.97))}
#VPD fucntion from Relative humidity
#RH is in % 
VPD<-function(esat,RH.d){esat-((RH.d/100)*esat)}

#filter out any RH at 100% or over
lRHf<-ifelse(lmet$RH>=1,99.9999,lmet$RH*100)
hRHf<-ifelse(hmet$RH>=1,99.9999,hmet$RH*100)


satDL<-e.sat(lmet$temp)
lmet$D<-VPD(satDL,lRHf)

satDH<-e.sat(hmet$temp)
hmet$D<-VPD(satDH,hRHf)

#now match up each D to water potential
#start by subsetting water potential into 
#high and low density data frames
#firtst pull out time info and  rounding rules
#first deal with data not being accepted as time
#Mins now seperated by a decimal
Ttemp1<-as.numeric(gsub(":",".",datwp$time))
#pull out hour
Thour<-floor(Ttemp1)
#adjust minutes to be proportion in an hour 
#while accounting for being placed after the decimal above
ThourM<-Thour+(((Ttemp1-Thour)*100)/60)
Tmins<-ThourM-Thour

#now need to round to be to the closest measurement
Hround<-ifelse(Tmins<=.25,Thour,
		ifelse(Tmins>.25&Tmins<=.5,Thour+.5,
		ifelse(Tmins>.5&Tmins<=.75,Thour+.5,
		ifelse(Tmins>.75,Thour+1,NA))))

datwp$measT<-Hround

wpH<-datwp[datwp$Site=="h",]
wpL<-datwp[datwp$Site=="l",]

#combine with VPD data
library(plyr)

colnames(lmet)<-c("time", "RH", "temp","DOY","measT","D")
colnames(hmet)<-c("time", "RH", "temp","DOY","measT","D")
Low.WP<-join(wpL,lmet,by=c("DOY","measT"),type="left")
High.WP<-join(wpH,hmet,by=c("DOY","measT"),type="left")
par(mfrow=c(1,2))
plot(log(Low.WP$D[Low.WP$Species=="larix"]),Low.WP$wp[Low.WP$Species=="larix"],pch=19, ylim=c(0,2))
points(log(Low.WP$D[Low.WP$Species=="salix"]),Low.WP$wp[Low.WP$Species=="salix"],pch=19,col="darkgoldenrod3")
points(log(Low.WP$D[Low.WP$Species=="betula"]),Low.WP$wp[Low.WP$Species=="betula"],pch=19,col="red3")

low.larch<-lm(Low.WP$wp[Low.WP$Species=="larix"]~log(Low.WP$D[Low.WP$Species=="larix"]))
summary(low.larch)
abline(low.larch)
#not significant for shrubs
low.bet<-lm(Low.WP$wp[Low.WP$Species=="salix"]~log(Low.WP$D[Low.WP$Species=="salix"]))
summary(low.bet)
low.sal<-lm(Low.WP$wp[Low.WP$Species=="betula"]~log(Low.WP$D[Low.WP$Species=="betula"]))
summary(low.sal)


#now look at high:
plot(log(High.WP$D[High.WP$Species=="larix"]),High.WP$wp[High.WP$Species=="larix"],pch=19, col="black", ylim=c(0,2))
points(log(High.WP$D[High.WP$Species=="betula"]),High.WP$wp[High.WP$Species=="betula"],pch=19,col="red3")

high.larch<-lm(High.WP$wp[High.WP$Species=="larix"]~log(High.WP$D[High.WP$Species=="larix"]))
summary(high.larch)
abline(high.larch)
#compile soil moisture sensors from met data
#all met data is in eastern time so it needs +15 hours added to it
Lsm<-read.csv("LDF2_sm_all.csv")
Hsm<-read.csv("davydov_sm_all.csv")

#convert dates
Hdate<-as.Date(Hsm$time, "%m/%d/%Y %I:%M %p")
Hdoy<-yday(Hdate)

Ldate<-as.Date(Lsm$time, "%m/%d/%Y %H:%M")
Ldoy<-yday(Ldate)

#now aggregate to day
HSmat<-matrix(rep(0,length(unique(Hdoy))*6), ncol=6)
LSmat<-matrix(rep(0,length(unique(Ldoy))*6), ncol=6)

for(i in 1:6){
	HSmat[,i]<-aggregate(Hsm[,i+1], by=list(Hdoy), FUN="mean")$x
	LSmat[,i]<-aggregate(Lsm[,i+1], by=list(Ldoy), FUN="mean")$x
}
colnames(HSmat)<-c("swc1", "T1","swc2","T2","swc3","T3")
colnames(LSmat)<-c("swc1", "T1","swc2","T2","swc3","T3")
Hswc<-data.frame(HSmat)
Hswc$doy<-aggregate(Hsm[,2], by=list(Hdoy), FUN="mean")$Group.1
Lswc<-data.frame(LSmat)
Lswc$doy<-aggregate(Lsm[,2], by=list(Ldoy), FUN="mean")$Group.1

#High density swc1 and 2 are at 5 cm and then 3 is at 50cm
#low density swc1 is under a shrub, swc2 is under a tree, swc3 is deep under a shrub at 50cm

#group midpoints into a color category
midgroup<-ifelse(datS$midpoint<=5,1,
		ifelse(datS$midpoint<=10&datS$midpoint>5,2,
		ifelse(datS$midpoint<=15&datS$midpoint>10,3,
		ifelse(datS$midpoint<=20&datS$midpoint>15,4,
		ifelse(datS$midpoint<=30&datS$midpoint>20,5,
		ifelse(datS$midpoint<=40&datS$midpoint>30,6,
		ifelse(datS$midpoint<=60&datS$midpoint>40,7,NA)))))))


#aggregate the SWC across shrub tree, location and  midgoup
datS$midgroup<-midgroup
datSn<-na.omit(datS)
SWave<-aggregate(datSn$SWC,by=list(datSn$doy,datSn$midgroup,datSn$location,datSn$site), FUN="mean")

midcolor<-ifelse(SWave$depth==1,"cadetblue2",
		ifelse(SWave$depth==2,"darkturquoise",
		ifelse(SWave$depth==3,"royalblue2",
		ifelse(SWave$depth==4,"seagreen3",
		ifelse(SWave$depth==5,"springgreen4",
		ifelse(SWave$depth==6,"sienna2",
		ifelse(SWave$depth==7,"tomato3",NA)))))))

colnames(SWave)<-c("doy", "depth", "location", "site","SWC")
SWave$midcolor<-midcolor

#make a plot

par(mfrow=c(1,2))

plot(c(0,1),c(0,1), type="n", xlim=c(184,242), ylim=c(0,.7), xlab="Day of Year", ylab="High density SWC")
points(Hswc$doy,Hswc$swc1, type="l", col="cadetblue3", lwd=2)
points(Hswc$doy,Hswc$swc2,type="l", col="cadetblue4", lwd=2)
points(Hswc$doy,Hswc$swc3,type="l", col="tomato4", lwd=2)

		
points(SWave$doy[SWave$location=="t"&SWave$site=="h"],SWave$SWC[SWave$location=="t"&SWave$site=="h"],
		pch=19, col=SWave$midcolor[SWave$location=="t"&SWave$site=="h"])
		
points(SWave$doy[SWave$location=="s"&SWave$site=="h"]+.5,SWave$SWC[SWave$location=="s"&SWave$site=="h"],
		pch=15, col=SWave$midcolor[SWave$location=="s"&SWave$site=="h"])
		
		legend(185,.7,c("<5","5-10","10-15","15-20","20-30","30-40","40-60"), 
				fill=c("cadetblue2","darkturquoise","royalblue2","seagreen3","springgreen4","sienna2","tomato3"), bty="n")
		legend(220,.7, c("5 cm 1", "5 cm 2", "50cm"), col=c("cadetblue3","cadetblue4", "tomato4"), lwd=2, bty="n")
		
		
plot(c(0,1),c(0,1), type="n", xlim=c(180,242), ylim=c(0,.7), xlab="Day of Year", ylab="Low density SWC")
points(Lswc$doy,Lswc$swc1, type="l", col="cadetblue3", lwd=2)
points(Lswc$doy,Lswc$swc2,type="l", col="cadetblue4", lwd=2)
points(Lswc$doy,Lswc$swc3,type="l", col="tomato4", lwd=2)

		
points(SWave$doy[SWave$location=="t"&SWave$site=="l"],SWave$SWC[SWave$location=="t"&SWave$site=="l"],
		pch=19, col=SWave$midcolor[SWave$location=="t"&SWave$site=="l"])
		
points(SWave$doy[SWave$location=="s"&SWave$site=="l"]+.5,SWave$SWC[SWave$location=="s"&SWave$site=="l"],
		pch=15, col=SWave$midcolor[SWave$location=="s"&SWave$site=="l"])
		
		legend(185,.7,c("<5","5-10","10-15","15-20","20-30","30-40","40-60"), 
				fill=c("cadetblue2","darkturquoise","royalblue2","seagreen3","springgreen4","sienna2","tomato3"), bty="n")
		legend(220,.7, c("5 cm shrub", "5 cm tree", "50cm"), col=c("cadetblue3","cadetblue4", "tomato4"), lwd=2, bty="n")
			
		