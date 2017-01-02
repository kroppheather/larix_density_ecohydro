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

#now need to round to closest measurement
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
plot(Low.WP$D,Low.WP$wp,pch=19, ylim=c(0,2), xlim=c(0,2.7))
points(High.WP$D,High.WP$wp,pch=19, col="cornflowerblue")

#compile soil moisture sensors from met data
#all met data is in eastern time so it needs +15 hours added to it