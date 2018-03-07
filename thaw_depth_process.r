###########################################################################
###########################################################################
############## Created by Heather Kropp in October 2017      ##############
############## This script interpolates a daily thaw depth   ##############
###########################################################################
###########################################################################
############## output: TDall: interpolated thaw depth        ##############
############## by stand and year                             ##############
###########################################################################
###########################################################################

library(lubridate)
library(plyr)
library(zoo)

#################################################################
####read in datafiles                                     #######
#################################################################
#read in thaw depth data
datTH <- read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\individual_data\\thaw_depth.csv")
datTH17 <- read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\individual_data\\thaw_depth17.csv")

#omit any rows that may have an NA in the thaw depth column
datTH <- datTH[is.na(datTH$TD)==FALSE,]
datTH17 <- datTH17[is.na(datTH17$TD)==FALSE,]


#################################################################
####organize date                                         #######
#################################################################
dateTH <- as.Date(datTH$Date, "%m/%d/%Y")
datTH$doy <- yday(dateTH)
datTH$year <- year(dateTH)

#################################################################
####organize data                                         #######
#################################################################

#aggregate directions around trees in 2017
tTH17 <- datTH17[is.na(datTH17$Tree)==FALSE,]
mTH17 <- datTH17[is.na(datTH17$Tree),]

sTH17 <- aggregate(tTH17$TD, by=list(tTH17$site,tTH17$doy,tTH17$year,tTH17$Tree),FUN="mean") 
colnames(sTH17) <- c("site","doy","year","Tree","TD")

#now aggregate days for both types of data

d1TH17 <- aggregate(mTH17$TD, by=list(mTH17$site,mTH17$doy,mTH17$year),FUN="mean")
d2TH17 <- aggregate(sTH17$TD, by=list(sTH17$site,sTH17$doy,sTH17$year),FUN="mean")

#bind together
TH17 <- rbind(d1TH17, d2TH17)
colnames(TH17) <- c("site", "doy", "year", "TD")

#aggregate 2016 data

TH16 <- aggregate(datTH$TD, by=list(datTH$site,datTH$doy,datTH$year), FUN="mean")
colnames(TH16) <- c("site", "doy", "year", "TD")

#seperate into stands
hTH17 <- TH17[TH17$site=="hd",]
lTH17 <- TH17[TH17$site=="ld",]
hTH16 <- TH16[TH16$site=="hd",]
lTH16 <- TH16[TH16$site=="ld",]


#################################################################
####organize data                                         #######
#################################################################

daysAll16 <- data.frame(doy=seq(184,243), year=rep(2016,length(seq(184,243))))
daysAll17 <- data.frame(doy=seq(148,225), year=rep(2017,length(seq(148,225))))

#join each stand into all days
h2016 <- join(daysAll16, hTH16, by=c("doy","year"), type="left")
h2016$site <- rep("hd", dim(h2016)[1])
l2016 <- join(daysAll16, lTH16, by=c("doy","year"), type="left")
l2016$site <- rep("ld", dim(l2016)[1])

h2017 <- join(daysAll17, hTH17, by=c("doy","year"), type="left")
h2017$site <- rep("hd", dim(h2017)[1])
l2017 <- join(daysAll17, lTH17, by=c("doy","year"), type="left")
l2017$site <- rep("ld", dim(l2017)[1])

#get the number that the first measurement starts on


h2016$TDday[h2016$doy>=188] <- na.approx(h2016$TD[h2016$doy>=188])
#get the first approximation increment and fill in remaining days
hrate <- h2016$TDday[h2016$doy==189]- h2016$TDday[h2016$doy==188]
h2016$TDday[h2016$doy<188] <- h2016$TDday[h2016$doy==188]-(hrate*(188-h2016$doy[h2016$doy<188]))

l2016$TDday<- na.approx(l2016$TD)

h2017$TDday <- na.approx(h2017$TD)

l2017$TDday[l2017$doy<=224] <- na.approx(l2017$TD[l2017$doy<=224])
#fill in last day
lrate <- l2017$TDday[l2017$doy==224]-l2017$TDday[l2017$doy==223]
l2017$TDday[l2017$doy>224] <- l2017$TDday[l2017$doy==224] +(lrate*(l2017$doy[l2017$doy>224]-224))


TDall <- rbind(h2016,l2016,h2017,l2017)

rm(list=c("datTH","datTH17","dateTH","tTH17","mTH17","sTH17","d1TH17","d2TH17","TH17","TH16",
			"hTH17","lTH17","hTH16","lTH16",
			"daysAll16","daysAll17","h2016","l2016","l2017","h2017","lrate","hrate"))