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
dateH<-as.Date(datH$TIMESTAMP)

datH$hour<-ifelse(datH$JHM/100-floor(datH$JHM/100)==0,datH$JHM/100,floor(datH$JHM/100)+.5)
#now convert to day of year

#high one day ahead
datH$doy<-datH$doy-1

#need to create new doy index that defines day of year between 5am and 5am

datH$doy5<-ifelse(datH$hour<=5,datH$doy-1,datH$doy)