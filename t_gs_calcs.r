###############################################################################
###########This code is for organizing raw sensor data ########################
###########and converting sensor data to transpiration ########################
###########and canopy stomatal conductance             ########################
###############################################################################

#load libraries
library(lubridate)


#set wd
setwd("c:\\Users\\hkropp\\Google Drive\\Viper_SF")

#read in data from each stand gradient
datH<-read.table("high_density_TDP.csv", sep=",", head=TRUE, na.strings=c("NAN"))
datL<-read.table("low_density_TDP.csv", sep=",", head=TRUE, na.strings=c("NAN"))

#read in sensor size

#tree name

#aspect

#diam

#sapwood depth




#tower was put up on 6/28/16 and all set up at Davy wrapped up by 6/29/16
#tower at LDF2 6/30 with set up  finishing on 7/1/16