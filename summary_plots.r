#############################################
##########Soil Profile Data #################

setwd("c:\\Users\\hkropp\\Google Drive\\root_analysis")
#read in data describing the profile
datP<-read.csv("soil_prof_desc.csv")

#green data frame to omit NAs

#aggregrate the upper profile characterstics
orgD<-aggregate(datP$green, by=list(datP$location,datP$site), FUN="mean")