###########################################################
###########################################################
#This code contains plots for visualizing root biomass data

#set working director
setwd("c:\\Users\\hkropp\\Google Drive\\root_16")

#read in data set
datr<-read.csv("compiled_data.csv")

#make plots to look at the data across the soil profile for each sample date
#and site
#there will be 2 sample dates for the low density site
#and 3 sample dates for the high sensity site

#check all sample dates for each site
sampleN<-unique(data.frame(doy=datr$DOY, site=datr$site))

#doy 188 and 184 for the low density will be considered the same sampling
#period since we had to go back to finish up deeper roots
#get range of data for fine and coarse
crl<-range(datr$bio[datr$type=="c"&datr$status=="l"&datr$site=="l"])
crh<-range(datr$bio[datr$type=="c"&datr$status=="l"&datr$site=="h"])
frh<-range(datr$bio[datr$type=="f"&datr$status=="l"&datr$site=="h"])
frl<-range(datr$bio[datr$type=="f"&datr$status=="l"&datr$site=="l"])

#########
#make plots that show the time sequence
#only 2 time points for low density
par(mfrow=c(1,2))
#plot fine root biomass
plot(datr$bio[datr$site=="l"&datr$type=="f"&datr$status=="l"&datr$DOY<190&datr$loc=="t"],
	datr$mid[datr$site=="l"&datr$type=="f"&datr$status=="l"&datr$DOY<190&datr$loc=="t"],
	ylim=c(100,0), xlim=c(0,25),pch=19, col="darkgreen",xaxs="i", yaxs="i",
	xlab="Fine root biomass (mg/cm3)", ylab="Midpoint (cm)", cex=1.5,
	main="Early July")
points(datr$bio[datr$site=="l"&datr$type=="f"&datr$status=="l"&datr$DOY<190&datr$loc=="s"],
	datr$mid[datr$site=="l"&datr$type=="f"&datr$status=="l"&datr$DOY<190&datr$loc=="s"],
	col="chocolate2", cex=1.5, pch=19)
#add a legend
legend(20,0,c("tree","shrub"), col=c("darkgreen","chocolate2"),pch=19,cex=1.5, bty="n")
#plot course root biomass
plot(datr$bio[datr$site=="l"&datr$type=="f"&datr$status=="l"&datr$DOY==208&datr$loc=="t"],
	datr$mid[datr$site=="l"&datr$type=="f"&datr$status=="l"&datr$DOY==208&datr$loc=="t"],
	ylim=c(100,0), xlim=c(0,25),pch=19, col="darkgreen",xaxs="i", yaxs="i",
	xlab="Fine root biomass (mg/cm3)", ylab="Midpoint (cm)", cex=1.5,
	main="End of July")
points(datr$bio[datr$site=="l"&datr$type=="f"&datr$status=="l"&datr$DOY==208&datr$loc=="s"],
	datr$mid[datr$site=="l"&datr$type=="f"&datr$status=="l"&datr$DOY==208&datr$loc=="s"],
	col="chocolate2", cex=1.5, pch=19)
#add a legend
legend(20,0,c("tree","shrub"), col=c("darkgreen","chocolate2"),pch=19,cex=1.5, bty="n")
#plot title
mtext(" Low Density", side=3, outer=TRUE,line=-2,cex=2)


##########
#now look at coarse biomass
#plot coarse root biomass
plot(datr$bio[datr$site=="l"&datr$type=="c"&datr$status=="l"&datr$DOY<190&datr$loc=="t"],
	datr$mid[datr$site=="l"&datr$type=="c"&datr$status=="l"&datr$DOY<190&datr$loc=="t"],
	ylim=c(100,0), xlim=c(0,30),pch=19, col="darkgreen",xaxs="i", yaxs="i",
	xlab="Coarse root biomass (mg/cm3)", ylab="Midpoint (cm)", cex=1.5,
	main="Early July")
points(datr$bio[datr$site=="l"&datr$type=="c"&datr$status=="l"&datr$DOY<190&datr$loc=="s"],
	datr$mid[datr$site=="l"&datr$type=="c"&datr$status=="l"&datr$DOY<190&datr$loc=="s"],
	col="chocolate2", cex=1.5, pch=19)
#add a legend
legend(20,0,c("tree","shrub"), col=c("darkgreen","chocolate2"),pch=19,cex=1.5, bty="n")
#plot course root biomass
plot(datr$bio[datr$site=="l"&datr$type=="c"&datr$status=="l"&datr$DOY==208&datr$loc=="t"],
	datr$mid[datr$site=="l"&datr$type=="c"&datr$status=="l"&datr$DOY==208&datr$loc=="t"],
	ylim=c(100,0), xlim=c(0,30),pch=19, col="darkgreen",xaxs="i", yaxs="i",
	xlab="Coarse root biomass (mg/cm3)", ylab="Midpoint (cm)", cex=1.5,
	main="End of July")
points(datr$bio[datr$site=="l"&datr$type=="c"&datr$status=="l"&datr$DOY==208&datr$loc=="s"],
	datr$mid[datr$site=="l"&datr$type=="c"&datr$status=="l"&datr$DOY==208&datr$loc=="s"],
	col="chocolate2", cex=1.5, pch=19)
#add a legend
legend(20,0,c("tree","shrub"), col=c("darkgreen","chocolate2"),pch=19,cex=1.5, bty="n")
#plot title
mtext(" Low Density", side=3, outer=TRUE,line=-2,cex=2)

#look as small fine root biomass
par(mfrow=c(1,2))
#plot fine root biomass
plot(datr$bio[datr$site=="l"&datr$type=="f"&datr$status=="l"&datr$DOY<190&datr$loc=="t"],
	datr$mid[datr$site=="l"&datr$type=="f"&datr$status=="l"&datr$DOY<190&datr$loc=="t"],
	ylim=c(100,0), xlim=c(0,1),pch=19, col="darkgreen",xaxs="i", yaxs="i",
	xlab="Fine root biomass (mg/cm3)", ylab="Midpoint (cm)", cex=1.5,
	main="Early July")
points(datr$bio[datr$site=="l"&datr$type=="f"&datr$status=="l"&datr$DOY<190&datr$loc=="s"],
	datr$mid[datr$site=="l"&datr$type=="f"&datr$status=="l"&datr$DOY<190&datr$loc=="s"],
	col="chocolate2", cex=1.5, pch=19)
#add a legend
legend(20,0,c("tree","shrub"), col=c("darkgreen","chocolate2"),pch=19,cex=1.5, bty="n")
#plot course root biomass
plot(datr$bio[datr$site=="l"&datr$type=="f"&datr$status=="l"&datr$DOY==208&datr$loc=="t"],
	datr$mid[datr$site=="l"&datr$type=="f"&datr$status=="l"&datr$DOY==208&datr$loc=="t"],
	ylim=c(100,0), xlim=c(0,1),pch=19, col="darkgreen",xaxs="i", yaxs="i",
	xlab="Fine root biomass (mg/cm3)", ylab="Midpoint (cm)", cex=1.5,
	main="End of July")
points(datr$bio[datr$site=="l"&datr$type=="f"&datr$status=="l"&datr$DOY==208&datr$loc=="s"],
	datr$mid[datr$site=="l"&datr$type=="f"&datr$status=="l"&datr$DOY==208&datr$loc=="s"],
	col="chocolate2", cex=1.5, pch=19)
#add a legend
legend(20,0,c("tree","shrub"), col=c("darkgreen","chocolate2"),pch=19,cex=1.5, bty="n")
#plot title
mtext(" Low Density", side=3, outer=TRUE,line=-2,cex=2)


#########
#make plots that show the time sequence
#only 3 time points for high density
par(mfrow=c(1,3))
#plot fine root biomass
plot(datr$bio[datr$site=="h"&datr$type=="f"&datr$status=="l"&datr$DOY==185&datr$loc=="t"],
	datr$mid[datr$site=="h"&datr$type=="f"&datr$status=="l"&datr$DOY==185&datr$loc=="t"],
	ylim=c(60,0), xlim=c(0,25),pch=19, col="darkgreen",xaxs="i", yaxs="i",
	xlab="Fine root biomass (mg/cm3)", ylab="Midpoint (cm)", cex=1.5,
	main="Early July")
points(datr$bio[datr$site=="h"&datr$type=="f"&datr$status=="l"&datr$DOY==185&datr$loc=="s"],
	datr$mid[datr$site=="h"&datr$type=="f"&datr$status=="l"&datr$DOY==185&datr$loc=="s"],
	col="chocolate2", cex=1.5, pch=19)
#add a legend
legend(20,0,c("tree","shrub"), col=c("darkgreen","chocolate2"),pch=19,cex=1.5, bty="n")
#plot mid july root biomass
plot(datr$bio[datr$site=="h"&datr$type=="f"&datr$status=="l"&datr$DOY==197&datr$loc=="t"],
	datr$mid[datr$site=="h"&datr$type=="f"&datr$status=="l"&datr$DOY==197&datr$loc=="t"],
	ylim=c(60,0), xlim=c(0,25),pch=19, col="darkgreen",xaxs="i", yaxs="i",
	xlab="Fine root biomass (mg/cm3)", ylab="Midpoint (cm)", cex=1.5,
	main="Mid July")
points(datr$bio[datr$site=="h"&datr$type=="f"&datr$status=="l"&datr$DOY==197&datr$loc=="s"],
	datr$mid[datr$site=="h"&datr$type=="f"&datr$status=="l"&datr$DOY==197&datr$loc=="s"],
	col="chocolate2", cex=1.5, pch=19)
#add a legend
legend(20,0,c("tree","shrub"), col=c("darkgreen","chocolate2"),pch=19,cex=1.5, bty="n")
#plot end of july root biomass
plot(datr$bio[datr$site=="h"&datr$type=="f"&datr$status=="l"&datr$DOY==208&datr$loc=="t"],
	datr$mid[datr$site=="h"&datr$type=="f"&datr$status=="l"&datr$DOY==208&datr$loc=="t"],
	ylim=c(60,0), xlim=c(0,25),pch=19, col="darkgreen",xaxs="i", yaxs="i",
	xlab="Fine root biomass (mg/cm3)", ylab="Midpoint (cm)", cex=1.5,
	main="End of July")
points(datr$bio[datr$site=="h"&datr$type=="f"&datr$status=="l"&datr$DOY==208&datr$loc=="s"],
	datr$mid[datr$site=="h"&datr$type=="f"&datr$status=="l"&datr$DOY==208&datr$loc=="s"],
	col="chocolate2", cex=1.5, pch=19)
#add a legend
legend(20,0,c("tree","shrub"), col=c("darkgreen","chocolate2"),pch=19,cex=1.5, bty="n")
#plot title
mtext(" High Density", side=3, outer=TRUE,line=-1.75,cex=2)

#########
#make plots that show the time sequence
#only 3 time points for high density
par(mfrow=c(1,3))
#plot fine root biomass
plot(datr$bio[datr$site=="h"&datr$type=="f"&datr$status=="l"&datr$DOY==185&datr$loc=="t"],
	datr$mid[datr$site=="h"&datr$type=="f"&datr$status=="l"&datr$DOY==185&datr$loc=="t"],
	ylim=c(60,0), xlim=c(0,1),pch=19, col="darkgreen",xaxs="i", yaxs="i",
	xlab="Fine root biomass (mg/cm3)", ylab="Midpoint (cm)", cex=1.5,
	main="Early July")
points(datr$bio[datr$site=="h"&datr$type=="f"&datr$status=="l"&datr$DOY==185&datr$loc=="s"],
	datr$mid[datr$site=="h"&datr$type=="f"&datr$status=="l"&datr$DOY==185&datr$loc=="s"],
	col="chocolate2", cex=1.5, pch=19)
#add a legend
legend(20,0,c("tree","shrub"), col=c("darkgreen","chocolate2"),pch=19,cex=1.5, bty="n")
#plot mid july root biomass
plot(datr$bio[datr$site=="h"&datr$type=="f"&datr$status=="l"&datr$DOY==197&datr$loc=="t"],
	datr$mid[datr$site=="h"&datr$type=="f"&datr$status=="l"&datr$DOY==197&datr$loc=="t"],
	ylim=c(60,0), xlim=c(0,1),pch=19, col="darkgreen",xaxs="i", yaxs="i",
	xlab="Fine root biomass (mg/cm3)", ylab="Midpoint (cm)", cex=1.5,
	main="Mid July")
points(datr$bio[datr$site=="h"&datr$type=="f"&datr$status=="l"&datr$DOY==197&datr$loc=="s"],
	datr$mid[datr$site=="h"&datr$type=="f"&datr$status=="l"&datr$DOY==197&datr$loc=="s"],
	col="chocolate2", cex=1.5, pch=19)
#add a legend
legend(20,0,c("tree","shrub"), col=c("darkgreen","chocolate2"),pch=19,cex=1.5, bty="n")
#plot end of july root biomass
plot(datr$bio[datr$site=="h"&datr$type=="f"&datr$status=="l"&datr$DOY==208&datr$loc=="t"],
	datr$mid[datr$site=="h"&datr$type=="f"&datr$status=="l"&datr$DOY==208&datr$loc=="t"],
	ylim=c(60,0), xlim=c(0,1),pch=19, col="darkgreen",xaxs="i", yaxs="i",
	xlab="Fine root biomass (mg/cm3)", ylab="Midpoint (cm)", cex=1.5,
	main="End of July")
points(datr$bio[datr$site=="h"&datr$type=="f"&datr$status=="l"&datr$DOY==208&datr$loc=="s"],
	datr$mid[datr$site=="h"&datr$type=="f"&datr$status=="l"&datr$DOY==208&datr$loc=="s"],
	col="chocolate2", cex=1.5, pch=19)

#########
#make plots that show the time sequence
#only 3 time points for high density
par(mfrow=c(1,3))
#plot fine root biomass
plot(datr$bio[datr$site=="h"&datr$type=="c"&datr$status=="l"&datr$DOY==185&datr$loc=="t"],
	datr$mid[datr$site=="h"&datr$type=="c"&datr$status=="l"&datr$DOY==185&datr$loc=="t"],
	ylim=c(60,0), xlim=c(0,70),pch=19, col="darkgreen",xaxs="i", yaxs="i",
	xlab="Course root biomass (mg/cm3)", ylab="Midpoint (cm)", cex=1.5,
	main="Early July")
points(datr$bio[datr$site=="h"&datr$type=="c"&datr$status=="l"&datr$DOY==185&datr$loc=="s"],
	datr$mid[datr$site=="h"&datr$type=="c"&datr$status=="l"&datr$DOY==185&datr$loc=="s"],
	col="chocolate2", cex=1.5, pch=19)
#add a legend
legend(50,0,c("tree","shrub"), col=c("darkgreen","chocolate2"),pch=19,cex=1.5, bty="n")
#plot mid july root biomass
plot(datr$bio[datr$site=="h"&datr$type=="c"&datr$status=="l"&datr$DOY==197&datr$loc=="t"],
	datr$mid[datr$site=="h"&datr$type=="c"&datr$status=="l"&datr$DOY==197&datr$loc=="t"],
	ylim=c(60,0), xlim=c(0,70),pch=19, col="darkgreen",xaxs="i", yaxs="i",
	xlab="Course root biomass (mg/cm3)", ylab="Midpoint (cm)", cex=1.5,
	main="Mid July")
points(datr$bio[datr$site=="h"&datr$type=="c"&datr$status=="l"&datr$DOY==197&datr$loc=="s"],
	datr$mid[datr$site=="h"&datr$type=="c"&datr$status=="l"&datr$DOY==197&datr$loc=="s"],
	col="chocolate2", cex=1.5, pch=19)
#add a legend
legend(50,0,c("tree","shrub"), col=c("darkgreen","chocolate2"),pch=19,cex=1.5, bty="n")
#plot end of july root biomass
plot(datr$bio[datr$site=="h"&datr$type=="c"&datr$status=="l"&datr$DOY==208&datr$loc=="t"],
	datr$mid[datr$site=="h"&datr$type=="c"&datr$status=="l"&datr$DOY==208&datr$loc=="t"],
	ylim=c(60,0), xlim=c(0,70),pch=19, col="darkgreen",xaxs="i", yaxs="i",
	xlab="Course root biomass (mg/cm3)", ylab="Midpoint (cm)", cex=1.5,
	main="End of July")
points(datr$bio[datr$site=="h"&datr$type=="c"&datr$status=="l"&datr$DOY==208&datr$loc=="s"],
	datr$mid[datr$site=="h"&datr$type=="c"&datr$status=="l"&datr$DOY==208&datr$loc=="s"],
	col="chocolate2", cex=1.5, pch=19)
#add a legend
legend(50,0,c("tree","shrub"), col=c("darkgreen","chocolate2"),pch=19,cex=1.5, bty="n")
#plot title
mtext(" High Density", side=3, outer=TRUE,line=-2.5,cex=2)