###########################################################################
###########################################################################
############## Created by Heather Kropp in October 2017      ##############
############## Extracts paramters and plots from gc model    ##############
###########################################################################



#libraries
library(plyr)

#set plot directory
plotDI <- "c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\gc_model\\plots\\run7"


#read in stand day data
daySD <- read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\gc_model\\run7\\out\\standDaydata.csv")
dayS <- read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\gc_model\\run7\\out\\Daysdata.csv")
#################################################################
####read in sapflow data                                  #######
#################################################################
datM <- read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\gc_model\\run7\\out\\mod_stats.csv")
datQ <- read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\gc_model\\run7\\out\\mod_quants.csv")

#stand index of 1 is low

datC <- cbind(datM,datQ)
#extract vector
dexps<-"\\[*[[:digit:]]*\\]"
datC$parms1<-gsub(dexps,"",rownames(datC))
datC$parms2<- gsub("[[:punct:]]", "",rownames(datC))
datC$parms3<- gsub("[[:digit:]]", "",datC$parms2)

#first make a flag designating the sig
datC$Sig <- ifelse(datC$X2.5.<0&datC$X97.5.<0,1,
			ifelse(datC$X2.5.>0&datC$X97.5.>0,1,0))
#now subset

datS <- datC[datC$parms3=="S",]
datgref <- datC[datC$parms3=="gref",]
#add ind
datS <- cbind(datS,daySD)
datgref <- cbind(datgref,daySD)
datparm <- datC[datC$parms3=="a"|datC$parms3=="b",]
datAntPr <- datC[datC$parms3=="pastpr",]
datAntPr$Days <- rep(dayS$Days, each=2)
datAntPr$stand <- rep(seq(1,2), times=68)
colnames(datAntPr)[1:13] <- paste0("p",colnames(datAntPr)[1:13])
#now join into datgref
datgrefA <- join(datgref, datAntPr, by=c("Days","stand"), type="left")

#Tair mean =13.5

#create a plot of the two regressions

wd <- 30
hd <- 30
gmin <-0
gmax <- 100
Smin <-0
Smax <-1.7
Tmin <-0
Tmax <- 25
Pmin <-0
Pmax <- 20
colL <-"royalblue"
colH <- "tomato3"
axisC <-3

jpeg(paste0(plotDI , "\\regression_coeff.jpg"), width=2000, height=2000, units="px")
	ab <- layout(matrix(seq(1,4), ncol=2, byrow=FALSE), width=rep(lcm(wd),4), height=rep(lcm(hd),4))
	
par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1), type="n", xlim=c(Tmin,Tmax), ylim=c(gmin,gmax), axes=FALSE, xaxs="i", yaxs="i",
			xlab=" ", ylab=" ")
	points(datgrefA$Tair[datgrefA$stand==1], datgrefA$Mean[datgrefA$stand==1],
			col=colL, pch=19, cex=3)
	points(datgrefA$Tair[datgrefA$stand==2], datgrefA$Mean[datgrefA$stand==2],
			col=colH, pch=19, cex=3)			
	
	points(seq(Tmin,Tmax, by=.1), datparm[1,1]+(datparm[3,1]*(seq(Tmin,Tmax, by=.1)-13.5)), type="l",
			col=colL, lwd=6)
	points(seq(Tmin,Tmax, by=.1), datparm[2,1]+(datparm[4,1]*(seq(Tmin,Tmax, by=.1)-13.5)), type="l",
			col=colH, lwd=6)
box(which="plot")	


	
par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1), type="n", xlim=c(Tmin,Tmax), ylim=c(Smin,Smax), axes=FALSE, xaxs="i", yaxs="i",
			xlab=" ", ylab=" ")
	points(datgrefA$Tair[datgrefA$stand==1], datS$Mean[datS$stand==1],
			col=colL, pch=19, cex=3)
	points(datgrefA$Tair[datgrefA$stand==2], datS$Mean[datS$stand==2],
			col=colH, pch=19, cex=3)	
	points(seq(Tmin,Tmax, by=.1), datparm[7,1]+(datparm[9,1]*(seq(Tmin,Tmax, by=.1)-13.5)), type="l",
		col=colL, lwd=6)	
	points(seq(Tmin,Tmax, by=.1), datparm[8,1]+(datparm[10,1]*(seq(Tmin,Tmax, by=.1)-13.5)), type="l",
		col=colH, lwd=6)	
	axis(1, seq(Tmin,Tmax, by=5), cex.axis=axisC, lwd.ticks=3)
			
box(which="plot")	

par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1), type="n", xlim=c(Pmin,Pmax), ylim=c(gmin,gmax), axes=FALSE, xaxs="i", yaxs="i",
			xlab=" ", ylab=" ")
	points(datgrefA$pMean[datgrefA$stand==1], datgrefA$Mean[datgrefA$stand==1],
			col=colL, pch=19, cex=3)
	points(datgrefA$pMean[datgrefA$stand==2], datgrefA$Mean[datgrefA$stand==2],
			col=colH, pch=19, cex=3)
box(which="plot")	

par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1), type="n", xlim=c(Pmin,Pmax), ylim=c(Smin,Smax), axes=FALSE, xaxs="i", yaxs="i",
			xlab=" ", ylab=" ")
			
	points(datgrefA$pMean[datgrefA$stand==1], datS$Mean[datS$stand==1],
			col=colL, pch=19, cex=3)
	points(datgrefA$pMean[datgrefA$stand==2], datS$Mean[datS$stand==2],
			col=colH, pch=19, cex=3)			

box(which="plot")	
	
dev.off()	