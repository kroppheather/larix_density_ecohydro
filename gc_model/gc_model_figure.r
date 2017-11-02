###########################################################################
###########################################################################
############## Created by Heather Kropp in October 2017      ##############
############## Extracts paramters and plots from gc model    ##############
###########################################################################



#libraries
library(plyr)

#set plot directory
plotDI <- "c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\gc_model\\plots\\run13"


#read in stand day data

daySD <- read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\gc_model\\run13\\out\\standDayTreedata.csv")
datgc <-read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\gc_model\\run13\\out\\gcdata.csv")
#################################################################
####read in parameters                                    #######
#################################################################
datM <- read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\gc_model\\run13\\out\\mod_stats.csv")
datQ <- read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\gc_model\\run13\\out\\mod_quants.csv")

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
datlslope <- datC[datC$parms3=="lsope",]
datrep <- datC[datC$parms3=="repgs",]
#add ind
datS <- cbind(datS,daySD)
datgref <- cbind(datgref,daySD)
datparm <- datC[datC$parms3=="a"|datC$parms3=="b"|datC$parms3=="d",]

#################################################################
####goodness of fit plots                                 #######
#################################################################
plot(datgc$g.c,datrep$Mean, pch=19, xlim=c(0,200), ylim=c(0,200))
fit<-lm(datrep$Mean~datgc$g.c)
summary(fit)

plot(datgc$g.c[datgc$treeID.new==13],datrep$Mean[datgc$treeID.new==13], pch=19, xlim=c(0,400), ylim=c(0,400))
fit2<-lm(datrep$Mean[datgc$treeID.new==13]~datgc$g.c[datgc$treeID.new=13])
summary(fit2)


#Tair mean =13.5
#################################################################
####parameter plots                                       #######
#################################################################
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

jpeg(paste0(plotDI , "\\regression_coeff.jpg"), width=2300, height=2000, units="px", quality=100)
	ab <- layout(matrix(seq(1,4), ncol=2, byrow=FALSE), width=rep(lcm(wd),4), height=rep(lcm(hd),4))
	
par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1), type="n", xlim=c(Tmin,Tmax), ylim=c(gmin,gmax), axes=FALSE, xaxs="i", yaxs="i",
			xlab=" ", ylab=" ")
	arrows(datgrefA$Tair, datgrefA$X2.5.,datgrefA$Tair, datgrefA$X97.5.,lwd=2,code=0 )
	points(datgrefA$Tair[datgrefA$stand==1], datgrefA$Mean[datgrefA$stand==1],
			col=colL, pch=19, cex=4)
	points(datgrefA$Tair[datgrefA$stand==2], datgrefA$Mean[datgrefA$stand==2],
			col=colH, pch=19, cex=4)			
	

	axis(2,seq(gmin,gmax, by=10), cex.axis=axisC, las=2, lwd.ticks=3)
	mtext("Reference canopy conductance",side=2, line=18, cex=4)
	mtext(expression(paste("(g"[ref],", mmol m"^"-2", "s"^"-1",")")),side=2, line=10, cex=4)
	legend(3,100, c("low density", "high density"), col=c(colL,colH), bty="n", cex=4, pch=19)
box(which="plot")	


	
par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1), type="n", xlim=c(Tmin,Tmax), ylim=c(Smin,Smax), axes=FALSE, xaxs="i", yaxs="i",
			xlab=" ", ylab=" ")
	arrows(datgrefA$Tair,datS$X2.5.,datgrefA$Tair,datS$X97.5.,lwd=2,code=0)
	
	points(datgrefA$Tair[datgrefA$stand==1], datS$Mean[datS$stand==1],
			col=colL, pch=19, cex=4)
	points(datgrefA$Tair[datgrefA$stand==2], datS$Mean[datS$stand==2],
			col=colH, pch=19, cex=4)	
	axis(2,seq(Smin,Smax-.1, by=.1), cex.axis=axisC, las=2, lwd.ticks=3)
	axis(1, seq(Tmin,Tmax-5, by=5), rep(" ", length(seq(Tmin,Tmax-5, by=5))), cex.axis=axisC, lwd.ticks=3)
	mtext(seq(Tmin,Tmax-5, by=5),at=seq(Tmin,Tmax-5, by=5), line=4, side=1, cex=3)	
	mtext("Stomatal sensitivity",side=2, line=18, cex=4)
	mtext("(S, -)",side=2, line=10, cex=4)	
	mtext("Average daily temperature (C)",side=1, line=8, cex=4)
box(which="plot")	

par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1), type="n", xlim=c(Pmin,Pmax), ylim=c(gmin,gmax), axes=FALSE, xaxs="i", yaxs="i",
			xlab=" ", ylab=" ")
	arrows(datgrefA$pMean, datgrefA$X2.5.,datgrefA$pMean, datgrefA$X97.5.,lwd=2,code=0)
	arrows(datgrefA$pX2.5., datgrefA$Mean,datgrefA$pX97.5., datgrefA$Mean,lwd=2,code=0)
	
	points(datgrefA$pMean[datgrefA$stand==1], datgrefA$Mean[datgrefA$stand==1],
			col=colL, pch=19, cex=4)
	points(datgrefA$pMean[datgrefA$stand==2], datgrefA$Mean[datgrefA$stand==2],
			col=colH, pch=19, cex=4)
	
box(which="plot")	

par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1), type="n", xlim=c(Pmin,Pmax), ylim=c(Smin,Smax), axes=FALSE, xaxs="i", yaxs="i",
			xlab=" ", ylab=" ")
	arrows(datgrefA$pMean, datS$X2.5.,	datgrefA$pMean, datS$X97.5.,lwd=2,code=0)
	arrows(datgrefA$pX2.5., datS$Mean,datgrefA$pX97.5., datS$Mean,lwd=2,code=0)
	
	points(datgrefA$pMean[datgrefA$stand==1], datS$Mean[datS$stand==1],
			col=colL, pch=19, cex=4)
	points(datgrefA$pMean[datgrefA$stand==2], datS$Mean[datS$stand==2],
			col=colH, pch=19, cex=4)			
	axis(1, seq(Pmin,Pmax, by=5), rep(" ", length(seq(Pmin,Pmax, by=5))), cex.axis=axisC, lwd.ticks=3)
	mtext(seq(Pmin,Pmax, by=5),at=seq(Pmin,Pmax, by=5), line=4, side=1, cex=3)
	mtext("Past precipitation over 3 weeks (mm)",side=1, line=8, cex=4)
box(which="plot")	
	
dev.off()	


#################################################################
####check fit                                             #######
#################################################################
plot(datgc$g.c,mugc$mean, xlim=c(0,400), ylim=c(0,400))
fit1 <- lm(mugc$mean~datgc$g.c)
summary(fit1)


#################################################################
####plot wieghts                                          #######
#################################################################

#set up lag periods
#lagStart <- c(1,2,4,6,8,15)
#lagEnd <- c(1,3,5,7,14,21)


#set up x for low
xseq1 <- c(1,4,7,10,13,16)
xseq2 <- c(2,5,8,11,14,17)
xl <-0
xh <-18

colL <-"royalblue"
colH <- "tomato3"


datw$site <- rep(c(1,2), times=6)
jpeg(paste0(plotDI , "\\weights.jpg"), width=1500, height=1500, units="px", quality=100)
	par(mai=c(3,3,3,3))
	plot(c(0,1),c(0,1), type="n", xlim=c(xl,xh), ylim=c(0,1), xlab=" ", ylab=" ",
		xaxs="i", yaxs="i", axes=FALSE)
	abline(h=1/6, lty=3, lwd=2)	
	for(i in 1:6){
	polygon(c(xseq1[i]-.5,xseq1[i]-.5,xseq1[i]+.5,xseq1[i]+.5),
			c(0,datw$Mean[datw$site==1][i],datw$Mean[datw$site==1][i],0), col=colL, border=FALSE)
	polygon(c(xseq2[i]-.5,xseq2[i]-.5,xseq2[i]+.5,xseq2[i]+.5),
			c(0,datw$Mean[datw$site==2][i],datw$Mean[datw$site==2][i],0), col=colH, border=FALSE)
	}	
	arrows(xseq1,datw$X2.5.[datw$site==1],xseq1,datw$X97.5.[datw$site==1], lwd=2, code=0)
	arrows(xseq2,datw$X2.5.[datw$site==2],xseq2,datw$X97.5.[datw$site==2], lwd=2, code=0)
	legend(1,1, c("low density", "high density", "uniform average"), 
			col=c(colL,colH, "black"),pch=c(15,15,NA), lty=c(NA,NA,3), lwd=c(NA,NA,2), cex=3, bty="n")
	axis(2, seq(0,1, by=.1), cex.axis=3, las=2, lwd.ticks=3)
	axis(1, c(-1,1.5,4.5,7.5,10.5,13.5,16.5,20), rep(" ", 8), lwd.ticks=3)
	mtext(c("1 day", "2-3 days", "4-5 days", "6-7 days", "8-14 days", "15-21 days"),
			at=c(1.5,4.5,7.5,10.5,13.5,16.5), side=1, line=2, cex=3)
	mtext("Importance Weight", side=2, line=6, cex=4)		
	mtext("Precipiation lag time", side=1, line=6, cex=4)		
dev.off()	



#################################################################
####make a plot     of gc                                 #######
#################################################################


#look at how many gc observations in stand day
#stand day11 for low and 51 for high

xl <- .5
xh <- 1.3
yl <- 0
yh <- 210
colL <-"royalblue"
colH <- "tomato3"
dL <-11
dH <-51

jpeg(paste0(plotDI , "\\gc.jpg"), width=1500, height=1500, units="px", quality=100)
	par(mai=c(3,3,3,3))
	plot(c(0,1),c(0,1), type="n", xlim=c(xl,xh), ylim=c(yl,yh), xlab=" ", ylab=" ",
		xaxs="i", yaxs="i", axes=FALSE)
	points(datgc$D[datgc$standDay==dL],datgc$g.c[datgc$standDay==dL], pch=19, col=colL, cex=4)	
	points(datgc$D[datgc$standDay==dH],datgc$g.c[datgc$standDay==dH], pch=19, col=colH, cex=4)
	points(seq(xl,xh,by=.1), datgrefA$Mean[dL]*(1-(datS$Mean[dL]*log(seq(xl,xh,by=.1)))),
			lwd=2, type="l", col=colL)
	points(seq(xl,xh,by=.1), datgrefA$Mean[dH]*(1-(datS$Mean[dH]*log(seq(xl,xh,by=.1)))),
			lwd=2, type="l", col=colH)
	axis(1,seq(.5,1.3, by=.1), rep(" ", length(seq(.5,1.3, by=.1))), cex.axis=3, lwd.ticks=3)	
	mtext(seq(.5,1.3, by=.1), at=seq(.5,1.3, by=.1), side=1, line=3,cex=3)
	axis(2, seq(0,200, by=50), cex.axis=3, lwd.ticks=3, las=2) 
	mtext("Canopy stomatal conductance", side=2, cex=4, line=11)
	mtext(expression(paste("(g"[c],", mmol m"^"-2", "s"^"-1",")")),side=2, line=5, cex=4)
	mtext("Vapor pressure deficity (D, kPa)", side=1, cex=4, line=7)
	legend(.55,200, c("low density gc", "high density gc", "low density model", "high density model"),
			col=c(colL,colH,colL,colH), lwd=c(NA,NA,3,3), pch=c(19,19,NA,NA), cex=3, bty="n")
dev.off()		