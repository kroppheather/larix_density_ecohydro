###########################################################################
###########################################################################
############## Created by Heather Kropp in October 2017      ##############
############## This script creates figures for all time      ##############
############## series data.                                  ##############
###########################################################################
###########################################################################
############## Input files:                                  ##############
############## from sapflux calc:                            ##############
############## Transpiration: El.L,El.L17,El.H,El.H17        ##############
############## stomatal conductance:gc.L, gc.L17, gc.H, gc.H17#############
############## tree info: datTreeL, datTreeL17, datTreeH,     #############
##############            datTreeH17                          #############
##############  from thaw depth: TDall                        #############
###########################################################################




#################################################################
####read in sapflow data                                  #######
#################################################################
source("c:\\Users\\hkropp\\Documents\\GitHub\\larix_density_ecohydro\\sapflux_process.r")
#libraries loaded from source
#plyr, lubridate,caTools

#set the plotting directory
plotDI <- "c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\manuscript_figures"
#################################################################
####read in thaw depth data                               #######
#################################################################

source("c:\\Users\\hkropp\\Documents\\GitHub\\larix_density_ecohydro\\thaw_depth_process.r")


#################################################################
####read in datafiles                                     #######
#################################################################

#read in precip data
datAirP <- read.csv("c:\\Users\\hkropp\\Google Drive\\viperSensor\\airport\\airport.csv")

#read in continuous soil data
datSW <- read.csv("c:\\Users\\hkropp\\Google Drive\\viperSensor\\soil\\vwc.GS3.csv")

#canopy rh and temperature
datRH <- read.csv("c:\\Users\\hkropp\\Google Drive\\viperSensor\\met\\RH.VP4.csv")
datTC <- read.csv("c:\\Users\\hkropp\\Google Drive\\viperSensor\\met\\TempC.VP4.csv")

#PAR
datPAR <- read.csv("c:\\Users\\hkropp\\Google Drive\\viperSensor\\met\\PAR.QSOS PAR.csv")

#read in leaf and sapwood area
datLSA <- read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\sapflux_diag\\tables\\treeSummary.csv")


#vertical root  data
datR<-read.csv("c:\\Users\\hkropp\\Google Drive\\root_analysis\\fine_root_out.csv")

#read in vertical root index and depth info
datD<-read.csv("c:\\Users\\hkropp\\Google Drive\\root_analysis\\siteDay\\Depth.csv")
#read in vertical root data for estimated depth increments
datE<-read.csv("c:\\Users\\hkropp\\Google Drive\\root_analysis\\siteDay\\rbio_SiteDay.csv")
datMD<-read.csv("c:\\Users\\hkropp\\Google Drive\\root_analysis\\siteDay\\medDepth.csv")








#####################################################################
####  figure 4. Vertical root profile                            ####
#####################################################################

#organize datE
datE$spt.id<-rep(seq(1,7),times=100)
datE$inc<-rep(seq(1,100),each=7)

#organize into a matrix for each type
R.mean<-matrix(rep(0,100*7), ncol=7)
R.low<-matrix(rep(0,100*7), ncol=7)
R.high<-matrix(rep(0,100*7), ncol=7)
for(z in 1:100){
	for(i in 1:7){
		R.mean[z,i]<-datE$r.mean[datE$inc==z&datE$spt.id==i]
		R.low[z,i]<-datE$pc2.5[datE$inc==z&datE$spt.id==i]
		R.high[z,i]<-datE$pc97.5[datE$inc==z&datE$spt.id==i]
		}
	
}


#run function on parameter values
D.seq<-matrix(rep(0,100*7), ncol=7)
D.seqUN<-matrix(rep(0,100*7), ncol=7)
for(i in 1:7){
	D.seq[,i]<-seq(.1,datD$Ave.deepest[i],length.out=100)/datD$Ave.deepest[i]
	D.seqUN[,i]<-seq(.1,datD$Ave.deepest[i],length.out=100)
}



#try an overlay plot where the root function 0-1 is plotted to the average active layer depth
#and the root biomass is represented with points
lw<-12
lh<-15

jpeg(paste0(plotDI,"\\vertical_root.jpg"), width=1600, height=1050, units="px",quality=100)
ab<-layout(matrix(seq(1,8), ncol=4, byrow=TRUE), width=c(lcm(lw),lcm(lw),lcm(lw),lcm(lw),lcm(lw),lcm(lw),lcm(lw),lcm(lw)),
				height=c(lcm(lh),lcm(lh),lcm(lh),lcm(lh),lcm(lh),lcm(lh),lcm(lh),lcm(lh)))
				
layout.show(ab)
#find the maximum measurement of roots
depmax<-aggregate(datR$depth.midpoint, by=list(datR$loc,datR$site), FUN="max")
#look at diff between max and ave
Ddiff<-depmax$x-datD$A.depth
#see what the highest point is to make plots around
Dhigh<-ifelse(depmax$x>=datD$A.depth,depmax$x,datD$A.depth)
#highest in high density
yuH<-100
yuL<-100
xH<-11
medSeq<-seq(0,11.5,length.out=50)
rootpoly <- rgb(80/255,80/255,80/255,.8)
meanlw <- 4.5
ptcx <- 3
icepoly <- rgb(200/255,200/255,200/255,.2)
lgcx <-3
txcx <- 5
mcx <- 3.5
ltcx <- 2.5
#make a plot of biomass for high density on period 1

layout.show(ab)
#start by doing all plots across the same depth range
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(0,xH),ylim=c(yuH,0), xlab=" ", ylab=" ", axes=FALSE,yaxs="i",xaxs="i")

polygon(c(0,0,xH,xH), c(yuH,datD$Ave.deepest[1],datD$Ave.deepest[1],yuH),border=NA,
				col=icepoly)
				
#abline(h=datD$Ave.deepest[1],lwd=5,col="black")				
polygon(c(medSeq,rev(medSeq)),
		c(rep(datMD$med2.5[1],length(medSeq)),	rep(datMD$med97.5[1],length(medSeq))),
			col="grey75",border=NA)
points(medSeq,rep(datMD$r.med[1],length(medSeq)), type="l", lty=3,lwd=3)
			
polygon(c(R.low[,1],rev(R.high[,1])), c(D.seqUN[,1], rev(D.seqUN[,1])), col=rootpoly,border=NA)


points(R.mean[,1],D.seqUN[,1], type="l", lwd=meanlw , col="black")

points(datR$bio.mg.cm3[datR$site=="h"&datR$period==1],datR$depth.midpoint[datR$site=="h"&datR$period==1], 
		col="black", pch=19, cex=ptcx)


		
box(which="plot")
text(5,90,"early July", cex=txcx)
text(10.25,3.5, "a",cex=txcx)
axis(2,seq(100,0, by=-10), lab=rep(" ",length(seq(100,0, by=-10))), las=2, cex.axis=2, lwd.ticks=3)
mtext(seq(100,0, by=-10), at=seq(100,0, by=-10), line=2, side=2, cex=ltcx, las=2)


polygon(c(.5,10.5,10.5,.5),c(35,35,85,85), border=NA, col="white")
text(5,40,"high density", cex=txcx)
legend(.5,40,c("observed biomass","mean biomass","95% mean CI","median rooting depth",
				"95% CI median depth", "average thaw depth"), pch=c(19,NA,15,NA,15,22),lty=c(NA,meanlw ,NA,3,NA,NA),
				lwd=c(NA,2,NA,2,NA,NA),col=c("black","black",rootpoly,
					"black","grey75",icepoly),pt.bg=c(NA,NA,NA,NA,NA,icepoly)
					,bty="n",cex=lgcx)
#Mid July
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(0,xH),ylim=c(yuH,0), xlab=" ", ylab=" ", axes=FALSE,yaxs="i",xaxs="i")
polygon(c(0,0,xH,xH), c(yuH,datD$Ave.deepest[2],datD$Ave.deepest[2],yuH),border=NA,
				col=icepoly)
#abline(h=datD$Ave.deepest[2],lwd=3,col=rgb(72/255,209/255,204/255))	
polygon(c(medSeq,rev(medSeq)),
		c(rep(datMD$med2.5[2],length(medSeq)),	rep(datMD$med97.5[2],length(medSeq))),
			col="grey75",border=NA)
points(medSeq,rep(datMD$r.med[2],length(medSeq)), type="l", lty=3,lwd=3)		
polygon(c(R.low[,2],rev(R.high[,2])), c(D.seqUN[,2], rev(D.seqUN[,2])), col=rootpoly, border=NA)
points(R.mean[,2],D.seqUN[,2], type="l", lwd=meanlw , col="black")


points(datR$bio.mg.cm3[datR$site=="h"&datR$period==2],datR$depth.midpoint[datR$site=="h"&datR$period==2], 
		col="black", pch=19, cex=ptcx)

box(which="plot")
text(5,90,"mid July", cex=txcx)
text(10.25,3.5, "b",cex=txcx)
#End July
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(0,xH),ylim=c(yuH,0), xlab=" ", ylab=" ", axes=FALSE,yaxs="i",xaxs="i")
polygon(c(0,0,xH,xH), c(yuH,datD$Ave.deepest[3],datD$Ave.deepest[3],yuH),border=NA,
				col=icepoly)
#abline(h=datD$Ave.deepest[3],lwd=3,col=rgb(72/255,209/255,204/255))	
polygon(c(medSeq,rev(medSeq)),
		c(rep(datMD$med2.5[3],length(medSeq)),	rep(datMD$med97.5[3],length(medSeq))),
			col="grey75",border=NA)
points(medSeq,rep(datMD$r.med[3],length(medSeq)), type="l", lty=3,lwd=3)		
polygon(c(R.low[,3],rev(R.high[,3])), c(D.seqUN[,3], rev(D.seqUN[,3])), col=rootpoly, border=NA)
points(R.mean[,3],D.seqUN[,3], type="l", lwd=meanlw , col="black")



points(datR$bio.mg.cm3[datR$site=="h"&datR$period==3],datR$depth.midpoint[datR$site=="h"&datR$period==3], 
		col="black", pch=19, cex=ptcx)

box(which="plot")
text(5,90,"end July", cex=txcx)
text(10.25,3.5, "c",cex=txcx)
#Mid August
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(0,xH),ylim=c(yuH,0), xlab=" ", ylab=" ", axes=FALSE,yaxs="i",xaxs="i")
polygon(c(0,0,xH,xH), c(yuH,datD$Ave.deepest[4],datD$Ave.deepest[4],yuH),border=NA,
				col=icepoly)
#abline(h=datD$Ave.deepest[4],lwd=3,col=rgb(72/255,209/255,204/255))					
polygon(c(medSeq,rev(medSeq)),
		c(rep(datMD$med2.5[4],length(medSeq)),	rep(datMD$med97.5[4],length(medSeq))),
			col="grey75",border=NA)
points(medSeq,rep(datMD$r.med[4],length(medSeq)), type="l", lty=3,lwd=3)		
polygon(c(R.low[,4],rev(R.high[,4])), c(D.seqUN[,4], rev(D.seqUN[,4])), col=rootpoly, border=NA)
points(R.mean[,4],D.seqUN[,4], type="l", lwd=meanlw , col="black")


points(datR$bio.mg.cm3[datR$site=="h"&datR$period==4],datR$depth.midpoint[datR$site=="h"&datR$period==4], 
		col="black", pch=19, cex=ptcx)

box(which="plot")
text(5,90,"mid August", cex=txcx)
text(10.25,3.5, "d",cex=txcx)
#####start low density
#start by doing all plots across the same depth range
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(0,xH),ylim=c(yuL,0), xlab=" ", ylab=" ", axes=FALSE,yaxs="i",xaxs="i")
polygon(c(0,0,xH,xH), c(yuL,datD$Ave.deepest[5],datD$Ave.deepest[5],yuL),border=NA,
				col=icepoly)
#abline(h=datD$Ave.deepest[5],lwd=3,col=rgb(72/255,209/255,204/255))	
polygon(c(medSeq,rev(medSeq)),
		c(rep(datMD$med2.5[5],length(medSeq)),	rep(datMD$med97.5[5],length(medSeq))),
			col="grey75",border=NA)
points(medSeq,rep(datMD$r.med[5],length(medSeq)), type="l", lty=3,lwd=3)		
polygon(c(R.low[,5],rev(R.high[,5])), c(D.seqUN[,5], rev(D.seqUN[,5])), col=rootpoly, border=NA)
points(R.mean[,5],D.seqUN[,5], type="l", lwd=meanlw , col="black")


points(datR$bio.mg.cm3[datR$site=="l"&datR$period==1],datR$depth.midpoint[datR$site=="l"&datR$period==1], 
		col="black", pch=19, cex=ptcx)
polygon(c(.5,10.5,10.5,.5),c(35,35,85,85), border=NA, col="white")		
text(5,40,"low density", cex=txcx)		
legend(.5,40,c("observed biomass","mean biomass","95% mean CI","median rooting depth",
				"95% CI median depth", "average thaw depth"), pch=c(19,NA,15,NA,15,22),lty=c(NA,meanlw ,NA,3,NA,NA),
				lwd=c(NA,2,NA,2,NA,NA),col=c("black","black",rootpoly,
					"black","grey75",icepoly),pt.bg=c(NA,NA,NA,NA,NA,icepoly),bty="n",cex=lgcx)
box(which="plot")
text(5,90,"early July", cex=txcx)
axis(2,seq(100,10, by=-10), lab=rep(" ",length(seq(100,10, by=-10))), las=2, cex.axis=2, lwd.ticks=3)
mtext(seq(100,10, by=-10), at=seq(100,10, by=-10), line=2, side=2, cex=ltcx, las=2)
mtext("Depth (cm)", outer=TRUE, side=2, line=-5, cex=mcx)
axis(1,seq(0,9, by=3), lab=rep(" ",length(seq(0,9, by=3))), las=2, cex.axis=2, lwd.ticks=3)
mtext(seq(0,9, by=3), at=seq(0,9, by=3), line=2.5, side=1, cex=ltcx)
text(10.25,3.5, "e",cex=txcx)
#empty plot mid july
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(0,xH),ylim=c(yuL,0), xlab=" ", ylab=" ", axes=FALSE,yaxs="i",xaxs="i")
text(5,90,"mid July", cex=txcx)
axis(1,seq(0,9, by=3), lab=rep(" ",length(seq(0,9, by=3))), las=2, cex.axis=2, lwd.ticks=3)
mtext(seq(0,9, by=3), at=seq(0,9, by=3), line=2.5, side=1, cex=ltcx)
box(which="plot")
text(10.25,3.5, "f",cex=txcx)	
#End of July
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(0,xH),ylim=c(yuL,0), xlab=" ", ylab=" ", axes=FALSE,yaxs="i",xaxs="i")
polygon(c(0,0,xH,xH), c(yuL,datD$Ave.deepest[6],datD$Ave.deepest[6],yuL),border=NA,
				col=icepoly)
#abline(h=datD$Ave.deepest[6],lwd=3,col=rgb(72/255,209/255,204/255))	
polygon(c(medSeq,rev(medSeq)),
		c(rep(datMD$med2.5[6],length(medSeq)),	rep(datMD$med97.5[6],length(medSeq))),
			col="grey75",border=NA)
points(medSeq,rep(datMD$r.med[6],length(medSeq)), type="l", lty=3,lwd=3)		
polygon(c(R.low[,6],rev(R.high[,6])), c(D.seqUN[,6], rev(D.seqUN[,6])), col=rootpoly, border=NA)
points(R.mean[,6],D.seqUN[,6], type="l", lwd=meanlw , col="black")


points(datR$bio.mg.cm3[datR$site=="l"&datR$period==3],datR$depth.midpoint[datR$site=="l"&datR$period==3], 
		col="black", pch=19, cex=ptcx)

box(which="plot")
text(5,90,"end July", cex=txcx)
axis(1,seq(0,9, by=3), lab=rep(" ",length(seq(0,9, by=3))), las=2, cex.axis=2, lwd.ticks=3)
mtext(seq(0,9, by=3), at=seq(0,9, by=3), line=2.5, side=1, cex=ltcx)
text(10.25,3.5, "g",cex=txcx)
#Mid August
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(0,xH),ylim=c(yuL,0), xlab=" ", ylab=" ", axes=FALSE,yaxs="i",xaxs="i")
polygon(c(0,0,xH,xH), c(yuL,datD$Ave.deepest[7],datD$Ave.deepest[7],yuL),border=NA,
				col=icepoly)
#abline(h=datD$Ave.deepest[7],lwd=3,col=rgb(72/255,209/255,204/255))	
polygon(c(medSeq,rev(medSeq)),
		c(rep(datMD$med2.5[7],length(medSeq)),	rep(datMD$med97.5[7],length(medSeq))),
			col="grey75",border=NA)
points(medSeq,rep(datMD$r.med[7],length(medSeq)), type="l", lty=3,lwd=3)		
polygon(c(R.low[,7],rev(R.high[,7])), c(D.seqUN[,7], rev(D.seqUN[,7])), col=rootpoly, border=NA)
points(R.mean[,7],D.seqUN[,7], type="l", lwd=meanlw , col="black")


points(datR$bio.mg.cm3[datR$site=="l"&datR$loc=="s"&datR$period==4],datR$depth.midpoint[datR$site=="l"&datR$loc=="s"&datR$period==4], 
		col="black", pch=19, cex=ptcx)

box(which="plot")
text(5,90,"mid August", cex=txcx)
axis(1,seq(0,9, by=3), lab=rep(" ",length(seq(0,9, by=3))), las=2, cex.axis=2, lwd.ticks=3)
mtext(seq(0,9, by=3), at=seq(0,9, by=3), line=2.5, side=1, cex=ltcx)
mtext(expression("Root Biomass (mg cm"^-3~")"), side=1, outer=TRUE, line=-1, cex=mcx)
text(10.25,3.5, "h",cex=txcx)
dev.off()
###############################End vertical root figure ########################################################################
################################################################################################################################
################################################################################################################################



#####################################################################
####  figure 5. Soil type root profile                           ####
#####################################################################

