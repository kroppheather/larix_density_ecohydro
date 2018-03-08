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
#### met and transpiration####
#read in precip data
datAirP <- read.csv("c:\\Users\\hkropp\\Google Drive\\viperSensor\\airport\\airport.csv")

#read in continuous soil data
datSW <- read.csv("c:\\Users\\hkropp\\Google Drive\\viperSensor\\soil\\vwc.GS3.csv")
datSW2 <- read.csv("c:\\Users\\hkropp\\Google Drive\\viperSensor\\soil\\vwc.5TM.csv")

datStemp2 <- read.csv("c:\\Users\\hkropp\\Google Drive\\viperSensor\\soil\\tempS.5TM.csv")

#canopy rh and temperature
datRH <- read.csv("c:\\Users\\hkropp\\Google Drive\\viperSensor\\met\\RH.VP4.csv")
datTC <- read.csv("c:\\Users\\hkropp\\Google Drive\\viperSensor\\met\\TempC.VP4.csv")

datAA <- join(datRH, datTC, by=c("doy","year","hour","sensorLoc","site"), type="inner")
datAA$e.sat <- 0.611*exp((17.502*datAA$TempC.VP4)/(datAA$TempC.VP4+240.97))
datAA$rh.fix <- ifelse(datAA$RH.VP4>=1,.999,datAA$RH.VP4)
datAA$D <- (datAA$e.sat-(datAA$rh.fix*datAA$e.sat))
#PAR
datPAR <- read.csv("c:\\Users\\hkropp\\Google Drive\\viperSensor\\met\\PAR.QSOS PAR.csv")

#read in leaf and sapwood area
datLSA <- read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\sapflux_diag\\tables\\treeSummary.csv")

#### soil and root density ####
#root density
datRD<-read.csv("c:\\Users\\hkropp\\Google Drive\\root_analysis\\density_fr.csv")

#read in soil profile data 
datSP<-read.csv("c:\\Users\\hkropp\\Google Drive\\root_analysis\\soil profile all.csv")



#### vertical roots ####
#vertical root  data
datR<-read.csv("c:\\Users\\hkropp\\Google Drive\\root_analysis\\fine_root_out.csv")

#read in vertical root index and depth info
datD<-read.csv("c:\\Users\\hkropp\\Google Drive\\root_analysis\\siteDay\\Depth.csv")
#read in vertical root data for estimated depth increments
datE<-read.csv("c:\\Users\\hkropp\\Google Drive\\root_analysis\\siteDay\\rbio_SiteDay.csv")
datMD<-read.csv("c:\\Users\\hkropp\\Google Drive\\root_analysis\\siteDay\\medDepth.csv")

#####################################################################
####  figure 2. Micromet figure                                  ####
#####################################################################

#aggregate data to daily
#air data
datTA <- aggregate(datAA$TempC.VP4, by=list(datAA$doy, datAA$year,datAA$site), FUN="mean")
colnames(datTA) <- c("doy","year","site","TempC.VP4")
datDA <- aggregate(datAA$D, by=list(datAA$doy, datAA$year,datAA$site), FUN="mean")
datTA$D <- datDA$x

#soil moisture data
datSoilW <- aggregate(datSW2$vwc.5TM, by=list(datSW2$doy,datSW2$year,datSW2$sensorZ,datSW2$site),FUN="mean") 
colnames(datSoilW) <- c("doy","year","depth","site","SW")

datSsh2 <- aggregate(datStemp2$tempS.5TM,
		by=list(datStemp2$doy,
				datStemp2$year,
				datStemp2$site,
				datStemp2$sensorZ),FUN="mean",na.action=na.omit)
colnames(datSsh2) <- c("doy","year","site","depthD","T.sD")


#182 20 cm hd
#160 10cm hd
# 161 18 cm ld
#148 8 cm ld

datSoilW$SW <- ifelse(datSoilW$doy<182&datSoilW$year==2017&datSoilW$site=="hd"&datSoilW$depth==20, NA,
			ifelse(datSoilW$doy<160&datSoilW$year==2017&datSoilW$site=="hd"&datSoilW$depth==10,NA,
			ifelse(datSoilW$doy<161&datSoilW$year==2017&datSoilW$site=="ld"&datSoilW$depth==18,NA,
			ifelse(datSoilW$doy<160&datSoilW$year==2017&datSoilW$site=="ld"&datSoilW$depth==8,NA,datSoilW$SW))))

lwl<-50
lhl<-25


ylr1 <- 0
yhr1 <- 30
ylr2 <- 0
yhr2 <- 3.5
ylr3 <- 0
yhr3 <- 80
xl16 <- 181
xh16 <- 245
xl17 <- 152
xh17 <- 226
#low
col1 <- rgb(51/255,51/255,51/255)

#high
col2 <- rgb(191/255,191/255,191/255)
precipc <- rgb(100/255,100/255,100/255)
lty1 <- 3
lty2 <- 3
lty3 <- 4
lty4 <- 4
prec.scale <- yhr1/40
SWmax <- 0.4
SW.scale <- yhr2/SWmax
TSmax <- 15
TS.scale <- yhr3/TSmax

lw <- 12
#subset precip
pr2016 <- datAirP[datAirP$year==2016&datAirP$doy>=xl16&datAirP$doy<xh16,]
pr2017 <- datAirP[datAirP$year==2017&datAirP$doy>=xl17&datAirP$doy<xh17,]


jpeg(paste0(plotDI,"\\micro_met.jpg"), width=3500, height=3000, units="px",quality=100)
ab<-layout(matrix(seq(1,6), ncol=2, byrow=TRUE), width=rep(lcm(lwl),6),
				height=rep(lcm(lhl),6))

#low air par 2016
	par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(xl16-1,xh16+1), ylim=c(ylr1,yhr1), xlab=" ", ylab=" ", xaxs="i",yaxs="i", axes=FALSE)
		
		for(i in 1:dim(pr2016)[1]){
		
		polygon(c(pr2016$doy[i]-.5,pr2016$doy[i]-.5,pr2016$doy[i]+.5,pr2016$doy[i]+.5),
				c(0, pr2016$Pr.mm[i]*prec.scale,pr2016$Pr.mm[i]*prec.scale,0), border=NA,col=precipc)
		
		}

		points(datTA$doy[datTA$site=="ld"&datTA$year==2016&datTA$doy>xl16&datTA$doy<xh16],
				datTA$TempC.VP4[datTA$site=="ld"&datTA$year==2016&datTA$doy>xl16&datTA$doy<xh16], type="l", 
				lwd=lw,col=col1)
			points(datTA$doy[datTA$site=="hd"&datTA$year==2016&datTA$doy>xl16&datTA$doy<xh16],
				datTA$TempC.VP4[datTA$site=="hd"&datTA$year==2016&datTA$doy>xl16&datTA$doy<xh16], type="l", lwd=lw, col=col2)

		
	box(which="plot")			
	
#low air par 2017
	par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(xl17-1,xh17+1), ylim=c(ylr1,yhr1), xlab=" ", ylab=" ", xaxs="i",yaxs="i", axes=FALSE)
			
		for(i in 1:dim(pr2017)[1]){
		
		polygon(c(pr2017$doy[i]-.5,pr2017$doy[i]-.5,pr2017$doy[i]+.5,pr2017$doy[i]+.5),
				c(0, pr2017$Pr.mm[i]*prec.scale,pr2017$Pr.mm[i]*prec.scale,0), border=NA,col=precipc)
		
		}
	
			
			points(datTA$doy[datTA$site=="ld"&datTA$year==2017&datTA$doy>xl17&datTA$doy<xh17],
				datTA$TempC.VP4[datTA$site=="ld"&datTA$year==2017&datTA$doy>xl17&datTA$doy<xh17], type="l", lwd=lw, col=col1)
			points(datTA$doy[datTA$site=="hd"&datTA$year==2017&datTA$doy>xl17&datTA$doy<xh17],
				datTA$TempC.VP4[datTA$site=="hd"&datTA$year==2017&datTA$doy>xl17&datTA$doy<xh17], type="l", lwd=lw,col=col2)
	box(which="plot")

#low air vpd 2016
	par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(xl16-1,xh16+1), ylim=c(ylr2,yhr2), xlab=" ", ylab=" ", xaxs="i",yaxs="i", axes=FALSE)
			points(datTA$doy[datTA$site=="ld"&datTA$year==2016&datTA$doy>xl16&datTA$doy<xh16],
				datTA$D[datTA$site=="ld"&datTA$year==2016&datTA$doy>xl16&datTA$doy<xh16], type="l", lwd=lw, col=col1)
			points(datTA$doy[datTA$site=="hd"&datTA$year==2016&datTA$doy>xl16&datTA$doy<xh16],
				datTA$D[datTA$site=="hd"&datTA$year==2016&datTA$doy>xl16&datTA$doy<xh16], type="l", lwd=lw, col=col2)
			points(datSoilW$doy[datSoilW$year==2016&datSoilW$site=="ld"&datSoilW$depth==8&datSoilW$doy>=xl16&datSoilW$doy<xh16],
				datSoilW$SW[datSoilW$year==2016&datSoilW$site=="ld"&datSoilW$depth==8&datSoilW$doy>=xl16&datSoilW$doy<xh16]*SW.scale
				,type="l",lwd=lw,col=col1,lty=lty1)
			points(datSoilW$doy[datSoilW$year==2016&datSoilW$site=="hd"&datSoilW$depth==10&datSoilW$doy>=xl16&datSoilW$doy<xh16],
				datSoilW$SW[datSoilW$year==2016&datSoilW$site=="hd"&datSoilW$depth==10&datSoilW$doy>=xl16&datSoilW$doy<xh16]*SW.scale
				,type="l",lwd=lw,col=col2,lty=lty2)
				
			points(datSoilW$doy[datSoilW$year==2016&datSoilW$site=="ld"&datSoilW$depth==18&datSoilW$doy>=xl16&datSoilW$doy<xh16],
				datSoilW$SW[datSoilW$year==2016&datSoilW$site=="ld"&datSoilW$depth==18&datSoilW$doy>=xl16&datSoilW$doy<xh16]*SW.scale
				,type="l",lwd=lw,col=col1,lty=lty3)
			points(datSoilW$doy[datSoilW$year==2016&datSoilW$site=="hd"&datSoilW$depth==20&datSoilW$doy>=xl16&datSoilW$doy<xh16],
				datSoilW$SW[datSoilW$year==2016&datSoilW$site=="hd"&datSoilW$depth==20&datSoilW$doy>=xl16&datSoilW$doy<xh16]*SW.scale
				,type="l",lwd=lw,col=col2,lty=lty4)	
				
	box(which="plot")	
	
#low air par 2017
	par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(xl17-1,xh17+1), ylim=c(ylr2,yhr2), xlab=" ", ylab=" ", xaxs="i",yaxs="i", axes=FALSE)
			points(datTA$doy[datTA$site=="ld"&datTA$year==2017&datTA$doy>xl17&datTA$doy<xh17],
				datTA$D[datTA$site=="ld"&datTA$year==2017&datTA$doy>xl17&datTA$doy<xh17], type="l", lwd=lw, col=col1)
			points(datTA$doy[datTA$site=="hd"&datTA$year==2017&datTA$doy>xl17&datTA$doy<xh17],
				datTA$D[datTA$site=="hd"&datTA$year==2017&datTA$doy>xl17&datTA$doy<xh17], type="l", lwd=lw, col=col2)
				
			points(datSoilW$doy[datSoilW$year==2017&datSoilW$site=="ld"&datSoilW$depth==8&datSoilW$doy>=xl17&datSoilW$doy<xh17],
				datSoilW$SW[datSoilW$year==2017&datSoilW$site=="ld"&datSoilW$depth==8&datSoilW$doy>=xl17&datSoilW$doy<xh17]*SW.scale
				,type="l",lwd=lw,col=col1,lty=lty1)
			points(datSoilW$doy[datSoilW$year==2017&datSoilW$site=="hd"&datSoilW$depth==10&datSoilW$doy>=xl17&datSoilW$doy<xh17],
				datSoilW$SW[datSoilW$year==2017&datSoilW$site=="hd"&datSoilW$depth==10&datSoilW$doy>=xl17&datSoilW$doy<xh17]*SW.scale
				,type="l",lwd=lw,col=col2,lty=lty2)	
			points(datSoilW$doy[datSoilW$year==2017&datSoilW$site=="ld"&datSoilW$depth==18&datSoilW$doy>=xl17&datSoilW$doy<xh17],
				datSoilW$SW[datSoilW$year==2017&datSoilW$site=="ld"&datSoilW$depth==18&datSoilW$doy>=xl17&datSoilW$doy<xh17]*SW.scale
				,type="l",lwd=lw,col=col1,lty=lty3)
			points(datSoilW$doy[datSoilW$year==2017&datSoilW$site=="hd"&datSoilW$depth==20&datSoilW$doy>=xl17&datSoilW$doy<xh17],
				datSoilW$SW[datSoilW$year==2017&datSoilW$site=="hd"&datSoilW$depth==20&datSoilW$doy>=xl17&datSoilW$doy<xh17]*SW.scale
				,type="l",lwd=lw,col=col2,lty=lty4)					
				
	box(which="plot")

#low TD 2016
	par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(xl16-1,xh16+1), ylim=c(ylr3,yhr3), xlab=" ", ylab=" ", xaxs="i",yaxs="i", axes=FALSE)
			points(TDall$doy[TDall$site=="ld"&TDall$year==2016],
				TDall$TDday[TDall$site=="ld"&TDall$year==2016], type="l", lwd=lw, col=col1)
			points(TDall$doy[TDall$site=="hd"&TDall$year==2016],
				TDall$TDday[TDall$site=="hd"&TDall$year==2016], type="l", lwd=lw, col=col2)
				
			points(datSsh2$doy[datSsh2$year==2016&datSsh2$site=="ld"&datSsh2$depth==8&datSsh2$doy>=xl16&datSsh2$doy<xh16],
				datSsh2$T.sD[datSsh2$year==2016&datSsh2$site=="ld"&datSsh2$depth==8&datSsh2$doy>=xl16&datSsh2$doy<xh16]*TS.scale
				,lwd=lw,col=col1,lty=lty1, type="l")
			points(datSsh2$doy[datSsh2$year==2016&datSsh2$site=="hd"&datSsh2$depth==10&datSsh2$doy>=xl16&datSsh2$doy<xh16],
				datSsh2$T.sD[datSsh2$year==2016&datSsh2$site=="hd"&datSsh2$depth==10&datSsh2$doy>=xl16&datSsh2$doy<xh16]*TS.scale
				,lwd=lw,col=col2,lty=lty2, type="l")		
				
	box(which="plot")
#low TD 2017
	par(mai=c(0,0,0,0))
		plot(c(0,1),c(0,1), type="n", xlim=c(xl17-1,xh17+1), ylim=c(ylr3,yhr3), xlab=" ", ylab=" ", xaxs="i",yaxs="i", axes=FALSE)
			points(TDall$doy[TDall$site=="ld"&TDall$year==2017],
				TDall$TDday[TDall$site=="ld"&TDall$year==2017], type="l", lwd=lw, col=col1)
			points(TDall$doy[TDall$site=="hd"&TDall$year==2017],
				TDall$TDday[TDall$site=="hd"&TDall$year==2017], type="l", lwd=lw, col=col2)
			points(datSsh2$doy[datSsh2$year==2017&datSsh2$site=="ld"&datSsh2$depth==8&datSsh2$doy>=xl17&datSsh2$doy<xh17],
				datSsh2$T.sD[datSsh2$year==2017&datSsh2$site=="ld"&datSsh2$depth==8&datSsh2$doy>=xl17&datSsh2$doy<xh17]*TS.scale
				,lwd=lw,col=col1,lty=lty1, type="l")
			points(datSsh2$doy[datSsh2$year==2017&datSsh2$site=="hd"&datSsh2$depth==10&datSsh2$doy>=xl17&datSsh2$doy<xh17],
				datSsh2$T.sD[datSsh2$year==2017&datSsh2$site=="hd"&datSsh2$depth==10&datSsh2$doy>=xl17&datSsh2$doy<xh17]*TS.scale
				,lwd=lw,col=col2,lty=lty2, type="l")	
	
	
	box(which="plot")	


dev.off()




###############################End micromet             ########################################################################
################################################################################################################################
################################################################################################################################


	
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


datRD$vol<-datRD$dim1*datRD$dim2*(datRD$depth.end-datRD$depth.start)
datRD$mid.pt<-datRD$depth.start+((datRD$depth.end-datRD$depth.start)/2)
#calculate densidy in mg/cm3
datRD$rdens<-(datRD$fine.root.biomass/datRD$vol)*1000

########################################
###look specifically  by layer type

#get the end of each layer
dEL<-aggregate(datRD$depth.end, by=list(datRD$soil.type,datRD$location,datRD$rep,datRD$stand),FUN="max")
dSL<-aggregate(datRD$depth.start, by=list(datRD$soil.type,datRD$location,datRD$rep,datRD$stand),FUN="min")
#get total root biomass in the layer
dRB<-aggregate(datRD$fine.root.biomass, by=list(datRD$soil.type,datRD$location,datRD$rep,datRD$stand),FUN="sum")
#total volume of the layer
dV<-aggregate(datRD$vol, by=list(datRD$soil.type,datRD$location,datRD$rep,datRD$stand),FUN="sum")
#add colnames for each table
colnames(dEL)<-c("soil.type","loc","rep","stand","dEnd")
colnames(dSL)<-c("soil.type","loc","rep","stand","dStart")
colnames(dRB)<-c("soil.type","loc","rep","stand","r.bio")
colnames(dV)<-c("soil.type","loc","rep","stand","vol")

#get density of root biomass
dRB$r.dens<-dRB$r.bio/dV$vol

#omit the missing measurement
dRB<-na.omit(dRB)
#convert from g/cm3 to mg/cm3
dRB$mgrD<-dRB$r.dens*1000


#now average root density across layers
layR<-aggregate(dRB$mgrD,by=list(dRB$soil.type,dRB$stand),FUN="mean")
layRSD<-aggregate(dRB$mgrD,by=list(dRB$soil.type,dRB$stand),FUN="sd")
layRL<-aggregate(dRB$mgrD,by=list(dRB$soil.type,dRB$stand),FUN="length")
colnames(layR)<-c("soil.type","stand","r.d")
colnames(layRSD)<-c("soil.type","stand","r.sd")
colnames(layRL)<-c("soil.type","stand","r.n")

layR$se<-layRSD$r.sd/sqrt(layRL$r.n)
layR$pseq<-c(5,1,3,6,2,4)

#make a layer ID to use for stats
layRID<-data.frame(soil.type=layR$soil.type,stand=layR$stand,pseq=layR$pseq)
dRBj<-join(dRB,layRID, by=c("soil.type","stand"), type="left")

#anova
modR<-lm(dRBj$r.dens~as.factor(dRBj$pseq))
#check residuals
hist(residuals(modR))
shapiro.test(residuals(modR))
#not normal and transformation didn't work (code deleted)
#do kruskal wallice test
modRkw<-kruskal.test(dRBj$r.dens~as.factor(dRBj$pseq))
#results are significant so do a pairwise test
#mann whitney tests
pair<-matrix(rep(NA,6*6), ncol=6)
a<-list()
for(j in 1:6){
	for(i in 1:6){
	if(i != j){
		a<-wilcox.test(dRBj$r.dens[dRBj$pseq==i|dRBj$pseq==j]~as.factor(dRBj$pseq[dRBj$pseq==i|dRBj$pseq==j]))
		pair[i,j]<-a$p.value
			}
	}
}
#flag the pairwise diff that are not sig
pairI<-ifelse(pair<.05,1,0)
#pairs 3x4,3x5,4x5,5x6 are not sig diff
#letters are as follows:
#1 A
#2 B
#3 C
#4 C
#5 CD
#6 D



#make a table
SigL<-data.frame(pseq=seq(1,6), sL=c("A","B","C","C", "CD", "D"))
#join to layR table
layR<-join(layR,SigL, by="pseq", type="left")

#calculate thickness since the data is actually in total depth
#from surface
datSP$bmT<-datSP$total.moss-datSP$green.moss.thickness
datSP$omT<-datSP$organic.depth-datSP$total.moss


datG<-na.omit(data.frame(site=datSP$site,thick=datSP$green.moss.thickness))
datB<-na.omit(data.frame(site=datSP$site,thick=datSP$bmT))
datO<-na.omit(data.frame(site=datSP$site,thick=datSP$omT))

#now get stats on each layer
Green<-aggregate(datG$thick, by=list(datG$site),FUN="mean")
colnames(Green)<-c("stand","thick")
GreenSD<-aggregate(datG$thick, by=list(datG$site),FUN="sd")
colnames(GreenSD)<-c("stand","thick.sd")
GreenL<-aggregate(datG$thick, by=list(datG$site),FUN="length")
colnames(GreenL)<-c("stand","thick.n")
Green$se<-GreenSD$thick.sd/sqrt(GreenL$thick.n)

Brown<-aggregate(datB$thick, by=list(datB$site),FUN="mean")
colnames(Brown)<-c("stand","thick")
BrownSD<-aggregate(datB$thick, by=list(datB$site),FUN="sd")
colnames(BrownSD)<-c("stand","thick.sd")
BrownL<-aggregate(datB$thick, by=list(datB$site),FUN="length")
colnames(BrownL)<-c("stand","thick.n")
Brown$se<-BrownSD$thick.sd/sqrt(BrownL$thick.n)

Organic<-aggregate(datO$thick, by=list(datO$site),FUN="mean")
colnames(Organic)<-c("stand","thick")
OrganicSD<-aggregate(datO$thick, by=list(datO$site),FUN="sd")
colnames(OrganicSD)<-c("stand","thick.sd")
OrganicL<-aggregate(datO$thick, by=list(datO$site),FUN="length")
colnames(OrganicL)<-c("stand","thick.n")
Organic$se<-OrganicSD$thick.sd/sqrt(OrganicL$thick.n)



###########################################################
#make a barplot 
#of density in each layer
#add a plotting order to layR
lgcx <-2.75
txcx <- 2.75
mcx <- 4
ltcx <- 3
#order is moss, organic, mineral
#start with hd in each layer
lcol <- "grey50"
hcol <- "grey75"
layR$pcol<-rep(c(hcol,lcol), each=3)
mgcol <- "grey85"
mbcol <- "grey50"
ocol <- "grey30"

lda <- 3
#setup plot layout
wd<-18
hd<-18
jpeg(paste0(plotDI,"\\root_type.jpg"), width=1600, height=1050, units="px",quality=100)
ac<-layout(matrix(seq(1,2),ncol=2), width=rep(lcm(wd),2),height=rep(lcm(hd),2))
layout.show(ac)

par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n",ylim=c(6.1,-.1),xlim=c(-.1,8), axes=FALSE, xlab=" ", ylab=" ", yaxs="i",
		xaxs="i")
	for(i in 1:dim(layR)[1]){
	polygon(c(0,layR$r.d[i],layR$r.d[i],0),c(layR$pseq[i]-1,layR$pseq[i]-1,layR$pseq[i],layR$pseq[i]),
			col=layR$pcol[i])	
	}
	arrows(layR$r.d-layR$se,layR$pseq-.5,layR$r.d+layR$se,layR$pseq-.5,code=0,lwd=lda )
	
mtext(c("moss", "organic", "mineral", "< 20 cm"), at =c(1,3,5,5.5),side=2,line=2.5, las=2, cex=ltcx)
axis(2, c(1,3,5), c(" ", " ", " "), las=2, lwd.ticks=3)
axis(1, seq(0,7, by=1),rep(" ", length(seq(0,7, by=1))), lwd.ticks=3)
mtext(seq(0,7, by=1), at=seq(0,7, by=1), line=2.5,side=1,cex=ltcx)


box(which="plot")
legend(3,0,c("high density","low density"), fill=c(hcol,lcol),bty="n", cex=lgcx)
mtext("Soil layer type", side=2, cex=mcx, line=13)
mtext(expression(paste("Root biomass mg cm"^"-3")), side=1, cex=mcx, line=8)
text(layR$r.d+layR$se+.5,layR$pseq-.5,layR$sL,cex=txcx)
#plot thickness
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n",ylim=c(20,0),xlim=c(0,2), axes=FALSE, xlab=" ", ylab=" ", yaxs="i",
		xaxs="i")
	for(i in 1:2){
		polygon(c(i-1,i-1,i,i),c(0,Green$thick[i],Green$thick[i],0), col=mgcol)
	}

	for(i in 1:2){
		polygon(c(i-1,i-1,i,i),
		c(Green$thick[i],Green$thick[i]+Brown$thick[i],Green$thick[i]+Brown$thick[i],Green$thick[i]), 
		col=mbcol)
	}

		for(i in 1:2){
		polygon(c(i-1,i-1,i,i),
		c(Green$thick[i]+Brown$thick[i],Green$thick[i]+Brown$thick[i]+Organic$thick[i],
		Green$thick[i]+Brown$thick[i]+Organic$thick[i],Green$thick[i]+Brown$thick[i]), 
		col=ocol)
	}

	
	
		arrows(c(.5,1.5), Green$thick-Green$se,c(.5,1.5), Green$thick+Green$se, lwd=lda , code=0)
		
			arrows(c(.5,1.5), Green$thick+Brown$thick-Brown$se,c(.5,1.5), 
					Green$thick+Brown$thick+Brown$se, lwd=lda , code=0)
		arrows(c(.5,1.5), Green$thick+Brown$thick+Organic$thick-Organic$se,c(.5,1.5), 
					Green$thick+Brown$thick+Organic$thick+Organic$se, lwd=lda , code=0)
	
	axis(4,seq(20,0, by=-2),rep(" ", length(seq(20,0, by=-2))), lwd.ticks=3)
	mtext(seq(20,0, by=-2), at=seq(20,0, by=-2), line=2.5,side=4,las=2,cex=ltcx)
	
	box(which="plot")	
	axis(1, c(.5,1.5), c(" ", " "), lwd.ticks=3)
	mtext( c("high density", "low density"), at=c(.5,1.5), line=2.5,side=1,cex=ltcx)
	
mtext("Soil layer depth (cm)", side=4, cex=mcx, line=10)
mtext("Stand", side=1, cex=mcx, line=8)

legend(.92,13,c("green moss", "brown moss", "fibric organic"), fill=c(mgcol, mbcol,ocol), bty="n", cex=lgcx)
dev.off()


###############################End soil root figure    ########################################################################
################################################################################################################################
################################################################################################################################