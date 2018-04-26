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
datStemp <- read.csv("c:\\Users\\hkropp\\Google Drive\\viperSensor\\soil\\tempS.GS3.csv")
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

#read in data for model goodness of fit
datGC <- read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\gc_model\\run42\\out\\gcdata.csv")
datMG <- read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\gc_model\\run42\\out\\mod_stats.csv",row.names=1)
datMGQ <- read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\gc_model\\run42\\out\\mod_quants.csv",row.names=1)
#read in root model goodness of fit
datRootR <- read.csv("c:\\Users\\hkropp\\Google Drive\\root_analysis\\siteDay\\modrep.csv")
datRootD <-read.csv("c:\\Users\\hkropp\\Google Drive\\root_analysis\\siteDay\\root_bioDat.csv")

dexps <- "\\[*[[:digit:]]*\\]"
datMG$parms <- gsub(dexps,"", rownames(datMG))
dexps2 <- "\\[*[[:digit:]]*\\,"
datMG$parms2 <- gsub(dexps2,"", datMG$parms)

datC <- cbind(datMG,datMGQ)



##### read in sapwood thickness or sensor correciton
datSWAl <- read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\individual_data\\sap_thick.csv")
#density allometry
datAllom <- read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\individual_data\\larix_allom.csv")
#stand day
datStandD<-read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\gc_model\\run42\\out\\standDay.csv")
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

datTs50 <- datStemp[datStemp$sensorZ==50,]
datTs50d <- aggregate(datTs50$tempS.GS3,by=list(datTs50$doy,datTs50$year,datTs50$site), FUN="mean",na.action=na.omit)
colnames(datTs50d) <- c("doy","year","site","Ts50")
#182 20 cm hd
#160 10cm hd
# 161 18 cm ld
#148 8 cm ld

datSoilW$SW <- ifelse(datSoilW$doy<182&datSoilW$year==2017&datSoilW$site=="hd"&datSoilW$depth==20, NA,
			ifelse(datSoilW$doy<160&datSoilW$year==2017&datSoilW$site=="hd"&datSoilW$depth==10,NA,
			ifelse(datSoilW$doy<161&datSoilW$year==2017&datSoilW$site=="ld"&datSoilW$depth==18,NA,
			ifelse(datSoilW$doy<160&datSoilW$year==2017&datSoilW$site=="ld"&datSoilW$depth==8,NA,datSoilW$SW))))

lwl<-50
lhl<-35

#increments of 5
ylr1 <- 0
yhr1 <- 30
#increments .5
ylr2 <- 0
yhr2 <- 3.5
#increments of 10
ylr3 <- 0
yhr3 <- 83
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
#increments 0.05
SWmax <- 0.5
SW.scale <- yhr2/SWmax
#increments of 3
TSmax <- 15
TS.scale <- yhr3/TSmax

mx <- 6
lx <- 8
lgx <- 10
tx <- 14

tseq <- seq(0,yhr1, by=5)
prseqL <- seq(0,40, by=10)
prseqA <- prseqL*prec.scale
Dseq <- seq(0,yhr2-0.5, by=.5)
SWseqL <- seq(0,.45,by=.05)
SWseqA <- SWseqL*SW.scale
TDseq <- seq(0,yhr3-13,by=10)
TSseqL <- seq(0,TSmax-3,by=3)
TSseqA <- TS.scale*TSseqL
tw <- 8
lw <- 12

x16seq <- seq(185,245, by=10)
x17seq <- seq(160,220, by=10)
#subset precip
pr2016 <- datAirP[datAirP$year==2016&datAirP$doy>=xl16&datAirP$doy<xh16,]
pr2017 <- datAirP[datAirP$year==2017&datAirP$doy>=xl17&datAirP$doy<xh17,]



jpeg(paste0(plotDI,"\\micro_met.jpg"), width=3700, height=3400, units="px",quality=100)
ab<-layout(matrix(seq(1,6), ncol=2, byrow=TRUE), width=rep(lcm(lwl),6),
				height=rep(lcm(lhl),6))

#Tair P 2016
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
		
		axis(2, tseq, rep(" ",length(tseq)), lwd.ticks=tw)
		mtext(tseq, at=tseq, cex=mx, line=5, side=2, las=2)
			
	mtext("Air temperature", 	cex=lx, line=32,side=2)
	mtext(expression(paste("(",italic(T[a]),",",degree,"C )")), 	cex=lx, line=18,side=2)
	text(242,28,"(a)",cex=tx)
	box(which="plot")			
	
#Tair P 2017
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
		axis(4, prseqA, rep(" ",length(prseqA)), lwd.ticks=tw)
		mtext(prseqL, at=prseqA, cex=mx, line=5, side=4, las=2)
		
	legend(151,31, c(expression(paste("low", italic(T[a]))),expression(paste("high", italic(T[a]))),
						expression(paste(italic(P)))),
						lty=c(1,1,NA),lwd=c(lw,lw,NA), pch=c(NA,NA,15), col=c(col1,col2,precipc),bty="n",cex=lgx)
	mtext("Precipitation", 	cex=lx, line=28,side=4)
	mtext(expression(paste("(",italic(P),", mm )")), 	cex=lx, line=42,side=4)
	text(222.5,28,"(b)",cex=tx)	
#M D 2016
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
		axis(2, Dseq, rep(" ",length(Dseq)), lwd.ticks=tw)
		mtext(Dseq, at=Dseq, cex=mx, line=5, side=2, las=2)	

	mtext("Vapor pressure", 	cex=lx, line=32,side=2)
	mtext( expression(paste("deficit(",italic(D),", kPa )")), 	cex=lx, line=18,side=2)		
	box(which="plot")	
	text(242,3.3,"(c)",cex=tx)
#M D 2017
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
		axis(4, SWseqA, rep(" ",length(SWseqA)), lwd.ticks=tw)
		mtext(SWseqL, at=SWseqA, cex=mx, line=5, side=4, las=2)			
	box(which="plot")
	mtext("Soil moisture", 	cex=lx, line=28,side=4)
	mtext(expression(paste("(",italic(M),", m"^"3"~"m"^"-3"~")")), 	cex=lx, line=42,side=4)
	legend(149,3.7, c(expression(paste("low", italic(D))),expression(paste("low 8cm ", italic(M))),
						expression(paste("low 18cm ", italic(M)))),
						lty=c(1,lty1,lty3),lwd=c(lw,lw,lw),  col=c(col1,col1,col1),bty="n",cex=lgx)
	
	legend(184,3.7, c(expression(paste("high", italic(D))),expression(paste("high 10cm ", italic(M))),
						expression(paste("high 20cm ", italic(M)))),
						lty=c(1,lty2,lty4),lwd=c(lw,lw,lw),  col=c(col2,col2,col2),bty="n",cex=lgx)	
	text(222.5,3.3,"(d)",cex=tx)	
#TD TS 2016
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
			
		axis(2, TDseq, rep(" ",length(TDseq)), lwd.ticks=tw)
		mtext(TDseq, at=TDseq, cex=mx, line=5, side=2, las=2)	
	mtext("Permafrost thaw", 	cex=lx, line=32,side=2)
	mtext(expression(paste("depth (",italic(TD),", cm )")), 	cex=lx, line=18,side=2)	
		axis(1, x16seq, rep(" ",length(x16seq)), lwd.ticks=tw)
		mtext(x16seq, at=x16seq, cex=mx, line=7, side=1)	
				points(datTs50d$doy[datTs50d$year==2016&datTs50d$site=="ld"],datTs50d$Ts50[datTs50d$year==2016&datTs50d$site=="ld"]*TS.scale,
				lwd=lw,col=col1,lty=lty3,type="l")
			points(datTs50d$doy[datTs50d$year==2016&datTs50d$site=="hd"],datTs50d$Ts50[datTs50d$year==2016&datTs50d$site=="hd"]*TS.scale,
				lwd=lw,col=col2,lty=lty4,type="l")			
	box(which="plot")
	text(242,78,"(e)",cex=tx)
#TD TS 2017
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
			points(datTs50d$doy[datTs50d$year==2017&datTs50d$site=="ld"],datTs50d$Ts50[datTs50d$year==2017&datTs50d$site=="ld"]*TS.scale,
				lwd=lw,col=col1,lty=lty3,type="l")
			points(datTs50d$doy[datTs50d$year==2017&datTs50d$site=="hd"],datTs50d$Ts50[datTs50d$year==2017&datTs50d$site=="hd"]*TS.scale,
				lwd=lw,col=col2,lty=lty4,type="l")		
		axis(4, TSseqA, rep(" ",length(TSseqA)), lwd.ticks=tw)
		mtext(TSseqL, at=TSseqA, cex=mx, line=5, side=4, las=2)
		mtext("Soil temperature", 	cex=lx, line=28,side=4)
		mtext(expression(paste("( ",italic(T[s]),",", degree,"C )")), 	cex=lx, line=42,side=4)
		axis(1, x17seq, rep(" ",length(x17seq)), lwd.ticks=tw)
		mtext(x17seq, at=x17seq, cex=mx, line=7, side=1)	
	box(which="plot")	
	mtext("Day of year", cex=lx, side=1,outer=TRUE,line=-7)
	legend(151,84, c(expression(paste("low", italic(TD))),expression(paste("high", italic(TD))),
						expression(paste("organic low", italic(T[s]))),expression(paste("organic high", italic(T[s])))
						),
						lty=c(1,1,lty1,lty2),lwd=c(lw,lw,lw,lw), pch=c(NA,NA,NA,NA),
						col=c(col1,col2,col1,col2),bty="n",cex=lgx)
						
	legend(179,84,	c(expression(paste("50cm low", italic(T[s]))),expression(paste("50cm high", italic(T[s])))),
			lty=c(lty3,lty4),lwd=c(lw,lw),pch=c(NA,NA),col=c(col1,col2),bty="n",cex=lgx)
	text(222.5,78,"(f)",cex=tx)					
dev.off()




###############################End micromet             ########################################################################
################################################################################################################################
################################################################################################################################

#####################################################################
####  figure 3. Transpiration/gc figure                          ####
#####################################################################


########################################################
#####half houly data ###################################
########################################################


#start by aggregating across trees for transpiration and gc
#low16
ThhL <- data.frame(El.L[,1:3],T=rowMeans(El.L[,4:16], na.rm=TRUE), T.sd=apply(El.L[,4:16],1,sd,na.rm=TRUE),
					T.n=apply(El.L[,4:16],1,function(x) length(which(!is.na(x)))))

#low17
ThhL17 <- data.frame(El.L17[,1:3],T=rowMeans(El.L17[,4:16], na.rm=TRUE), T.sd=apply(El.L17[,4:16],1,sd,na.rm=TRUE),
					T.n=apply(El.L17[,4:16],1,function(x) length(which(!is.na(x)))))

#high16
ThhH <- data.frame(El.H[,1:3],T=rowMeans(El.H[,4:10], na.rm=TRUE), T.sd=apply(El.H[,4:10],1,sd,na.rm=TRUE),
					T.n=apply(El.H[,4:10],1,function(x) length(which(!is.na(x)))))

#high17
ThhH17 <- data.frame(El.H17[,1:3],T=rowMeans(El.H17[,4:16], na.rm=TRUE), T.sd=apply(El.H17[,4:16],1,sd,na.rm=TRUE),
					T.n=apply(El.H17[,4:16],1,function(x) length(which(!is.na(x)))))
					
#gc 
ghhL <- data.frame(gc.L[,1:3],gc=rowMeans(gc.L[,4:16], na.rm=TRUE), gc.sd=apply(gc.L[,4:16],1,sd,na.rm=TRUE),
					gc.n=apply(gc.L[,4:16],1,function(x) length(which(!is.na(x)))))

#low17
ghhL17 <- data.frame(gc.L17[,1:3],gc=rowMeans(gc.L17[,4:16], na.rm=TRUE), gc.sd=apply(gc.L17[,4:16],1,sd,na.rm=TRUE),
					gc.n=apply(gc.L17[,4:16],1,function(x) length(which(!is.na(x)))))

#high16
ghhH <- data.frame(gc.H[,1:3],gc=rowMeans(gc.H[,4:10], na.rm=TRUE), gc.sd=apply(gc.H[,4:10],1,sd,na.rm=TRUE),
					gc.n=apply(gc.H[,4:10],1,function(x) length(which(!is.na(x)))))

#high17
ghhH17 <- data.frame(gc.H17[,1:3],gc=rowMeans(gc.H17[,4:16], na.rm=TRUE), gc.sd=apply(gc.H17[,4:16],1,sd,na.rm=TRUE),
					gc.n=apply(gc.H17[,4:16],1,function(x) length(which(!is.na(x)))))				

#join met data to match for VPD, PAR, and Precip
#first join par to datAA
datPAA <- join(datPAR,datAA, by=c("doy","year","hour","site"),type="right")
#join precip
datPPAA <- join(datPAA, datAirP, by=c("doy","year"), type="left")
datPAAH <- datPPAA[datPPAA$site=="hd",]					
datPAAL <- datPPAA[datPPAA$site=="ld",]					
#turn to T and gc dataframes into a list
TList <- list(ThhL,ThhH,ThhL17,ThhH17)
gcList <- list(ghhL,ghhH, ghhL17,ghhH17)
PAList <- list(datPAAL,datPAAH,datPAAL,datPAAH)
TList2 <- list()
gcList2 <- list()
for(i in 1:4){
	TList2[[i]] <- join(TList[[i]], PAList[[i]], by=c("doy","year","hour"), type="left")
	gcList2[[i]] <- join(gcList[[i]], PAList[[i]], by=c("doy","year","hour"), type="left")
}					


for(i in 1:4){
	#exclude data that is not reliable due to precipitation
	TList2[[i]]$T <- ifelse(TList2[[i]]$Pr.mm>1,NA,TList2[[i]]$T)
	gcList2[[i]]$gc <- ifelse(gcList2[[i]]$Pr.mm>1,NA,gcList2[[i]]$gc)
	#exclude PAR < 5umol for gc
	gcList2[[i]]$gc <- ifelse(gcList2[[i]]$PAR.QSOS.Par<5,NA,gcList2[[i]]$gc)
	#VPD less than 0.6
	gcList2[[i]]$gc <- ifelse(gcList2[[i]]$D<0.6,NA,gcList2[[i]]$gc)
	
}				
#calculate standard error
for(i in 1:4){
	TList2[[i]]$T.se <-  TList2[[i]]$T.sd/sqrt(TList2[[i]]$T.n)
	gcList2[[i]]$gc.se <-  gcList2[[i]]$gc.sd/sqrt(gcList2[[i]]$gc.n)
	#set se to zero if T or gc are NA 
	TList2[[i]]$T.se<- ifelse(is.na(TList2[[i]]$T), 0,TList2[[i]]$T.se)
	gcList2[[i]]$gc.se<- ifelse(is.na(gcList2[[i]]$gc), 0,gcList2[[i]]$gc.se)
	
}	
#################################################################
####calculate daily transpiration                         #######
#################################################################


#El is in g m-2 s-1

#convert to g m-2 half hour-1
#and reorganize
E.temp <- list(El.L,El.H,El.L17,El.H17)
E.dim <- numeric(0)
E.temp2 <- list()
E.temp3 <- list()
E.tempwork <- list()
for(i in 1:4){
	E.dim[i] <- dim(E.temp[[i]])[2]
	E.temp2[[i]] <- data.frame(E.temp[[i]][,1:3], E.temp[[i]][,4:E.dim[i]]*60*30)
	E.temp3[[i]] <- data.frame(doy=rep(E.temp2[[i]]$doy,times=E.dim[i]-3),
								year=rep(E.temp2[[i]]$year,times=E.dim[i]-3),
								hour=rep(E.temp2[[i]]$hour,times=E.dim[i]-3),
								E.hh = as.vector(data.matrix(E.temp2[[i]][,4:E.dim[i]])),
								tree = rep(seq(1,E.dim[i]-3), each=dim(E.temp2[[i]])[1]))
	E.temp3[[i]] <- na.omit(E.temp3[[i]])
	E.tempwork[[i]] <- E.temp3[[i]]
	E.tempwork[[i]]$E.ss <- E.temp3[[i]]$E.hh/(30*60)
	E.tempwork[[i]]$dataset <- rep(i,dim(E.tempwork[[i]])[1])
}
Esshh <- ldply(E.tempwork,data.frame)
#convert to mols
Esshh$E.mmols <- (Esshh$E.ss/18)*1000

#now aggregate to see how many observations in a day
#and pull out data on days that have at least 3 trees
#and those trees have all 48 measurements in a day
ELength <- list()
EdayL <- list()
E.temp4 <- list()
for(i in 1:4){
	ELength[[i]] <- aggregate(E.temp3[[i]]$E.hh, by=list(E.temp3[[i]]$doy,E.temp3[[i]]$year,E.temp3[[i]]$tree),
								FUN="length")
	ELength[[i]] <- ELength[[i]][ELength[[i]]$x==48,]
	colnames(ELength[[i]]) <- c("doy","year","tree","count")
	#find out how many tree observations on each day
	EdayL[[i]] <- aggregate(ELength[[i]]$tree, by=list(ELength[[i]]$doy,ELength[[i]]$year), FUN="length")
	colnames(EdayL[[i]])<- c("doy","year", "ntree")
	#subset to only use days with at least 3 trees
	EdayL[[i]] <- EdayL[[i]][EdayL[[i]]$ntree>=3,]
	#join to only include days with enough sensors
	ELength[[i]] <- join(ELength[[i]],EdayL[[i]], by=c("doy","year"), type="inner")
	#create a tree, day id
	ELength[[i]]$treeDay <- seq(1, dim(ELength[[i]])[1])
	ELength[[i]]$dataset <- rep(i, dim(ELength[[i]])[1])
	#ELength now has the list of each sensor and day that should be included
	#subset the data to only do the calculations on the trees that meet the minimum criteria
	E.temp4[[i]] <- join(E.temp3[[i]],ELength[[i]], by=c("doy", "year", "tree"), type="inner")
}
#turn back into a dataframe
EtempALL <- ldply(E.temp4,data.frame)
EInfo <- ldply(ELength,data.frame)


#get the daily integration of the transpiration
EdayT <- numeric(0)
EdayTemp <- list()
for(i in 1:dim(EInfo)[1]){
	EdayTemp[[i]] <- data.frame(x=EtempALL$hour[EtempALL$treeDay==EInfo$treeDay[i]&EtempALL$dataset==EInfo$dataset[i]],
								y=EtempALL$E.hh[EtempALL$treeDay==EInfo$treeDay[i]&EtempALL$dataset==EInfo$dataset[i]])
	EdayT[i] <- trapz(EdayTemp[[i]]$x,EdayTemp[[i]]$y)

}
#add daily value into Einfo
EInfo$T.day <- EdayT
#in g per day now
EInfo$T.Lday <- EdayT/1000

#add stand labels to the datasets 
EInfo$stand <- ifelse(EInfo$dataset==1|EInfo$dataset==3,"ld","hd")

#get the stand averages of daily transpiration across day

EdayLm <- aggregate(EInfo$T.Lday, by=list(EInfo$doy,EInfo$year,EInfo$stand), FUN="mean")
EdayLsd <- aggregate(EInfo$T.Lday, by=list(EInfo$doy,EInfo$year,EInfo$stand), FUN="sd")
EdayLl <- aggregate(EInfo$T.Lday, by=list(EInfo$doy,EInfo$year,EInfo$stand), FUN="length")

Eday <- EdayLm
colnames(Eday) <- c("doy","year","site","T.L.day")
Eday$T.sd <- EdayLsd$x
Eday$T.n <- EdayLl$x

Eday$T.se <- Eday$T.sd/sqrt(Eday$T.n)


#compare Eday where there are the most days together in 2016
Edaysub1 <- Eday[Eday$doy<=225&Eday$year==2016,]
aggregate(Edaysub1$T.L.day, by=list(Edaysub1$site),FUN="mean")
aggregate(Edaysub1$T.L.day, by=list(Edaysub1$site),FUN="min")
aggregate(Edaysub1$T.L.day, by=list(Edaysub1$site),FUN="max")

aggregate(Edaysub1$T.L.day, by=list(Edaysub1$site),FUN="sd")$x/aggregate(Edaysub1$T.L.day, by=list(Edaysub1$site),FUN="length")$x
Edaysub2 <- Eday[Eday$doy<=175&Eday$year==2017,]
aggregate(Edaysub2$T.L.day, by=list(Edaysub2$site),FUN="mean")
aggregate(Edaysub2$T.L.day, by=list(Edaysub2$site),FUN="min")
aggregate(Edaysub2$T.L.day, by=list(Edaysub2$site),FUN="max")
aggregate(Edaysub2$T.L.day, by=list(Edaysub2$site),FUN="sd")$x/aggregate(Edaysub2$T.L.day, by=list(Edaysub2$site),FUN="length")$x

#################################################################
####calculate daily gc                                    #######
#################################################################


#reoranize into data frames
gctemp <- list(gc.L, gc.H, gc.L17, gc.H17)

gcdim <- numeric(0)
gctemp2 <- list()

for(i in 1:4){
	gcdim[i] <- dim(gctemp[[i]])[2]
	gctemp2[[i]] <- data.frame(doy=rep(gctemp[[i]]$doy,times=gcdim[i]-3),
								year=rep(gctemp[[i]]$year,times=gcdim[i]-3),
								hour=rep(gctemp[[i]]$hour,times=gcdim[i]-3),
								gc.h = as.vector(data.matrix(gctemp[[i]][,4:gcdim[i]])),
								tree = rep(seq(1,gcdim[i]-3), each=dim(gctemp[[i]])[1]),
								datset=rep(i, each=dim(gctemp[[i]])[1]) )
	gctemp2[[i]] <- na.omit(gctemp2[[i]])
}
gctemp3 <- ldply(gctemp2, data.frame)
gctemp3$site<- ifelse(gctemp3$datset==1|gctemp3$datset==3,"ld","hd")
#join met, PAR, and Precip to only average relevant values
MetJ <- join(datAA, datAirP, by=c("doy","year"), type="left")
MetJ <- join(MetJ, datPAR, by=c("doy","year","hour","site"),type="left")


MetJ$flag <- ifelse(MetJ$PAR.QSOS.Par<=5|MetJ$D<=0.6|MetJ$Pr.mm>1,1,0)

FlagJ <- data.frame(MetJ[,1:3],site=MetJ$site,flag=MetJ$flag)

gctemp3 <- join(gctemp3,FlagJ,by=c("doy","year","hour","site"),type="left" )
gctemp3$gc.h <- ifelse(gctemp3$flag==1,NA,gctemp3$gc.h)
#get the average daily gc across all trees


	gsDave <- aggregate(gctemp3$gc.h, by=list(gctemp3$doy,gctemp3$year,gctemp3$datset), FUN="mean",na.rm=TRUE, na.action=na.omit)
	gsDsd <- aggregate(gctemp3$gc.h, by=list(gctemp3$doy,gctemp3$year,gctemp3$datset), FUN="sd",na.rm=TRUE)
	gsDn <- aggregate(gctemp3$gc.h, by=list(gctemp3$doy,gctemp3$year,gctemp3$datset), function(x) length(which(!is.na(x))))
	
	colnames(gsDave) <- c("doy", "year", "dataset","gc.mmol.s")
	gsDave$stand <- ifelse(gsDave$dataset==1|gsDave$dataset==3,"ld","hd")
	
	gsDave$se <- gsDsd$x/sqrt(gsDn$x)
gsDave$gc.mmol.s <- ifelse(gsDave$doy==181&gsDave$year==2016,NA,gsDave$gc.mmol.s)
gsDave <- na.omit(gsDave)
	
#get the maximum daily par and D across each stand and day
maxD <- aggregate(datAA$D, by=list(datAA$doy, datAA$year,datAA$site),FUN="max")
colnames(maxD) <- c("doy","year","site","maxD")

gsSub1 <- gsDave[gsDave$doy<=225&gsDave$year==2016,]
aggregate(gsSub1$gc.mmol.s,by=list(gsSub1$stand),FUN="mean")
aggregate(gsSub1$gc.mmol.s,by=list(gsSub1$stand),FUN="sd")$x/aggregate(gsSub1$gc.mmol.s,by=list(gsSub1$stand),FUN="length")$x
######## make plots
lwl<-55
lhl<-45

#increments of 0.005
#T is in g/ m2*s
ylr1 <- 0
yhr1 <- 0.015
#gc is in mmol m-2 s-2
#increments 10
ylr2 <- 0
yhr2 <-80
#increments of 1
ylr3 <- 0
yhr3 <- 3.5
#increments of 0.05
ylr4 <- 0
yhr4 <- .25
#increments of 10
ylr5 <- 0
yhr5 <- 80
#show a subset since too hard to see patterns
#2016 range is 183-245
#increments of 1
#subset 216-218 for a close up
xl16 <- 216
xh16 <- 218

xl16b <- 180
xh16b <- 245

#2017  159-226

xl17b <- 155
xh17b <- 230
#low
col1 <- rgb(51/255,51/255,51/255,.8)

#high
col2 <- rgb(191/255,191/255,191/255)
#sizes
ptcx <- 17
llw <- 25
alw <- 7
acol <- rgb(0,0,0,.5)
blw <- 12
mx <- 11
lx <- 12
lwt <- 12
lgx <- 15
tx <- 21
plw <- 5
jpeg(paste0(plotDI,"\\T_gc.jpg"), width=7500, height=5000, units="px",quality=100)
ab<-layout(matrix(seq(1,9), ncol=3, byrow=TRUE), width=rep(lcm(lwl),9),
				height=rep(lcm(lhl),9))
#T 2016				
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1), type="n", xlim=c(xl16-0.4,xh16+1.1), ylim=c(ylr1,yhr1), 
		xlab=" ", ylab=" ", xaxs="i",yaxs="i", axes=FALSE)
	points(TList2[[1]]$doy[TList2[[1]]$doy>=xl16&TList2[[1]]$doy<=xh16]
			+(TList2[[1]]$hour[TList2[[1]]$doy>=xl16&TList2[[1]]$doy<=xh16]/24),
			TList2[[1]]$T[TList2[[1]]$doy>=xl16&TList2[[1]]$doy<=xh16], type="b", col=col1, cex=ptcx, pch=15)
	points(TList2[[2]]$doy[TList2[[2]]$doy>=xl16&TList2[[2]]$doy<=xh16]
		+(TList2[[2]]$hour[TList2[[2]]$doy>=xl16&TList2[[2]]$doy<=xh16]/24),
			TList2[[2]]$T[TList2[[2]]$doy>=xl16&TList2[[2]]$doy<=xh16], type="b", col=col2, cex=ptcx, pch=15)		
	arrows(TList2[[1]]$doy[TList2[[1]]$doy>=xl16&TList2[[1]]$doy<=xh16]	
			+(TList2[[1]]$hour[TList2[[1]]$doy>=xl16&TList2[[1]]$doy<=xh16]/24),
			TList2[[1]]$T[TList2[[1]]$doy>=xl16&TList2[[1]]$doy<=xh16]-TList2[[1]]$T.se[TList2[[1]]$doy>=xl16&TList2[[1]]$doy<=xh16],
		TList2[[1]]$doy[TList2[[1]]$doy>=xl16&TList2[[1]]$doy<=xh16]	
			+(TList2[[1]]$hour[TList2[[1]]$doy>=xl16&TList2[[1]]$doy<=xh16]/24),
			TList2[[1]]$T[TList2[[1]]$doy>=xl16&TList2[[1]]$doy<=xh16]+TList2[[1]]$T.se[TList2[[1]]$doy>=xl16&TList2[[1]]$doy<=xh16],
			code=0, lwd=alw, col=acol)
	arrows(TList2[[2]]$doy[TList2[[2]]$doy>=xl16&TList2[[2]]$doy<=xh16]	
			+(TList2[[2]]$hour[TList2[[2]]$doy>=xl16&TList2[[2]]$doy<=xh16]/24),
			TList2[[2]]$T[TList2[[2]]$doy>=xl16&TList2[[2]]$doy<=xh16]-TList2[[2]]$T.se[TList2[[2]]$doy>=xl16&TList2[[2]]$doy<=xh16],
		TList2[[2]]$doy[TList2[[2]]$doy>=xl16&TList2[[2]]$doy<=xh16]	
			+(TList2[[2]]$hour[TList2[[2]]$doy>=xl16&TList2[[2]]$doy<=xh16]/24),
			TList2[[2]]$T[TList2[[2]]$doy>=xl16&TList2[[2]]$doy<=xh16]+TList2[[2]]$T.se[TList2[[2]]$doy>=xl16&TList2[[2]]$doy<=xh16],
			code=0, lwd=alw, col=acol)		
	axis(2, seq(ylr1,yhr1, by=.005), rep(" ", length(seq(ylr1,yhr1, by=.005))), lwd.ticks=lwt)
	mtext(seq(ylr1,yhr1, by=.005)*1000, at=seq(ylr1,yhr1, by=.005), side=2, line=5, cex=mx, las=2)
	mtext("Canopy ", side=2,cex=lx,line=75)
	mtext("transpiration", side=2,cex=lx,line=50)
	mtext(expression(paste("(",italic(T),", mg m"^"-2","s"^"-1",")")), side=2,cex=lx,line=25)				
	legend(216.65,.016,c(expression(paste(italic(T)," low ")),
					expression(paste(italic(T)," high "))),pch=15,
					col=c(col1,col2), cex=lgx,bty="n")
	text(218.8,0.01375,"(a)",cex=tx)
	box(which="plot", lwd=plw)

#T 2016				
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1), type="n", xlim=c(xl16b-1,xh16b+2), ylim=c(ylr4,yhr4), 
		xlab=" ", ylab=" ", xaxs="i",yaxs="i", axes=FALSE)
	points(Eday$doy[Eday$year==2016&Eday$site=="ld"],
		Eday$T.L.day[Eday$year==2016&Eday$site=="ld"],pch=19,col=col1,cex=ptcx, type="b",lwd=blw)
	points(Eday$doy[Eday$year==2016&Eday$site=="hd"],
		Eday$T.L.day[Eday$year==2016&Eday$site=="hd"],pch=19,col=col2,cex=ptcx,type="b",lwd=blw)
	arrows(Eday$doy[Eday$year==2016],
		Eday$T.L.day[Eday$year==2016]-Eday$T.se[Eday$year==2016],
		Eday$doy[Eday$year==2016],
		Eday$T.L.day[Eday$year==2016]+Eday$T.se[Eday$year==2016],
		code=0, lwd=alw, col=acol)	
	text(242,0.23,"(b)",cex=tx, lwd=plw)
	box(which="plot", lwd=plw)
	
#T 2017				
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1), type="n", xlim=c(xl17b-1,xh17b+2), ylim=c(ylr4,yhr4), 
		xlab=" ", ylab=" ", xaxs="i",yaxs="i", axes=FALSE)
	points(Eday$doy[Eday$year==2017&Eday$site=="ld"],
		Eday$T.L.day[Eday$year==2017&Eday$site=="ld"],pch=19,col=col1,cex=ptcx,type="b",lwd=blw)
	points(Eday$doy[Eday$year==2017&Eday$site=="hd"],
		Eday$T.L.day[Eday$year==2017&Eday$site=="hd"],pch=19,col=col2,cex=ptcx,type="b",lwd=blw)
	
		arrows(Eday$doy[Eday$year==2017],
		Eday$T.L.day[Eday$year==2017]-Eday$T.se[Eday$year==2017],
		Eday$doy[Eday$year==2017],
		Eday$T.L.day[Eday$year==2017]+Eday$T.se[Eday$year==2017],
		code=0, lwd=alw, col=acol)	
	axis(4, seq(ylr4,yhr4, by=.05), rep(" ", length(seq(ylr4,yhr4, by=.05))), lwd.ticks=lwt)
	mtext(seq(ylr4,yhr4, by=.05), at=seq(ylr4,yhr4, by=.05), side=4, line=5, cex=mx, las=2)
	mtext("Daily canopy ", side=4,cex=lx,line=45)
	mtext("transpiration", side=4,cex=lx,line=70)
	mtext(expression(paste("(",italic(T[day]),", L m"^"-2","day"^"-1",")")), side=4,cex=lx,line=95)
	
		legend(170,.260,c(expression(paste(italic(T[day])," low ")),
					expression(paste(italic(T[day])," high"))),pch=19,
					col=c(col1,col2), cex=lgx,bty="n")
	box(which="plot", lwd=plw)	
	text(226,0.23,"(c)",cex=tx)
	
				
#gc 2016				
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1), type="n", xlim=c(xl16-0.4,xh16+1.1), ylim=c(ylr2,yhr2), 
		xlab=" ", ylab=" ", xaxs="i",yaxs="i", axes=FALSE)
	points(gcList2[[1]]$doy[gcList2[[1]]$doy>=xl16&gcList2[[1]]$doy<=xh16]
		+(gcList2[[1]]$hour[gcList2[[1]]$doy>=xl16&gcList2[[1]]$doy<=xh16]/24),
			gcList2[[1]]$gc[gcList2[[1]]$doy>=xl16&gcList2[[1]]$doy<=xh16], type="b", col=col1, cex=ptcx, pch=15)
	points(gcList2[[2]]$doy[gcList2[[2]]$doy>=xl16&gcList2[[2]]$doy<=xh16]
		+(gcList2[[2]]$hour[gcList2[[2]]$doy>=xl16&gcList2[[2]]$doy<=xh16]/24),
			gcList2[[2]]$gc[gcList2[[2]]$doy>=xl16&gcList2[[2]]$doy<=xh16], type="b", col=col2, cex=ptcx, pch=15)		
		arrows(gcList2[[1]]$doy[gcList2[[1]]$doy>=xl16&gcList2[[1]]$doy<=xh16]	
			+(gcList2[[1]]$hour[gcList2[[1]]$doy>=xl16&gcList2[[1]]$doy<=xh16]/24),
			gcList2[[1]]$gc[gcList2[[1]]$doy>=xl16&gcList2[[1]]$doy<=xh16]-gcList2[[1]]$gc.se[gcList2[[1]]$doy>=xl16&gcList2[[1]]$doy<=xh16],
		gcList2[[1]]$doy[gcList2[[1]]$doy>=xl16&gcList2[[1]]$doy<=xh16]	
			+(gcList2[[1]]$hour[gcList2[[1]]$doy>=xl16&gcList2[[1]]$doy<=xh16]/24),
			gcList2[[1]]$gc[gcList2[[1]]$doy>=xl16&gcList2[[1]]$doy<=xh16]+gcList2[[1]]$gc.se[gcList2[[1]]$doy>=xl16&gcList2[[1]]$doy<=xh16],
			code=0, lwd=alw, col=acol)
	arrows(gcList2[[2]]$doy[gcList2[[2]]$doy>=xl16&gcList2[[2]]$doy<=xh16]	
			+(gcList2[[2]]$hour[gcList2[[2]]$doy>=xl16&gcList2[[2]]$doy<=xh16]/24),
			gcList2[[2]]$gc[gcList2[[2]]$doy>=xl16&gcList2[[2]]$doy<=xh16]-gcList2[[2]]$gc.se[gcList2[[2]]$doy>=xl16&gcList2[[2]]$doy<=xh16],
		gcList2[[2]]$doy[gcList2[[2]]$doy>=xl16&gcList2[[2]]$doy<=xh16]	
			+(gcList2[[2]]$hour[gcList2[[2]]$doy>=xl16&gcList2[[2]]$doy<=xh16]/24),
			gcList2[[2]]$gc[gcList2[[2]]$doy>=xl16&gcList2[[2]]$doy<=xh16]+gcList2[[2]]$gc.se[gcList2[[2]]$doy>=xl16&gcList2[[2]]$doy<=xh16],
			code=0, lwd=alw, col=acol)

	axis(2, seq(ylr2,yhr2-10, by=10), rep(" ", length(seq(ylr2,yhr2-10, by=10))), lwd.ticks=lwt)
	mtext(seq(ylr2,yhr2-10, by=10), at=seq(ylr2,yhr2-10, by=10), side=2, line=5, cex=mx, las=2)
		mtext("Canopy stomatal", side=2,cex=lx,line=75)
	mtext("conductance", side=2,cex=lx,line=50)
	mtext(expression(paste("(",italic(g[c]),", mmol m"^"-2","s"^"-1",")")), side=2,cex=lx,line=25)			
	legend(216.65,84,c(expression(paste(italic(g[c])," low ")),
					expression(paste(italic(g[c])," high "))),pch=15,
					col=c(col1,col2), cex=lgx,bty="n")
	text(218.8,73,"(d)",cex=tx)
	box(which="plot", lwd=plw)
#gc 2016				
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1), type="n", xlim=c(xl16b-1,xh16b+2), ylim=c(ylr5,yhr5), 
		xlab=" ", ylab=" ", xaxs="i",yaxs="i", axes=FALSE)
		points(gsDave$doy[gsDave$year==2016&gsDave$stand=="ld"],
		gsDave$gc.mmol.s[gsDave$year==2016&gsDave$stand=="ld"],pch=19,col=col1,cex=ptcx,type="b",lwd=blw)
		points(gsDave$doy[gsDave$year==2016&gsDave$stand=="hd"],
		gsDave$gc.mmol.s[gsDave$year==2016&gsDave$stand=="hd"],pch=19,col=col2,cex=ptcx,type="b",lwd=blw)
		arrows(gsDave$doy[gsDave$year==2016],
		gsDave$gc.mmol.s[gsDave$year==2016]-gsDave$se[gsDave$year==2016],
		gsDave$doy[gsDave$year==2016],
		gsDave$gc.mmol.s[gsDave$year==2016]+gsDave$se[gsDave$year==2016],
		code=0, lwd=alw, col=acol)	
	box(which="plot", lwd=plw)
	text(242,73,"(e)",cex=tx)
#gc 2017				
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1), type="n", xlim=c(xl17b-1,xh17b+2), ylim=c(ylr5,yhr5), 
		xlab=" ", ylab=" ", xaxs="i",yaxs="i", axes=FALSE)
		points(gsDave$doy[gsDave$year==2017&gsDave$stand=="ld"],
		gsDave$gc.mmol.s[gsDave$year==2017&gsDave$stand=="ld"],pch=19,col=col1,cex=ptcx,type="b",lwd=blw)
		points(gsDave$doy[gsDave$year==2017&gsDave$stand=="hd"],
		gsDave$gc.mmol.s[gsDave$year==2017&gsDave$stand=="hd"],pch=19,col=col2,cex=ptcx,type="b",lwd=blw)
	arrows(gsDave$doy[gsDave$year==2017],
		gsDave$gc.mmol.s[gsDave$year==2017]-gsDave$se[gsDave$year==2017],
		gsDave$doy[gsDave$year==2017],
		gsDave$gc.mmol.s[gsDave$year==2017]+gsDave$se[gsDave$year==2017],
		code=0, lwd=alw, col=acol)	
	axis(4, seq(ylr5,yhr5-10, by=10), rep(" ", length(seq(ylr5,yhr5-10, by=10))), lwd.ticks=lwt)
	mtext(seq(ylr5,yhr5-10, by=10), at=seq(ylr5,yhr5-10, by=10), side=4, line=5, cex=mx, las=2)
	mtext("Daily average", side=4,cex=lx,line=45)
	mtext("canopy stomatal", side=4,cex=lx,line=70)
	mtext("conductance", side=4,cex=lx,line=95)
	mtext(expression(paste("(",italic(g[c]),", mmol m"^"-2","day"^"-1",")")), side=4,cex=lx,line=120)
	
	legend(170,84,c(expression(paste(italic(g[c])," average low" )),
					expression(paste(italic(g[c])," average high "))),pch=19,
					col=c(col1,col2), cex=lgx,bty="n")	
	text(226,73,"(f)",cex=tx)
	box(which="plot", lwd=plw)
#D 2016				
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1), type="n", xlim=c(xl16-0.4,xh16+1.1), ylim=c(ylr3,yhr3), 
		xlab=" ", ylab=" ", xaxs="i",yaxs="i", axes=FALSE)
	points(datAA$doy[datAA$doy>=xl16&datAA$doy<=xh16&datAA$site=="hd"&datAA$year==2016]+
			(datAA$hour[datAA$doy>=xl16&datAA$doy<=xh16&datAA$site=="hd"&datAA$year==2016]/24),
			datAA$D[datAA$doy>=xl16&datAA$doy<=xh16&datAA$site=="hd"&datAA$year==2016], type="l",lwd=llw,
			col=col2)
			
	points(datAA$doy[datAA$doy>=xl16&datAA$doy<=xh16&datAA$site=="ld"&datAA$year==2016]+
			(datAA$hour[datAA$doy>=xl16&datAA$doy<=xh16&datAA$site=="ld"&datAA$year==2016]/24),
			datAA$D[datAA$doy>=xl16&datAA$doy<=xh16&datAA$site=="ld"&datAA$year==2016], type="l",lwd=llw,
			col=col1)	
	points(datPAR$doy[datPAR$doy>=xl16&datPAR$doy<=xh16&datPAR$site=="ld"&datPAR$year==2016]+
		(datPAR$hour[datPAR$doy>=xl16&datPAR$doy<=xh16&datPAR$site=="ld"&datPAR$year==2016]/24),
		datPAR$PAR.QSOS.Par[datPAR$doy>=xl16&datPAR$doy<=xh16&datPAR$site=="ld"&datPAR$year==2016]/1000,
		type="l",lwd=llw,col=col1, lty=3)
		
	points(datPAR$doy[datPAR$doy>=xl16&datPAR$doy<=xh16&datPAR$site=="hd"&datPAR$year==2016]+
		(datPAR$hour[datPAR$doy>=xl16&datPAR$doy<=xh16&datPAR$site=="hd"&datPAR$year==2016]/24),
		datPAR$PAR.QSOS.Par[datPAR$doy>=xl16&datPAR$doy<=xh16&datPAR$site=="hd"&datPAR$year==2016]/1000,
		type="l",lwd=llw,col=col2, lty=3)	
	)

			
	axis(1, seq(xl16,xh16, by=1), rep(" ", length(seq(xl16,xh16, by=1))), lwd.ticks=lwt)
	mtext(seq(xl16,xh16, by=1), at=seq(xl16,xh16, by=1), side=1, line=12, cex=mx)
	
	axis(2, seq(ylr3,yhr3-.5, by=.5), rep(" ", length(seq(ylr3,yhr3-.5, by=.5))), lwd.ticks=lwt)
	mtext(seq(ylr3,yhr3-.5, by=.5), at=seq(ylr3,yhr3-.5, by=.5), side=2, line=5, cex=mx,las=2)	
	mtext("Photosynthetic", side=2,cex=lx,line=125)
	mtext("active radiation", side=2,cex=lx,line=100)
	mtext(expression(paste("(",italic(PAR),",mmol m"^"-2","s"^"-1",")")), side=2,cex=lx,line=75)
	mtext("Vapor pressure", side=2,cex=lx,line=50)
	mtext(expression(paste("deficit (" ,italic(D),", kPa)")), side=2,cex=lx,line=25)
	legend(215.5,3.75,c(expression(paste(italic(D)," low ")),
					expression(paste(italic(D)," high ")),
					expression(paste(italic(PAR)," low ")),
					expression(paste(italic(PAR)," high "))
					),lty=c(1,1,3,3),lwd=llw,
					col=c(col1,col2,col1,col2), cex=lgx,bty="n")

					
	text(218.8,3.2,"(g)",cex=tx)
	box(which="plot", lwd=plw)
#D 2016			
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1), type="n", xlim=c(xl16b-1,xh16b+2), ylim=c(ylr3,yhr3), 
		xlab=" ", ylab=" ", xaxs="i",yaxs="i", axes=FALSE)
	points(maxD$doy[maxD$year==2016&maxD$site=="ld"],maxD$maxD[maxD$year==2016&maxD$site=="ld"],
		type="l", lwd=llw,col=col1, lty=5)
	points(maxD$doy[maxD$year==2016&maxD$site=="hd"],maxD$maxD[maxD$year==2016&maxD$site=="hd"],
		type="l", lwd=llw,col=col2, lty=5)	
		
	axis(1, seq(xl16b,xh16b, by=15), rep(" ", length(seq(xl16b,xh16b, by=15))), lwd.ticks=lwt)
	mtext(seq(xl16b,xh16b, by=15), at=seq(xl16b,xh16b, by=15), side=1, line=12, cex=mx)	
	text(242,3.2,"(h)",cex=tx)
	box(which="plot", lwd=plw)


#D 2017			
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1), type="n", xlim=c(xl17b-1,xh17b+2), ylim=c(ylr3,yhr3), 
		xlab=" ", ylab=" ", xaxs="i",yaxs="i", axes=FALSE)
	points(maxD$doy[maxD$year==2017&maxD$site=="ld"],maxD$maxD[maxD$year==2017&maxD$site=="ld"],
		type="l", lwd=llw,col=col1, lty=5)
	points(maxD$doy[maxD$year==2017&maxD$site=="hd"],maxD$maxD[maxD$year==2017&maxD$site=="hd"],
		type="l", lwd=llw,col=col2, lty=5)	
	axis(4, seq(ylr3,yhr3-.5, by=.5), rep(" ", length(seq(ylr3,yhr3-.5, by=.5))), lwd.ticks=lwt)
	mtext(seq(ylr3,yhr3-.5, by=.5), at=seq(ylr3,yhr3-.5, by=.5), side=4, line=5, cex=mx, las=2)	
	axis(1, seq(xl17b+5,xh17b, by=15), rep(" ", length(seq(xl17b+5,xh17b, by=15))), lwd.ticks=lwt)
	mtext(seq(xl17b+5,xh17b, by=15), at=seq(xl17b+5,xh17b, by=15), side=1, line=12, cex=mx)		
	box(which="plot", lwd=plw)	
	mtext("Day of year", side=1,cex=lx,outer=TRUE, line = -25 )
	mtext("Daily maximum", side=4,cex=lx,line=45)
	mtext("vapor pressure", side=4,cex=lx,line=70)
	mtext(expression(paste("deficit (",italic(D[max]),", kPa)")), side=4,cex=lx,line=95)
		legend(170,3.7,c(expression(paste(italic(D[max])," low" )),
					expression(paste(italic(D[max])," high "))),lwd=llw,lty=5,
					col=c(col1,col2), cex=lgx,bty="n")	
	text(226,3.2,"(i)",cex=tx)				
dev.off()				









###############################End T/gc                 ########################################################################
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
text(10.1,4.7, "(a)",cex=txcx)
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
text(10.1,4.7, "(b)",cex=txcx)
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
text(10.1,4.7, "(c)",cex=txcx)
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
text(10.1,4.7, "(d)",cex=txcx)
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
text(10.1,4.7, "(e)",cex=txcx)
#empty plot mid july
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(0,xH),ylim=c(yuL,0), xlab=" ", ylab=" ", axes=FALSE,yaxs="i",xaxs="i")
text(5,90,"mid July", cex=txcx)
axis(1,seq(0,9, by=3), lab=rep(" ",length(seq(0,9, by=3))), las=2, cex.axis=2, lwd.ticks=3)
mtext(seq(0,9, by=3), at=seq(0,9, by=3), line=2.5, side=1, cex=ltcx)
box(which="plot")
text(10.1,4.7, "(f)",cex=txcx)	
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
text(10.1,4.7, "(g)",cex=txcx)
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
text(10.1,4.7, "(h)",cex=txcx)
dev.off()
###############################End vertical root figure ########################################################################
################################################################################################################################
################################################################################################################################



#####################################################################
####  figure 6. Soil type root profile                           ####
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
tx <- 4
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
text(7.4,5.6,"(a)",cex=tx)
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
text(1.85,18.5,"(b)",cex=tx)
legend(.92,12,c("green moss", "brown moss", "fibric organic"), fill=c(mgcol, mbcol,ocol), bty="n", cex=lgcx)
dev.off()


###############################End soil root figure    ########################################################################
################################################################################################################################
################################################################################################################################


#####################################################################
####  figure 6. goodness of fit figure                           ####
#####################################################################


datMGG <-datMG[datMG$parms=="rep.gs",] 

fitG <- lm(datMGG$Mean~datGC$g.c)
fitR <- lm(datRootR$rep.mean~datRootD$bio.mg.cm3)
gi <- round(summary(fitG)$coefficients[1,1],2)
gs <-round(summary(fitG)$coefficients[2,1],2)
ri <- round(summary(fitR)$coefficients[1,1],2)
rs <-round(summary(fitR)$coefficients[2,1],2)
rg <- round(summary(fitG)$r.squared,3)
rr <- round(summary(fitR)$r.squared,3)

l1 <- 0
h1 <- 70
l2 <- 0
h2 <- 12
pcx <- 3
mx <- 3
lx <- 4
ltw <- 3
llw <- 8
rtx <- 3
lgx <- 3
tx <- 4

#setup plot layout
wd<-22
hd<-22
jpeg(paste0(plotDI,"\\goodness_of_fit.jpg"), width=1800, height=1200, units="px",quality=100)
ac<-layout(matrix(seq(1,2),ncol=2), width=rep(lcm(wd),2),height=rep(lcm(hd),2))
	layout.show(ac)
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1), type="n",ylim=c(l1-1,h1),xlim=c(l1-1,h1), axes=FALSE, xlab=" ", ylab=" ", yaxs="i",
		xaxs="i")
	points(datGC$g.c,datMGG$Mean, pch=19,cex=pcx)
	axis(1, seq(l1,h1-10, by=10), rep(" ", length(seq(l1,h1-10, by=10))),lwd.ticks=ltw)
	mtext(seq(l1,h1-10, by=10), at=seq(l1,h1-10, by=10), side=1, line=3,cex=mx)
	axis(2, seq(l1,h1-10, by=10), rep(" ", length(seq(l1,h1-10, by=10))),lwd.ticks=ltw)
	mtext(seq(l1,h1-10, by=10), at=seq(l1,h1-10, by=10), side=2, line=3,cex=mx,las=2)
	abline(fitG, lwd=llw,lty=3, col="grey70")
	abline(0,1, lwd=llw,lty=1, col="grey70")
	text(30,65, expression(paste(hat(italic(g[c])),
						"= 6.74 + 0.7",italic(g[c]))),
						cex=rtx)
	text(30,58, expression(paste("R"^"2","= 0.673")),
						cex=rtx)
	mtext("Predicted canopy stomatal", side=2, cex=lx, line=12)
	mtext(expression(paste("conductance(",hat(italic(g[c])),", mmol m"^"-2","s"^"-1",")"))
			, side=2, cex=lx, line=7)
	mtext("Observed canopy stomatal", side=1, cex=lx, line=7)
	mtext(expression(paste("conductance(",italic(g[c]),", mmol m"^"-2","s"^"-1",")"))
			, side=1, cex=lx, line=12)						
	text(2,67, "(a)", cex=tx)
	box(which="plot")
	
	
#root biomass	
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1), type="n",ylim=c(l2-.5,h2+.5),xlim=c(l2-.5,h2+.5), axes=FALSE, xlab=" ", ylab=" ", yaxs="i",
		xaxs="i")
	points(datRootD$bio.mg.cm3,datRootR$rep.mean,pch=19,cex=pcx)
	axis(1, seq(l2,h2, by=3), rep(" ", length(seq(l2,h2, by=3))),lwd.ticks=ltw)
	mtext(seq(l2,h2, by=3), at=seq(l2,h2, by=3), side=1, line=3,cex=mx)
	axis(4, seq(l2,h2, by=3), rep(" ", length(seq(l2,h2, by=3))),lwd.ticks=ltw)
	mtext(seq(l2,h2, by=3), at=seq(l2,h2, by=3), side=4, line=3,cex=mx,las=2)		
	abline(fitR, lwd=llw,lty=3, col="grey70")
	abline(0,1, lwd=llw,lty=1, col="grey70")
	text(5,11.75, expression(paste(hat(italic(R[b])),
						"= 0.76 + 0.45",italic(R[b]))),
						cex=rtx)
	text(5,10.5, expression(paste("R"^"2","= 0.464")),
						cex=rtx)
	legend(-0.5,10,c("1:1","model fit"),lwd=llw,lty=c(1,3),col="grey70",cex=lgx,bty="n")	
	box(which="plot")
		mtext("Observed root biomass", side=1, cex=lx, line=7)
	mtext(expression(paste("(",italic(R[b]),",mg cm"^"-3",")"))
			, side=1, cex=lx, line=12)	
		mtext("Predicted root biomass", side=4, cex=lx, line=7)
	mtext(expression(paste("(",hat(italic(R[b])),",mg cm"^"-3",")"))
			, side=4, cex=lx, line=12)		
		text(0,12, "(b)", cex=tx)
dev.off()



###############################End goodness of fit      ########################################################################
################################################################################################################################
################################################################################################################################


#####################################################################
####  figure 6. precipitation weights                            ####
#####################################################################

wpr <- datC[datC$parms2=="wpr",]
wpr$stand <- rep(c(1,2), times=6)
wpr$wI <- rep(seq(1,6), each=2)

xl <- 0
xh <- 15.5
yl <- 0
yh <- 1
#low
col1 <- rgb(51/255,51/255,51/255)
#high
col2 <- rgb(191/255,191/255,191/255)
lwt <- 3
mx <- 4
lx <- 5
lwa <- 3
xseq1 <- c(1,3.5,6,8.5,11,13.5)
xseq2 <- c(2,4.5,7,9.5,12,14.5)
xas <- c(1.5,4,6.5,9,11.5,14)
lgx <- 4
wd<-35
hd<-25
jpeg(paste0(plotDI,"\\precipitation.jpg"), width=2000, height=1100, units="px",quality=100)
	ac<-layout(matrix(seq(1),ncol=1), width=rep(lcm(wd),1),height=rep(lcm(hd),1))
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1), type="n",xlim=c(xl,xh),ylim=c(yl,yh), axes=FALSE, 
		xlab=" ", ylab=" ", yaxs="i",
		xaxs="i")
	
	abline(h=1/6, lwd=5, col="grey60", lty=3)
	for(i in 1:6){
		polygon(c(xseq1[i]-.5,xseq1[i]-.5,xseq1[i]+.5,xseq1[i]+.5),
			c(0, wpr$Mean[wpr$stand==1][i],wpr$Mean[wpr$stand==1][i],0),
			border=NA, col=col1)
		polygon(c(xseq2[i]-.5,xseq2[i]-.5,xseq2[i]+.5,xseq2[i]+.5),
			c(0, wpr$Mean[wpr$stand==2][i],wpr$Mean[wpr$stand==2][i],0),
			border=NA, col=col2)	
	}
	arrows(xseq1,wpr$X2.5.[wpr$stand==1],xseq1,wpr$X97.5.[wpr$stand==1], code=0, 
			lwd=lwa)
	arrows(xseq2,wpr$X2.5.[wpr$stand==2],xseq2,wpr$X97.5.[wpr$stand==2], code=0, 
			lwd=lwa)
	axis(2, seq(0,1, by=.1), rep(" ",length(seq(0,1, by=.1))), lwd.ticks=lwt)
	mtext(seq(0,1, by=.1), at=seq(0,1, by=.1), cex=mx, side=2, line=2, las=2)
	axis(1, c(-1,xas,20), rep(" ", length(c(-1,xas,20))), lwd.ticks=lwt)
	mtext(c("1-3", "4-7", "8-14", "15-21",
			"22-35", "36-60"), at=xas, cex=mx, side=1,line=3)
	mtext(rep("days",6), at=xas, cex=mx,side=1,line=6)
	legend(5, 1, c("low","high","uniform average"), col=c(col1,col2,"grey60"),
		pch=c(15,15,NA), lwd=c(NA,NA,5), lty=c(NA,NA,3), bty="n", cex=lgx)
	mtext("Importance weight", side=2, cex=lx,line=10)	
	mtext("Lag period", side=1, cex=lx,line=10)
dev.off()




###############################End precipitation        ########################################################################
################################################################################################################################
################################################################################################################################

#output table of regression results
drparms <- datC[datC$parms2=="a"|datC$parms2=="b"|datC$parms2=="d",]


write.table(drparms, paste0(plotDI,"\\regression_coefficient.csv"),sep=",")


#####################################################################
####  appendix. allometry                                        ####
#####################################################################
#dbh vs sapwood
#leaf area vs dbh

#################################################################
####calculate sapwood thicknes                            #######
#################################################################

#fit a linear regression for sap thickness
#low
lmSWL <- lm(datSWAl$SWT[datSWAl$stand=="LDF2"]~datSWAl$DBH[datSWAl$stand=="LDF2"])
summary(lmSWL)
#high
lmSWH <- lm(datSWAl$SWT[datSWAl$stand=="DAV"]~datSWAl$DBH[datSWAl$stand=="DAV"])
summary(lmSWH)
#fit a linear regression for bark thickness
#low
lmBL <- lm(datSWAl$Bark[datSWAl$stand=="LDF2"]~datSWAl$DBH[datSWAl$stand=="LDF2"])
summary(lmBL)
#high
lmBH <- lm(datSWAl$Bark[datSWAl$stand=="DAV"]~datSWAl$DBH[datSWAl$stand=="DAV"])
summary(lmBH)

#for sapwood thickness, ldf2 just gets the stand mean since not significant
#datTreeDF $SWT <- ifelse(datTreeDF$stand=="hd", coefficients(lmSWH)[1]+(coefficients(lmSWH)[2]*datTreeDF$DBHt),
#				mean(datSW$SWT[datSW$stand=="LDF2"]))

#datTreeDF $Bark <- ifelse(datTreeDF$stand=="hd", coefficients(lmBH)[1]+(coefficients(lmBH)[2]*datTreeDF$DBHt),
#				coefficients(lmBL)[1]+(coefficients(lmBL)[2]*datTreeDF$DBHt))


#leaf allometry function 
leaf.bio<-function(DBH,a.leaf,b.leaf){a.leaf*(DBH^b.leaf)}
#fit nonlinear function
nlsLow <- nls(leaf~a.leaf*(DBH^b.leaf), data=list(DBH=datAllom$dbh[datAllom$density=="Low"],
				leaf=datAllom$foliage[datAllom$density=="Low"]),
				start=list(a.leaf=40.5, b.leaf=1.41))
nlsHigh <- nls(leaf~a.leaf*(DBH^b.leaf), data=list(DBH=datAllom$dbh[datAllom$density=="High"],
				leaf=datAllom$foliage[datAllom$density=="High"]),
				start=list(a.leaf=40.5, b.leaf=1.41))
				


########sapwood thickness plot####################
hd <- 20
wd <- 20


px <- 6
llw <- 5
mx <- 4
lx <- 5
ltw <- 4
tx <- 5
dl1 <- 0
dh1 <- 30
yl1 <- 0
yh1 <- 3
yl2 <- 0
yh2 <- 2
#g
yl3 <- 0
yh3 <- 5000				
jpeg(file=paste0(plotDI,"\\allometryAppendix.jpg"), width=2000, height=2000, units="px")
	layout(matrix(seq(1,6),ncol=2,byrow=TRUE),width=rep(lcm(wd),6),height=rep(lcm(hd),6))
#ld swt	
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1),type="n", xlim=c(dl1,dh1), ylim=c(yl1,yh1),axes=FALSE,xlab=" ", ylab=" ",
			xaxs="i",yaxs="i")
	points(datSWAl$DBH[datSWAl$stand=="LDF2"],datSWAl$SWT[datSWAl$stand=="LDF2"], pch=19,cex=px)
	abline(h=mean(datSWAl$SWT[datSWAl$stand=="LDF2"]), lwd=llw, lty=3)
	text(15, 2.5, paste("sw =", round(mean(datSWAl$SWT[datSWAl$stand=="LDF2"]),2)), cex=tx)
	axis(2, seq(yl1,yh1,by=.5),rep(" ",length(seq(yl1,yh1,by=.5))), lwd.ticks=ltw)
	mtext(seq(yl1,yh1,by=.5), at=seq(yl1,yh1,by=.5),side=2,line=4,cex=mx,las=2)
	mtext("Sapwood thickness", side=2,line=25,cex=lx)
	mtext("(sw, cm)", side=2,line=15,cex=lx)
	box(which="plot")
#hd swt	
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1),type="n", xlim=c(dl1,dh1), ylim=c(yl1,yh1),axes=FALSE,xlab=" ", ylab=" ",
			xaxs="i",yaxs="i")
	points(datSWAl$DBH[datSWAl$stand=="DAV"],datSWAl$SWT[datSWAl$stand=="DAV"], pch=19,cex=px)
	
	text(15, 2.5, paste("sw =", round(coefficients(lmSWH)[1],2),"+ dbh",round(coefficients(lmSWH)[2],2)), cex=tx)
	abline(lmSWH, lwd=llw)
	axis(4, seq(yl1,yh1,by=.5),rep(" ",length(seq(yl1,yh1,by=.5))), lwd.ticks=ltw)
	mtext(seq(yl1,yh1,by=.5), at=seq(yl1,yh1,by=.5),side=4,line=4,cex=mx,las=2)
	mtext("Sapwood thickness", side=4,line=20,cex=lx)
	mtext("(sw, cm)", side=4,line=30,cex=lx)
	box(which="plot")	


#ld bwt 
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1),type="n", xlim=c(dl1,dh1), ylim=c(yl2,yh2),axes=FALSE,xlab=" ", ylab=" ",
			xaxs="i",yaxs="i")
	points(datSWAl$DBH[datSWAl$stand=="LDF2"],datSWAl$Bark[datSWAl$stand=="LDF2"], pch=19,cex=px)
	abline(lmBL, lwd=llw)
	text(15, 2.5, paste("bw =", round(coefficients(lmBL)[1],2),"+ dbh",round(coefficients(lmBL)[2],2)), cex=tx)
	axis(2, seq(yl2,yh2-.5,by=.5),rep(" ",length(seq(yl2,yh2-.5,by=.5))), lwd.ticks=ltw)
	mtext(seq(yl2,yh2-.5,by=.5), at=seq(yl2,yh2-.5,by=.5),side=2,line=4,cex=mx,las=2)
		mtext("Bark thickness", side=2,line=25,cex=lx)
	mtext("(bw, cm)", side=2,line=15,cex=lx)
	box(which="plot")
#hd bwt 
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1),type="n", xlim=c(dl1,dh1), ylim=c(yl2,yh2),axes=FALSE,xlab=" ", ylab=" ",
			xaxs="i",yaxs="i")
	points(datSWAl$DBH[datSWAl$stand=="DAV"],datSWAl$Bark[datSWAl$stand=="DAV"], pch=19,cex=px)
	text(15, 2.5, paste("bw =", round(coefficients(lmBH)[1],2),"+ dbh",round(coefficients(lmBH)[2],2)), cex=tx)
	
	abline(lmBH, lwd=llw)
	axis(4, seq(yl2,yh2-.5,by=.5),rep(" ",length(seq(yl2,yh2-.5,by=.5))), lwd.ticks=ltw)
	mtext(seq(yl2,yh2-.5,by=.5), at=seq(yl2,yh2-.5,by=.5),side=4,line=4,cex=mx,las=2)
	mtext("Bark thickness", side=4,line=20,cex=lx)
	mtext("(bw, cm)", side=4,line=30,cex=lx)
	box(which="plot")

#ld leaf
par(mai=c(0,0,0,0))	
	plot(c(0,1),c(0,1),type="n", xlim=c(dl1,dh1), ylim=c(yl3,yh3),axes=FALSE,xlab=" ", ylab=" ",
			xaxs="i",yaxs="i")
	
	points(datAllom$dbh[datAllom$density=="Low"],datAllom$foliage[datAllom$density=="Low"],
			pch=19,cex=px)
	points(seq(0,30, by=.1), leaf.bio(seq(0,30, by=.1), summary(nlsLow)$coefficients[1,1],
				summary(nlsLow)$coefficients[2,1]), type="l", lwd=llw)			
	text(15,4000, expression(paste("lm = 150.50"^"1.00dbh",)), cex=tx)
	
	axis(2, seq(yl3,yh3-500,by=500),rep(" ",length(seq(yl3,yh3-500,by=500))), lwd.ticks=ltw)
	mtext(seq(yl3,yh3-500,by=500)/1000, at=seq(yl3,yh3-500,by=500),side=2,line=4,cex=mx,las=2)
	mtext("Canopy leaf", side=2,line=25,cex=lx)
	mtext("mass (lm, kg)", side=2,line=15,cex=lx)
	mtext("Diameter at breast height (cm)", side=1, outer=TRUE, cex=lx, line=-5)
	
	axis(1, seq(dl1,dh1-5, by=5), rep(" ", length(seq(dl1,dh1-5, by=5))), lwd.ticks=ltw)
	mtext(seq(dl1,dh1-5, by=5), at=seq(dl1,dh1-5, by=5), side=1,line=5,cex=mx)
	box(which="plot")
	
#hd leaf	
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1),type="n", xlim=c(dl1,dh1), ylim=c(yl3,yh3),axes=FALSE,xlab=" ", ylab=" ",
			xaxs="i",yaxs="i")
	points(datAllom$dbh[datAllom$density=="High"],datAllom$foliage[datAllom$density=="High"],
			pch=19,cex=px)
	points(seq(0,30, by=.1), leaf.bio(seq(0,30, by=.1), summary(nlsHigh)$coefficients[1,1],
				summary(nlsHigh)$coefficients[2,1]), type="l", lwd=llw)
	axis(4, seq(yl3,yh3-500,by=500),rep(" ",length(seq(yl3,yh3-500,by=500))), lwd.ticks=ltw)
	mtext(seq(yl3,yh3-500,by=500)/1000, at=seq(yl3,yh3-500,by=500),side=4,line=4,cex=mx,las=2)		
	mtext("Canopy leaf", side=4,line=20,cex=lx)
	mtext("mass (lm, kg)", side=4,line=30,cex=lx)
	axis(1, seq(dl1,dh1-5, by=5), rep(" ", length(seq(dl1,dh1-5, by=5))), lwd.ticks=ltw)
	mtext(seq(dl1,dh1-5, by=5), at=seq(dl1,dh1-5, by=5), side=1,line=5,cex=mx)
	text(15,4000, expression(paste("lm = 7.57"^"1.73dbh",)), cex=tx)
	
	box(which="plot")
			
dev.off()				




#past precip vs soil temperature and thaw depth 

###############################End allometry           ########################################################################
################################################################################################################################
################################################################################################################################


#####################################################################
####  appendix. correlations                                     ####
#####################################################################

#show soil temp: T.Dcm2, TD, and precip from mod.out
pastpr <- datC[datC$parms2=="pastpr",]
pastpr$Days <- rep(seq(1,60),each=2)
pastpr$stand <- rep(seq(1,2), times=60)

standDayAll <- join(pastpr, datStandD, by=c("Days","stand"), type="right")


#make a plot of the correlations
jpeg(file=paste0(plotDI,"\\correlationAppendix.jpg"), width=1500, height=1000, units="px")
par(mfrow=c(1,2), mai=c(2,2,2,2))
plot(standDayAll$Mean,standDayAll$TD, pch=19, cex=2, xlab="Past precipitation (mm)", ylab="Thaw depth (cm)",
			cex.lab=2, axes=FALSE)
	axis(2, seq(-20,80, by=20),cex.axis=2, las=2)
	axis(1, seq(-10,30,by=5), cex.axis=2)
text(20,20,paste("r =",round(cor(standDayAll$Mean,standDayAll$TD),2)), cex=2)
plot(standDayAll$Mean,standDayAll$T.Dcm2, pch=19, cex=2, xlab="Past precipitation (mm)", ylab="Organic soil temperature (C)",
			cex.lab=2, axes=FALSE)
			
	axis(2, seq(-10,8, by=2),cex.axis=2, las=2)
	axis(1, seq(-10,30,by=5), cex.axis=2)	
text(20,1,paste("r =",round(cor(standDayAll$Mean,standDayAll$T.Dcm2),2)), cex=2)	

dev.off()