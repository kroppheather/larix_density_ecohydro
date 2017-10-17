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
source("c:\\Users\\hkropp\\Documents\\GitHub\\larch_density_ecohydro\\sapflux_process.r")
#libraries loaded from source
#plyr, lubridate,caTools

#set the plotting directory
plotDI <- "c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\time_plot"
#################################################################
####read in thaw depth data                               #######
#################################################################

source("c:\\Users\\hkropp\\Documents\\GitHub\\larch_density_ecohydro\\thaw_depth_process.r")

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

#################################################################
####calculate daily transpiration                         #######
#################################################################


#El is in g m-2 s-1

#convert to g m-2 half hour-1
#and reorganize
E.temp <- list(El.L,El.L17,El.H,El.H17)
E.dim <- numeric(0)
E.temp2 <- list()
E.temp3 <- list()
for(i in 1:4){
	E.dim[i] <- dim(E.temp[[i]])[2]
	E.temp2[[i]] <- data.frame(E.temp[[i]][,1:3], E.temp[[i]][,4:E.dim[i]]*60*30)
	E.temp3[[i]] <- data.frame(doy=rep(E.temp2[[i]]$doy,times=E.dim[i]-3),
								year=rep(E.temp2[[i]]$year,times=E.dim[i]-3),
								hour=rep(E.temp2[[i]]$hour,times=E.dim[i]-3),
								E.hh = as.vector(data.matrix(E.temp2[[i]][,4:E.dim[i]])),
								tree = rep(seq(1,E.dim[i]-3), each=dim(E.temp2[[i]])[1]))
	E.temp3[[i]] <- na.omit(E.temp3[[i]])
}

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
EInfo$stand <- ifelse(EInfo$dataset==1|EInfo$dataset==2,"ld","hd")

#get the stand averages of daily transpiration across day

EdayLm <- aggregate(EInfo$T.Lday, by=list(EInfo$doy,EInfo$year,EInfo$stand), FUN="mean")
EdayLsd <- aggregate(EInfo$T.Lday, by=list(EInfo$doy,EInfo$year,EInfo$stand), FUN="sd")
EdayLl <- aggregate(EInfo$T.Lday, by=list(EInfo$doy,EInfo$year,EInfo$stand), FUN="length")

Eday <- EdayLm
colnames(Eday) <- c("doy","year","site","T.L.day")
Eday$T.sd <- EdayLsd$x
Eday$T.n <- EdayLl$x

Eday$T.se <- Eday$T.sd/sqrt(Eday$T.n)



#################################################################
####calculate gc daily values across all tree             #######
#################################################################

#reoranize into data frames
gctemp <- list(gc.L, gc.L17, gc.H, gc.H17)

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

#check that there aren't days with too few observations
gclength <- list()
gcLday1 <- list()
gcLday2 <- list()
for(i in 1:4){
	#how many observations in days and trees
	gclength[[i]] <- aggregate(gctemp2[[i]]$gc.h, by=list(gctemp2[[i]]$doy,gctemp2[[i]]$year,gctemp2[[i]]$tree), FUN="length")
	#subset to exclude trees that only have one obs in a day
	gclength[[i]] <- gclength[[i]][gclength[[i]]$x>3,]
	#how many trees in days
	gcLday1[[i]] <- aggregate(gclength[[i]]$Group.3, by=list(gclength[[i]]$Group.1,gclength[[i]]$Group.2),FUN="length")
		
}
# alot of observations so no need to subset more
#get the average daily gc across all trees


	gsDave <- aggregate(gctemp3$gc.h, by=list(gctemp3$doy,gctemp3$year,gctemp3$datset), FUN="mean")
	gsDsd <- aggregate(gctemp3$gc.h, by=list(gctemp3$doy,gctemp3$year,gctemp3$datset), FUN="sd")
	gsDn <- aggregate(gctemp3$gc.h, by=list(gctemp3$doy,gctemp3$year,gctemp3$datset), FUN="length")
	
	colnames(gsDave) <- c("doy", "year", "dataset","gc.mmol.s")
	gsDave$gc.sd <- gsDsd$x
	gsDave$gc.n <- gsDn$x
	gsDave$gc.se <- gsDave$gc.sd/sqrt(gsDave$gc.n)
	
	gsHHave <- aggregate(gctemp3$gc.h, by=list(gctemp3$hour,gctemp3$doy,gctemp3$year,gctemp3$datset), FUN="mean")
	gsHHsd <- aggregate(gctemp3$gc.h, by=list(gctemp3$hour,gctemp3$doy,gctemp3$year,gctemp3$datset), FUN="sd")
	gsHHn <- aggregate(gctemp3$gc.h, by=list(gctemp3$hour,gctemp3$doy,gctemp3$year,gctemp3$datset), FUN="length")
	colnames(gsHHave) <- c("hour","doy", "year", "dataset","gc.mmol.s")
	gsHHave$gc.sd <- gsHHsd$x
	gsHHave$gc.n <- gsHHn$x
	gsHHave$gc.se <- gsHHave$gc.sd/sqrt(gsHHave$gc.n)
#label the site
		gsDave$site <- ifelse(gsDave$dataset==1|gsDave$dataset==2,"ld","hd")
		gsHHave$site <- ifelse(gsHHave$dataset==1|gsHHave$dataset==2,"ld","hd")


#################################################################
####aggregate and organize met                            #######
#################################################################
#subset and match
datLRHmet <- data.frame(datRH[datRH$site=="ld",1:3], RH=datRH$RH.VP4[datRH$site=="ld"])
datLTCmet <- data.frame(datTC[datTC$site=="ld",1:3], Temp=datTC$TempC.VP4[datTC$site=="ld"])

datHRHmet <- data.frame(datRH[datRH$site=="hd",1:3], RH=datRH$RH.VP4[datRH$site=="hd"])
datHTCmet <- data.frame(datTC[datTC$site=="hd",1:3], Temp=datTC$TempC.VP4[datTC$site=="hd"])
#join temp and RH
datLmet <- join(datLRHmet, datLTCmet, by=c("doy","year","hour"),type="inner")
datHmet <- join(datHRHmet, datHTCmet, by=c("doy","year","hour"),type="inner")
#calculate VPD
datLe.sat<-0.611*exp((17.502*datLmet$Temp)/(datLmet$Temp+240.97))
datHe.sat<-0.611*exp((17.502*datHmet$Temp)/(datHmet$Temp+240.97))
datLRHfix<-ifelse(datLmet$RH>=1,.999,datLmet$RH)
datHRHfix<-ifelse(datHmet$RH>=1,.999,datHmet$RH)

datLmet$D<-(datLe.sat-(datLRHfix*datLe.sat))
datHmet$D<-(datHe.sat-(datHRHfix*datHe.sat))


#join PAR to the dataframes
datPARL <- data.frame(doy=datPAR$doy[datPAR$site=="ld"], year=datPAR$year[datPAR$site=="ld"],
						hour=datPAR$hour[datPAR$site=="ld"], PAR=datPAR$PAR.QSOS.Par[datPAR$site=="ld"])
datPARH <- data.frame(doy=datPAR$doy[datPAR$site=="hd"], year=datPAR$year[datPAR$site=="hd"],
						hour=datPAR$hour[datPAR$site=="hd"], PAR=datPAR$PAR.QSOS.Par[datPAR$site=="hd"])		

#join into met
datLmet <- join(datLmet, datPARL, by=c("doy","year","hour"), type="left")
datHmet <- join(datHmet, datPARH, by=c("doy","year","hour"), type="left")

#pull out daily means
dayLD <- aggregate(datLmet$D, by=list(datLmet$doy,datLmet$year), FUN="mean")
colnames(dayLD) <- c("doy","year","D")
dayLT <- aggregate(datLmet$Temp, by=list(datLmet$doy,datLmet$year), FUN="mean")
colnames(dayLT) <- c("doy","year","T")
dayL <- join(dayLT,dayLD, by=c("doy","year"),type="full")
	
dayHD <- aggregate(datHmet$D, by=list(datHmet$doy,datHmet$year), FUN="mean")
colnames(dayHD) <- c("doy","year","D")
dayHT <- aggregate(datHmet$Temp, by=list(datHmet$doy,datHmet$year), FUN="mean")
colnames(dayHT) <- c("doy","year","T")
dayH <- join(dayHT,dayHD, by=c("doy","year"),type="full")

	
#################################################################
####make a panel of daily met and T and gc calc           #######
#################################################################
#filter out point that seems to have an erroneous meas
Eday <- Eday[Eday$T.L.day<.4,]
#day range for x axis 
xl2016 <- 180
xh2016 <- 245
xl2017 <- 155
xh2017 <- 230
#subset precip
prec2016 <- datAirP[datAirP$doy<=xh2016&datAirP$doy>=xl2016&datAirP$year==2016,]
prec2017 <- datAirP[datAirP$doy<=xh2017&datAirP$doy>=xl2017&datAirP$year==2017,]
#set up plot widths
wd <- 35
hd <-17
colL <- "royalblue"
colH <- "tomato3"
colHt <- rgb(205/255,79/255,57/255, .5)
colLt <- rgb(65/255,105/255,225/255,.5)


ylT <- 0
yhT <- .3
ylG <- 0
yhG <- 300
ylA <- 0
yhA <- 25
ylD <- 0
yhD <- 1.6

axisC <- 5

TDmax <- 75
TDscale <- yhA/TDmax
Prmax <- 40
Prscale <- yhD/Prmax

jpeg(paste0(plotDI , "\\daily_summary.jpg"), width=2600, height=2200, units="px")
	ab <- layout(matrix(seq(1,8), ncol=2, byrow=TRUE), width=rep(lcm(wd),8), height=rep(lcm(hd),8))


	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1), xlim=c(xl2016,xh2016), ylim=c(ylT,yhT),type="n", axes=FALSE, xlab=" ", ylab=" ",
			yaxs="i", xaxs="i")
		points(Eday$doy[Eday$site=="ld"&Eday$year==2016],
		Eday$T.L.day[Eday$site=="ld"&Eday$year==2016],pch=19,
		col=colL,cex=5 )	
		points(Eday$doy[Eday$site=="hd"&Eday$year==2016],
		Eday$T.L.day[Eday$site=="hd"&Eday$year==2016],pch=19,
		col=colH,cex=5 )		
		arrows(Eday$doy[Eday$year==2016],
				Eday$T.L.day[Eday$year==2016]-
				Eday$T.se[Eday$year==2016],
				Eday$doy[Eday$year==2016],
				Eday$T.L.day[Eday$year==2016]+
				Eday$T.se[Eday$year==2016],lwd=3, code=0)
		axis(2, seq(ylT,yhT, by=.1 ), las=2, cex.axis=axisC, lwd.ticks=3)	
		legend(220,yhT,c("low density", "high density"), col=c(colL,colH), pch=19, cex=4, bty="n")
	mtext("Daily transpiraiton", side=2, line=18, cex=4)
	mtext(expression(paste("(L m"^"-2","day"^"-1",")")), side=2, line=10, cex=4)
	mtext("2016", side=3, line=5, cex=4)
	box(which="plot")

	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1), xlim=c(xl2017,xh2017), ylim=c(ylT,yhT),type="n", axes=FALSE, xlab=" ", ylab=" ",
			yaxs="i", xaxs="i")
		points(Eday$doy[Eday$site=="ld"&Eday$year==2017],
			Eday$T.L.day[Eday$site=="ld"&Eday$year==2017],pch=19,
			col=colL,cex=5 )	
		points(Eday$doy[Eday$site=="hd"&Eday$year==2017],
			Eday$T.L.day[Eday$site=="hd"&Eday$year==2017],pch=19,
			col=colH,cex=5 )		
		arrows(Eday$doy[Eday$year==2017],
				Eday$T.L.day[Eday$year==2017]-
				Eday$T.se[Eday$year==2017],
				Eday$doy[Eday$year==2017],
				Eday$T.L.day[Eday$year==2017]+
				Eday$T.se[Eday$year==2017],lwd=3, code=0)
	
		axis(4, seq(ylT,yhT, by=.1 ), las=2, cex.axis=axisC, lwd.ticks=3)		
		mtext("2017", side=3, line=5, cex=4)	
			
	box(which="plot")

	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1), xlim=c(xl2016,xh2016), ylim=c(ylG,yhG),type="n", axes=FALSE, xlab=" ", ylab=" ",
			yaxs="i", xaxs="i")
	points(gsDave$doy[gsDave$site=="ld"&gsDave$year==2016],
		gsDave$gc.mmol.s[gsDave$site=="ld"&gsDave$year==2016],pch=19,
		col=colL,cex=5, type="b", lwd=3 )	
	points(gsDave$doy[gsDave$site=="hd"&gsDave$year==2016],
		gsDave$gc.mmol.s[gsDave$site=="hd"&gsDave$year==2016],pch=19,
		col=colH,cex=5, type="b", lwd=3 )	
	arrows(gsDave$doy[gsDave$year==2016],
				gsDave$gc.mmol.s[gsDave$year==2016]-
				gsDave$gc.se[gsDave$year==2016],
			gsDave$doy[gsDave$year==2016],
				gsDave$gc.mmol.s[gsDave$year==2016]+
				gsDave$gc.se[gsDave$year==2016],lwd=3, code=0)	
	mtext("Daily average gc", side=2, line=18, cex=4)
	mtext(expression(paste("(mmol m"^"-2","s"^"-1",")")), side=2, line=10, cex=4)			
	axis(2, seq(0, yhG-50, by=50), las=2, cex.axis=axisC, lwd.ticks=3)			
	box(which="plot")

	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1), xlim=c(xl2017,xh2017), ylim=c(ylG,yhG),type="n", axes=FALSE, xlab=" ", ylab=" ",
			yaxs="i", xaxs="i")
	points(gsDave$doy[gsDave$site=="ld"&gsDave$year==2017],
		gsDave$gc.mmol.s[gsDave$site=="ld"&gsDave$year==2017],pch=19,
		col=colL,cex=5, type="b", lwd=3 )	
	points(gsDave$doy[gsDave$site=="hd"&gsDave$year==2017],
		gsDave$gc.mmol.s[gsDave$site=="hd"&gsDave$year==2017],pch=19,
		col=colH,cex=5, type="b", lwd=3 )	
	arrows(gsDave$doy[gsDave$year==2017],
				gsDave$gc.mmol.s[gsDave$year==2017]-
				gsDave$gc.se[gsDave$year==2017],
			gsDave$doy[gsDave$year==2017],
				gsDave$gc.mmol.s[gsDave$year==2017]+
				gsDave$gc.se[gsDave$year==2017],lwd=3, code=0)	
				
	axis(4, seq(0, yhG-50, by=50), las=2, cex.axis=axisC, lwd.ticks=3)
	box(which="plot")


	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1), xlim=c(xl2016,xh2016), ylim=c(ylA,yhA),type="n", axes=FALSE, xlab=" ", ylab=" ",
			yaxs="i", xaxs="i")
	
	points(dayH$doy[dayH$doy<=xh2016&dayH$doy>=xl2016&dayH$year==2016], 
			dayH$T[dayH$doy<=xh2016&dayH$doy>=xl2016&dayH$year==2016], type="l",
			lwd=6, col=colH)
		
	points(dayL$doy[dayL$doy<=xh2016&dayL$doy>=xl2016&dayL$year==2016], 
			dayL$T[dayL$doy<=xh2016&dayL$doy>=xl2016&dayL$year==2016], type="l",
			lwd=6, col=colLt)
	axis(2, seq(0,20, by=5), las=2, cex.axis=axisC, lwd.ticks=3)
	
	points(TDall$doy[TDall$year==2016&TDall$site=="ld"],TDall$TDday[TDall$year==2016&TDall$site=="ld"]*TDscale,
			type="l", col=colL, lty=4, lwd=6)
	
		points(TDall$doy[TDall$year==2016&TDall$site=="hd"],TDall$TDday[TDall$year==2016&TDall$site=="hd"]*TDscale,
			type="l", col=colHt, lty=4, lwd=6)
	mtext("Daily average air temp", side=2, line=18, cex=4)
	mtext("(C)", side=2, line=10, cex=4)
	box(which="plot")

	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1), xlim=c(xl2017,xh2017), ylim=c(ylA,yhA),type="n", axes=FALSE, xlab=" ", ylab=" ",
			yaxs="i", xaxs="i")
	points(dayH$doy[dayH$doy<=xh2017&dayH$doy>=xl2017&dayH$year==2017], 
			dayH$T[dayH$doy<=xh2017&dayH$doy>=xl2017&dayH$year==2017], type="l",
			lwd=6, col=colH)
		
	points(dayL$doy[dayL$doy<=xh2017&dayL$doy>=xl2017&dayL$year==2017], 
			dayL$T[dayL$doy<=xh2017&dayL$doy>=xl2017&dayL$year==2017], type="l",
			lwd=6, col=colLt)
			
	points(TDall$doy[TDall$year==2017&TDall$site=="ld"],TDall$TDday[TDall$year==2017&TDall$site=="ld"]*TDscale,
			type="l", col=colL, lty=4, lwd=6)
	
	points(TDall$doy[TDall$year==2017&TDall$site=="hd"],TDall$TDday[TDall$year==2017&TDall$site=="hd"]*TDscale,
			type="l", col=colHt, lty=4, lwd=6)		
	
	axis(4, seq(0,20,by=5),seq(0,20,by=5)*3, las=2, cex.axis=axisC, lwd.ticks=3)			
	legend(165,26, c("low density TD", "high density TD", "low density Ta", "high density Ta"),
					col=c(colL,colHt,colLt,colH), lwd=6, lty=c(4,4,1,1), bty="n", cex=4)
		mtext("Thaw depth", side=4, line=10, cex=4)
	mtext("(cm)", side=4, line=18, cex=4)
	box(which="plot")

	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1), xlim=c(xl2016,xh2016), ylim=c(ylD,yhD),type="n", axes=FALSE, xlab=" ", ylab=" ",
			yaxs="i", xaxs="i")
		for(i in 1:dim(prec2016)[1]){
		polygon(c(prec2016$doy[i]-.5,prec2016$doy[i]-.5,prec2016$doy[i]+.5,prec2016$doy[i]+.5),
				c(0,prec2016$Pr.mm[i]*Prscale,prec2016$Pr.mm[i]*Prscale,0), col="grey60", border=FALSE)
		}

	
	points(dayH$doy[dayH$doy<=xh2016&dayH$doy>=xl2016&dayH$year==2016], 
			dayH$D[dayH$doy<=xh2016&dayH$doy>=xl2016&dayH$year==2016], type="l",
			lwd=6, col=colH)
		
	points(dayL$doy[dayL$doy<=xh2016&dayL$doy>=xl2016&dayL$year==2016], 
			dayL$D[dayL$doy<=xh2016&dayL$doy>=xl2016&dayL$year==2016], type="l",
			lwd=6, col=colLt)		
	axis(2, seq(0,1.2, by=.4), las=2, cex.axis=axisC, lwd.ticks=3)		
	mtext(seq(xl2016,xh2016, by=10),at=seq(xl2016,xh2016, by=10), line=4, side=1, cex=3)
	axis(1, seq(xl2016,xh2016, by=10), rep(" ", length(seq(xl2016,xh2016, by=10)))	,cex.axis=axisC, lwd.ticks=3)	
	mtext("Daily average VPD", side=2, line=18, cex=4)
	mtext("(kPa)", side=2, line=10, cex=4)
	
	box(which="plot")

	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1), xlim=c(xl2017,xh2017), ylim=c(ylD,yhD),type="n", axes=FALSE, xlab=" ", ylab=" ",
			yaxs="i", xaxs="i")
	
		for(i in 1:dim(prec2017)[1]){
		polygon(c(prec2017$doy[i]-.5,prec2017$doy[i]-.5,prec2017$doy[i]+.5,prec2017$doy[i]+.5),
				c(0,prec2017$Pr.mm[i]*Prscale,prec2017$Pr.mm[i]*Prscale,0), col="grey60", border=FALSE)
		}
	
	points(dayH$doy[dayH$doy<=xh2017&dayH$doy>=xl2017&dayH$year==2017], 
			dayH$D[dayH$doy<=xh2017&dayH$doy>=xl2017&dayH$year==2017], type="l",
			lwd=6, col=colH)
		
	points(dayL$doy[dayL$doy<=xh2017&dayL$doy>=xl2017&dayL$year==2017], 
			dayL$D[dayL$doy<=xh2017&dayL$doy>=xl2017&dayL$year==2017], type="l",
			lwd=6, col=colLt)	
	legend(165,1.6, c("low density VPD", "high density VPD", "Precipitaiton"),
					col=c(colLt, colH, "grey60"), lty=c(1,1,NA), pch=c(NA,NA,15),
					bty="n", lwd=c(6,6,NA), cex=4)
	axis(4,seq(0,1.2, by=.4),seq(0,1.2, by=.4)*25,  las=2, cex.axis=axisC, lwd.ticks=3)		
	
	mtext(seq(xl2017,xh2017, by=10),at=seq(xl2017,xh2017, by=10), line=4, side=1, cex=3)
	axis(1, seq(xl2017,xh2017, by=10), rep(" ", length(seq(xl2017,xh2017, by=10)))	,cex.axis=axisC, lwd.ticks=3)
	mtext("Precipitation", side=4, line=10, cex=4)
	mtext("(mm)", side=4, line=18, cex=4)
	mtext("Day of year", side=1, outer=TRUE, line=-3, cex=4)
	box(which="plot")

dev.off()

#################################################################
####make a panel of subset of half hourly                 #######      
####met and T and gc calc                                 #######
#################################################################