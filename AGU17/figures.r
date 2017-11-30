###########################################################################
###########################################################################
############## figures for AGU presentation                  ##############
##############                                               ##############
###########################################################################
###########################################################################
###########################################################################
############## Input files:                                  ##############
############## from sapflux calc:                            ##############
############## Transpiration: El.L,El.L17,El.H,El.H17        ##############
############## stomatal conductance:gc.L, gc.L17, gc.H, gc.H17#############
############## tree info: datTreeL, datTreeL17, datTreeH,     #############
##############            datTreeH17                          #############
############## from thaw depth:                               #############
##############  TDall: by stand and year                      #############
###########################################################################
###########################################################################


#################################################################
####read in sapflow data                                  #######
#################################################################
source("c:\\Users\\hkropp\\Documents\\GitHub\\larch_density_ecohydro\\sapflux_process.r")
#libraries loaded from source
#plyr, lubridate,caTools
#################################################################
####read in thawdepth data                                #######
#################################################################
source("c:\\Users\\hkropp\\Documents\\GitHub\\larch_density_ecohydro\\thaw_depth_process.r")

dirP <- "c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\AGU17"
# airport pressure and precip data
datAirP <- read.csv("c:\\Users\\hkropp\\Google Drive\\viperSensor\\airport\\airport.csv")

#canopy rh and temperature
datRH <- read.csv("c:\\Users\\hkropp\\Google Drive\\viperSensor\\met\\RH.VP4.csv")
datTC <- read.csv("c:\\Users\\hkropp\\Google Drive\\viperSensor\\met\\TempC.VP4.csv")

#PAR
datPAR <- read.csv("c:\\Users\\hkropp\\Google Drive\\viperSensor\\met\\PAR.QSOS PAR.csv")

#################################################################
####organize met data                                     #######
#################################################################
#subset and match
datLRHmet <- data.frame(datRH[datRH$site=="ld",1:3], RH=datRH$RH.VP4[datRH$site=="ld"])
datLTCmet <- data.frame(datTC[datTC$site=="ld",1:3], Temp=datTC$TempC.VP4[datTC$site=="ld"])

datHRHmet <- data.frame(datRH[datRH$site=="hd",1:3], RH=datRH$RH.VP4[datRH$site=="hd"])
datHTCmet <- data.frame(datTC[datTC$site=="hd",1:3], Temp=datTC$TempC.VP4[datTC$site=="hd"])
#join temp and RH
datLmet <- join(datLRHmet, datLTCmet, by=c("doy","year","hour"),type="inner")
datHmet <- join(datHRHmet, datHTCmet, by=c("doy","year","hour"),type="inner")

#join into gc tables
datgcL <- join(gc.L,datLmet, by=c("doy","year","hour"), type="left")
datgcH <- join(gc.H,datHmet, by=c("doy","year","hour"), type="left")
datgcL17 <- join(gc.L17,datLmet, by=c("doy","year","hour"), type="left")
datgcH17 <- join(gc.H17,datHmet, by=c("doy","year","hour"), type="left")

#join into T tables
datEL <- join(El.L,datLmet, by=c("doy","year","hour"), type="left")
datEH <- join(El.H,datHmet, by=c("doy","year","hour"), type="left")
datEL17 <- join(El.L17,datLmet, by=c("doy","year","hour"), type="left")
datEH17 <- join(El.H17,datHmet, by=c("doy","year","hour"), type="left")

######calculate VPD##########
#calculate saturated vapor pressure
datLe.sat<-0.611*exp((17.502*datgcL$Temp)/(datgcL$Temp+240.97))
datLe.sat17<-0.611*exp((17.502*datgcL17$Temp)/(datgcL17$Temp+240.97))
datHe.sat17<-0.611*exp((17.502*datgcH17$Temp)/(datgcH17$Temp+240.97))
datHe.sat<-0.611*exp((17.502*datgcH$Temp)/(datgcH$Temp+240.97))

#calculate vapor pressure deficit
#here rh is is in decimal form 
datgcL$RHfix<-ifelse(datgcL$RH>=1,.999,datgcL$RH)
datgcL17$RHfix<-ifelse(datgcL17$RH>=1,.999,datgcL17$RH)
datgcH$RHfix<-ifelse(datgcH$RH>=1,.999,datgcH$RH)
datgcH17$RHfix<-ifelse(datgcH17$RH>=1,.999,datgcH17$RH)

#vpd
datgcL$D<-(datLe.sat-(datgcL$RHfix*datLe.sat))
datgcL17$D<-(datLe.sat17-(datgcL17$RHfix*datLe.sat17))
datgcH17$D<-(datHe.sat17-(datgcH17$RHfix*datHe.sat17))
datgcH$D<-(datHe.sat-(datgcH$RHfix*datHe.sat))


#join PAR to the dataframes
datPARL <- data.frame(doy=datPAR$doy[datPAR$site=="ld"], year=datPAR$year[datPAR$site=="ld"],
						hour=datPAR$hour[datPAR$site=="ld"], PAR=datPAR$PAR.QSOS.Par[datPAR$site=="ld"])
datPARH <- data.frame(doy=datPAR$doy[datPAR$site=="hd"], year=datPAR$year[datPAR$site=="hd"],
						hour=datPAR$hour[datPAR$site=="hd"], PAR=datPAR$PAR.QSOS.Par[datPAR$site=="hd"])						
#join PAR to gc
datgcL <- join(datgcL, datPARL, by=c("doy", "year","hour"), type="inner")						
datgcL17 <- join(datgcL17, datPARL, by=c("doy", "year","hour"), type="inner")					
datgcH <- join(datgcH, datPARH, by=c("doy", "year","hour"), type="inner")
datgcH17 <- join(datgcH17, datPARH, by=c("doy", "year","hour"), type="inner")

#join precip data to check for filtering
datPrecip <- data.frame(doy=datAirP$doy, year=datAirP$year,Pr.mm = datAirP$Pr.mm)

datgcL <- join(datgcL, datPrecip, by=c("doy", "year"), type="left")						
datgcL17 <- join(datgcL17, datPrecip, by=c("doy", "year"), type="left")					
datgcH <- join(datgcH, datPrecip, by=c("doy", "year"), type="left")
datgcH17 <- join(datgcH17, datPrecip, by=c("doy", "year"), type="left")

#################################################################
####gc filter                                             #######
#################################################################
#light more than 5 umol m-2 s-1 (to exclude nocturnal measurements that may include refilling)
#D>= 0.6kPA (per Ewers and Oren 2000)
#precip more than 1mm

m.gcL <- datgcL[datgcL$D>=.6&datgcL$PAR>5&datgcL$Pr.mm<=1,]
m.gcL17 <- datgcL17[datgcL17$D>=.6&datgcL17$PAR>5&datgcL17$Pr.mm<=1,]
m.gcH <- datgcH[datgcH$D>=.6&datgcH$PAR>5&datgcH$Pr.mm<=1,]
m.gcH17 <- datgcH17[datgcH17$D>=.6&datgcH17$PAR>5&datgcH17$Pr.mm<=1,]


#################################################################
####organize gc  data                                     #######
#################################################################
#stand ID 1=low density, 2=high density
#organize the data by sensor and stand
#and omit missing sensor measurements
tgcL <- list()
for(i in 1:dim(datTreeL)[1]){
	tgcL[[i]] <- na.omit(data.frame(doy=m.gcL$doy, 
					year=m.gcL$year, hour=m.gcL$hour,
					g.c=m.gcL[,i+3], D=m.gcL$D,PAR=m.gcL$PAR,treeID.new=rep(datTreeL$treeID.new[i], dim(m.gcL)[1]),
					stand=rep(1, dim(m.gcL)[1])))
}

tgcL17 <- list()
for(i in 1:dim(datTreeL17)[1]){
	tgcL17[[i]] <- na.omit(data.frame(doy=m.gcL17$doy, 
					year=m.gcL17$year, hour=m.gcL17$hour,
					g.c=m.gcL17[,i+3], D=m.gcL17$D,PAR=m.gcL17$PAR,treeID.new=rep(datTreeL17$treeID.new[i], dim(m.gcL17)[1]),
					stand=rep(1, dim(m.gcL17)[1])))
}

tgcH <- list()
for(i in 1:dim(datTreeH)[1]){
	tgcH[[i]] <- na.omit(data.frame(doy=m.gcH$doy, 
					year=m.gcH$year, hour=m.gcH$hour,
					g.c=m.gcH[,i+3], D=m.gcH$D,PAR=m.gcH$PAR,treeID.new=rep(datTreeH$treeID.new[i], dim(m.gcH)[1]),
					stand=rep(2, dim(m.gcH)[1])))
}
tgcH17 <- list()
for(i in 1:dim(datTreeH17)[1]){
	tgcH17[[i]] <- na.omit(data.frame(doy=m.gcH17$doy, 
					year=m.gcH17$year, hour=m.gcH17$hour,
					g.c=m.gcH17[,i+3], D=m.gcH17$D,PAR=m.gcH17$PAR,treeID.new=rep(datTreeH17$treeID.new[i], dim(m.gcH17)[1]),
					stand=rep(2, dim(m.gcH17)[1])))
}


#now turn into single data frame
lowGC <-ldply(tgcL, data.frame)
lowGC17 <-ldply(tgcL17, data.frame)
highGC <-ldply(tgcH, data.frame)
highGC17 <-ldply(tgcH17, data.frame)

gcALL <- rbind(lowGC,lowGC17,highGC,highGC17)

#aggregate to by stand
gcALLag <- aggregate(gcALL$g.c, by=list(gcALL$hour,gcALL$doy,gcALL$year,gcALL$stand), FUN="mean")
gcALLsd <- aggregate(gcALL$g.c, by=list(gcALL$hour,gcALL$doy,gcALL$year,gcALL$stand), FUN="sd")
gcALLn <- aggregate(gcALL$g.c, by=list(gcALL$hour,gcALL$doy,gcALL$year,gcALL$stand), FUN="length")
#doesn't need to be averaged since the same but just for matching
gcALLagD <- aggregate(gcALL$D, by=list(gcALL$hour,gcALL$doy,gcALL$year,gcALL$stand), FUN="mean")
gcALLagP <- aggregate(gcALL$PAR, by=list(gcALL$hour,gcALL$doy,gcALL$year,gcALL$stand), FUN="mean")
colnames(gcALLag) <- c("hour","doy","year","stand","g.c")
gcALLag$D <- gcALLagD$x
gcALLag$PAR <- gcALLagP$x
gcALLag$sd <-gcALLsd$x
gcALLag$n <-gcALLn$x
gcALLag$se <-gcALLsd$x/sqrt(gcALLn$x)

#eliminate half hours where there weren't at least 3 measurements in a stand
gcALLsamp <-gcALLag[gcALLn$x>=3,]


#################################################################
####organize T  data                                      #######
#################################################################

tEL <- list()
for(i in 1:dim(datTreeL)[1]){
	tEL[[i]] <- na.omit(data.frame(doy=datEL$doy, 
					year=datEL$year, hour=datEL$hour,
					E=datEL[,i+3], RH=datEL$RH,Temp=datEL$Temp,
					stand=rep(1, dim(datEL)[1]),sensor=rep(i, dim(datEL)[1])))
}

tEL17 <- list()
for(i in 1:dim(datTreeL17)[1]){
	tEL17[[i]] <- na.omit(data.frame(doy=datEL17$doy, 
					year=datEL17$year, hour=datEL17$hour,
					E=datEL17[,i+3], RH=datEL17$RH,Temp=datEL17$Temp,
					stand=rep(1, dim(datEL17)[1]),sensor=rep(i, dim(datEL17)[1])))
}


tEH <- list()
for(i in 1:dim(datTreeH)[1]){
	tEH[[i]] <- na.omit(data.frame(doy=datEH$doy, 
					year=datEH$year, hour=datEH$hour,
					E=datEH[,i+3], RH=datEH$RH,Temp=datEH$Temp,
					stand=rep(2, dim(datEH)[1]),sensor=rep(i, dim(datEH)[1])))
}
tEH17 <- list()
for(i in 1:dim(datTreeH17)[1]){
	tEH17[[i]] <- na.omit(data.frame(doy=datEH17$doy, 
					year=datEH17$year, hour=datEH17$hour,
					E=datEH17[,i+3], RH=datEH17$RH,Temp=datEH17$Temp,
					stand=rep(2, dim(datEH17)[1]),sensor=rep(i, dim(datEH17)[1])))
}

#now turn into single data frame
lowE <-ldply(tEL, data.frame)
lowE17 <-ldply(tEL17, data.frame)
highE <-ldply(tEH, data.frame)
highE17 <-ldply(tEH17, data.frame)

EALLf <- rbind(lowE,lowE17,highE,highE17)



#calculate VPD

EALLf$e.sat <- 0.611*exp((17.502*EALLf$Temp)/(EALLf$Temp+240.97))
EALLf$RHfix <- ifelse(EALLf$RH>=1,.999,EALLf$RH)
EALLf$D <- (EALLf$e.sat -(EALLf$RHfix*EALLf$e.sat ))

#filter precip out
EALLf <- join(EALLf, datPrecip, by=c("doy","year"), type="left")
EALLf <- EALLf[EALLf$Pr.mm<=1,]

#################################################################
####aggregate T to stand half hourly                      #######
#################################################################
Ehh <- aggregate(EALLf$E, by=list(EALLf$hour,EALLf$doy,EALLf$year, EALLf$stand), FUN="mean")
Ehhsd <-aggregate(EALLf$E, by=list(EALLf$hour,EALLf$doy,EALLf$year, EALLf$stand), FUN="sd")
Ehhn <- aggregate(EALLf$E, by=list(EALLf$hour, EALLf$doy,EALLf$year,EALLf$stand), FUN="length")
colnames(Ehh) <- c("hour","doy","year","stand","Ehh")
Ehh$sd <- Ehhsd$x
Ehh$n <- Ehhn$x
Ehh$se <- Ehhsd$x/sqrt(Ehhn$x)
Ehh <- Ehh[Ehh$n>=3,]

#################################################################
####daily total T                                         #######
#################################################################

#get days with only 4 missing observations and can fill in the rest
#with interpolation
Ecount <- aggregate(EALLf$E, by=list(EALLf$doy,EALLf$year,EALLf$stand,EALLf$sensor), FUN="length")
Ecount <- Ecount[Ecount$x>=44,]
colnames(Ecount) <- c("doy", "year","stand","sensor","n")

#now see how many trees in each day 
Ecount2 <- aggregate(Ecount$n, by=list(Ecount$doy,Ecount$year,Ecount$stand), FUN="length")
Ecount2 <-Ecount2[Ecount2$x>=3,]
colnames(Ecount2) <- c("doy","year","stand","n")


dayUse <- join(Ecount, Ecount2, by=c("doy","year","stand"), type="inner")
#subset to only grab the days that have enough observations

EALLs1 <- join(EALLf, Ecount2, by=c("doy","year","stand"), type="inner")
#fill in the data missing from the spikes
dayUseFill <- data.frame(doy=rep(dayUse$doy, each=48),year=rep(dayUse$year, each=48),
						stand=rep(dayUse$stand, each=48),sensor=rep(dayUse$sensor, each=48),
						hour=rep(seq(0,23.5,by=.5),times=dim(dayUse)[1]))

EALLs2 <- join(EALLs1,dayUseFill, by=c("doy","year","stand","sensor","hour"), type="right")
EALLs <- EALLs2
library(zoo)
EALLs$E.f<-na.approx(EALLs2$E)


#convert E from g m-2 s-1 to g m-2 half hour -1
EALLs$E.hh <- EALLs$E.f*60*30

#organize and do a trapezoidal integration
HTx <- matrix(rep(NA,dim(dayUse)[1]*48), ncol=dim(dayUse)[1])
HTy <- matrix(rep(NA,dim(dayUse)[1]*48), ncol=dim(dayUse)[1])
HTday <- numeric(0)

for(i in 1:dim(dayUse)[1]){
	HTx[,i] <- EALLs$hour[EALLs$doy==dayUse$doy[i]&EALLs$year==dayUse$year[i]&EALLs$stand==dayUse$stand[i]&EALLs$sensor==dayUse$sensor[i]]
	HTy[,i] <- EALLs$E.hh[EALLs$doy==dayUse$doy[i]&EALLs$year==dayUse$year[i]&EALLs$stand==dayUse$stand[i]&EALLs$sensor==dayUse$sensor[i]]
	HTday[i] <- trapz(HTx[,i],HTy[,i])
}

#convert to L per m2 day
#1 g is cm3 which is 1 mL so (1/1000) to be L
	LTday<-HTday/1000
dayT <- dayUse[,1:4]
dayT$T.L <-LTday


#now aggregate across sensors
LTdaily <- aggregate(dayT$T.L, by=list(dayT$doy, dayT$year,dayT$stand), FUN="mean")
LTdailySD <- aggregate(dayT$T.L, by=list(dayT$doy, dayT$year,dayT$stand), FUN="sd")
LTdailyn <- aggregate(dayT$T.L, by=list(dayT$doy, dayT$year,dayT$stand), FUN="length")
colnames(LTdaily) <- c("doy","year","stand","T.L")
LTdaily$sd <- LTdailySD$x
LTdaily$n <- LTdailyn$x
LTdaily$se <- LTdailySD$x/sqrt(LTdailyn$x)





#################################################################
####plot daily T  data                                    #######
#################################################################
wd <- 30
hd <- 30
xl16 <- 180
xh16 <- 245
xl17 <- 155
xh17 <- 225
yl <-0
yh <- .35
colL <- "royalblue3"
colH <- "tomato3"
xseq2016 <- seq(xl16, xh16, by=10)
yseq <- seq(yl,yh, by=.1)
xseq2017 <- seq(xl17, xh17, by=10)
hhyl <- 0
hhyh <- .015
hhyseq <- seq(hhyl,hhyh, by=.01)
hhxl16 <-185
hhxh16 <- 200
hhxl17 <-160
hhxh17 <- 182
jpeg(paste0(dirP , "\\transpiraiton_new_filter.jpg"), width=2800, height=2000, units="px", quality=100)
	ab <- layout(matrix(seq(1,4), ncol=2, byrow=TRUE), width=rep(lcm(wd),4), height=rep(lcm(hd),4))
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1),type="n", ylim=c(hhyl,hhyh), xlim=c(hhxl16,hhxh16), xlab=" ", ylab=" ", axes=FALSE, xaxs="i",
			yaxs="i")	
	points(Ehh$doy[Ehh$stand==1&Ehh$year==2016]+(Ehh$hour[Ehh$stand==1&Ehh$year==2016]/24),
		Ehh$Ehh[Ehh$stand==1&Ehh$year==2016],cex=3, pch=19, col=colL, type="b")
	points(Ehh$doy[Ehh$stand==2&Ehh$year==2016]+(Ehh$hour[Ehh$stand==2&Ehh$year==2016]/24),
		Ehh$Ehh[Ehh$stand==2&Ehh$year==2016],cex=3, pch=19, col=colH, type="b")	
	#arrows(Ehh$doy[Ehh$year==2016]+(Ehh$hour[Ehh$year==2016]/24),
	#	Ehh$Ehh[Ehh$year==2016]+Ehh$se[Ehh$year==2016],
	#	Ehh$doy[Ehh$year==2016]+(Ehh$hour[Ehh$year==2016]/24),
	#	Ehh$Ehh[Ehh$year==2016]+Ehh$se[Ehh$year==2016],code=0,lwd=3)	
	axis(2,hhyseq, lwd.ticks=3, cex.axis=3,las=2 )
	box(which="plot")
	
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1),type="n", ylim=c(hhyl,hhyh), xlim=c(hhxl17,hhxh17), xlab=" ", ylab=" ", axes=FALSE, xaxs="i",
			yaxs="i")	
	points(Ehh$doy[Ehh$stand==1&Ehh$year==2017]+(Ehh$hour[Ehh$stand==1&Ehh$year==2017]/24),
		Ehh$Ehh[Ehh$stand==1&Ehh$year==2017],cex=3, pch=19, col=colL, type="b")
	points(Ehh$doy[Ehh$stand==2&Ehh$year==2017]+(Ehh$hour[Ehh$stand==2&Ehh$year==2017]/24),
		Ehh$Ehh[Ehh$stand==2&Ehh$year==2017],cex=3, pch=19, col=colH, type="b")	
	#arrows(Ehh$doy[Ehh$year==2017]+(Ehh$hour[Ehh$year==2017]/24),
	#	Ehh$Ehh[Ehh$year==2017]+Ehh$se[Ehh$year==2017],
	#	Ehh$doy[Ehh$year==2017]+(Ehh$hour[Ehh$year==2017]/24),
	#	Ehh$Ehh[Ehh$year==2017]+Ehh$se[Ehh$year==2017],code=0,lwd=3)	
	axis(4,hhyseq, lwd.ticks=3, cex.axis=3,las=2 )	
	box(which="plot")	
	
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1),type="n", ylim=c(yl,yh), xlim=c(xl16,xh16), xlab=" ", ylab=" ", axes=FALSE, xaxs="i",
			yaxs="i")
		points(LTdaily$doy[LTdaily$stand==1&LTdaily$year==2016], 
			LTdaily$T.L[LTdaily$stand==1&LTdaily$year==2016], pch=19, col=colL,cex=3 )
		points(LTdaily$doy[LTdaily$stand==2&LTdaily$year==2016], 
			LTdaily$T.L[LTdaily$stand==2&LTdaily$year==2016], pch=19, col=colH,cex=3 )	
		arrows(LTdaily$doy[LTdaily$year==2016],
			LTdaily$T.L[LTdaily$year==2016]-LTdaily$se[LTdaily$year==2016],
			LTdaily$doy[LTdaily$year==2016],
			LTdaily$T.L[LTdaily$year==2016]+LTdaily$se[LTdaily$year==2016],code=0,lwd=3)
	axis(1, xseq2016, lab=rep(" ", length(xseq2016)), lwd.ticks=3, cex.axis=3)
	axis(2,yseq, lwd.ticks=3, cex.axis=3,las=2 )
	box(which="plot")

	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1),type="n", ylim=c(yl,yh), xlim=c(xl17,xh17), xlab=" ", ylab=" ", axes=FALSE, xaxs="i",
			yaxs="i")
		points(LTdaily$doy[LTdaily$stand==1&LTdaily$year==2017], 
			LTdaily$T.L[LTdaily$stand==1&LTdaily$year==2017], pch=19, col=colL,cex=3 )
		points(LTdaily$doy[LTdaily$stand==2&LTdaily$year==2017], 
			LTdaily$T.L[LTdaily$stand==2&LTdaily$year==2017], pch=19, col=colH,cex=3 )	
		arrows(LTdaily$doy[LTdaily$year==2017],
			LTdaily$T.L[LTdaily$year==2017]-LTdaily$se[LTdaily$year==2017],
			LTdaily$doy[LTdaily$year==2017],
			LTdaily$T.L[LTdaily$year==2017]+LTdaily$se[LTdaily$year==2017],code=0,lwd=3)
	axis(1, xseq2017, lab=rep(" ", length(xseq2017)), lwd.ticks=3, cex.axis=2)
	axis(4,yseq, lwd.ticks=3, cex.axis=3,las=2 )
	box(which="plot")
dev.off()	
	