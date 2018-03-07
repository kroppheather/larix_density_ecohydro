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

#gc and model results
##### have to change based on updated model results
#read in stand day data

daySD <- read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\gc_model\\run34\\out\\standDay.csv")
datgc <-read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\gc_model\\run34\\out\\gcdata.csv")

datM <- read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\gc_model\\run34\\out\\mod_stats.csv")
datQ <- read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\gc_model\\run34\\out\\mod_quants.csv")

#pull out model S, d, and gref paramters for 

#stand index of 1 is low
#################################################################
####read in parameters                                    #######
#################################################################
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
datlslope <- datC[datC$parms3=="lslope",]
datrep <- datC[datC$parms3=="repgs",]
datmugs<- datC[datC$parms3=="mugs",]

#add ind

colnames(datS) <- paste0(colnames(datS), "S")
colnames(datgref) <- paste0(colnames(datgref), "G")
colnames(datlslope) <- paste0(colnames(datlslope), "L")

datRparm <- cbind(datgref,datS)
datRparm <- cbind(datRparm, datlslope)
datRparm <- cbind(datRparm, daySD)
#join the 
datparm <- datC[datC$parms3=="a"|datC$parms3=="b"|datC$parms3=="d",]



datparm$stand <- rep(c(1,2), times=12)
datparm$pN <- rep(rep(c(1,2,3,4),each=2),times=3)
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

#calculate D at the met level
#calculate saturated vapor pressure
datLmet$e.sat<-0.611*exp((17.502*datLmet$Temp)/(datLmet$Temp+240.97))
datHmet$e.sat<-0.611*exp((17.502*datHmet$Temp)/(datHmet$Temp+240.97))
#here rh is is in decimal form 
datLmet$RHfix<-ifelse(datLmet$RH>=1,.999,datLmet$RH)
datHmet$RHfix<-ifelse(datHmet$RH>=1,.999,datHmet$RH)
#vpd
datLmet$D<-(datLmet$e.sat-(datLmet$RHfix*datLmet$e.sat))
datHmet$D<-(datHmet$e.sat-(datHmet$RHfix*datHmet$e.sat))

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
EhhD <- aggregate(EALLf$D, by=list(EALLf$hour, EALLf$doy,EALLf$year,EALLf$stand), FUN="mean")
colnames(Ehh) <- c("hour","doy","year","stand","Ehh")
Ehh$sd <- Ehhsd$x
Ehh$n <- Ehhn$x
Ehh$se <- Ehhsd$x/sqrt(Ehhn$x)
Ehh$D <- EhhD$x
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
####plot half hourly T  data                              #######
#################################################################
#join par and D 


wd <- 45
hd <- 32

xl <- 198
xh <- 202
ys <- 2016
xl2 <- 171
xh2 <- 175
ys2 <- 2017

yl <-0
yh <- .011
ylD<- 0
yhD <- 2.5
ylP<- 0
yhP <- 1500
x.off <- .1
colL <- "royalblue3"
colH <- "tomato3"
xseq <- seq(xl, xh-1, by=1)
xseq2 <- seq(xl2, xh2, by=1)
yseq <- seq(yl,yh, by=.002)
yseqD <- seq(ylD, yhD-.5, by=.5)
yseqP<- seq(ylP, yhP-300, by=300)
bl <-3

jpeg(paste0(dirP , "\\transpiraiton_hh.jpg"), width=3600, height=3200, units="px", quality=100)

	ab <- layout(matrix(seq(1,6), ncol=2, byrow=FALSE), width=rep(lcm(wd),6), height=rep(lcm(hd),6))
	
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1),type="n", ylim=c(yl,yh), xlim=c(xl-x.off,xh+x.off), xlab=" ", ylab=" ", axes=FALSE, xaxs="i",
			yaxs="i")	
	
	for(i in 1:(xh-xl)){
	points(Ehh$doy[Ehh$stand==1&Ehh$year==ys&Ehh$doy==((xl-1)+i)]+(Ehh$hour[Ehh$stand==1&Ehh$year==ys&Ehh$doy==((xl-1)+i)]/24)
			,Ehh$Ehh[Ehh$stand==1&Ehh$year==ys&Ehh$doy==((xl-1)+i)],pch=19,type="b",  col=colL,cex=9,lwd=7)
	points(Ehh$doy[Ehh$stand==2&Ehh$year==ys&Ehh$doy==((xl-1)+i)]+(Ehh$hour[Ehh$stand==2&Ehh$year==ys&Ehh$doy==((xl-1)+i)]/24)
			,Ehh$Ehh[Ehh$stand==2&Ehh$year==ys&Ehh$doy==((xl-1)+i)],pch=19, type="b",  col=colH,cex=9,lwd=7)
	arrows(Ehh$doy[Ehh$year==ys&Ehh$doy==((xl-1)+i)]+(Ehh$hour[Ehh$year==ys&Ehh$doy==((xl-1)+i)]/24),
		Ehh$Ehh[Ehh$year==ys&Ehh$doy==((xl-1)+i)]-Ehh$se[Ehh$year==ys&Ehh$doy==((xl-1)+i)],
		Ehh$doy[Ehh$year==ys&Ehh$doy==((xl-1)+i)]+(Ehh$hour[Ehh$year==ys&Ehh$doy==((xl-1)+i)]/24),
		Ehh$Ehh[Ehh$year==ys&Ehh$doy==((xl-1)+i)]+Ehh$se[Ehh$year==ys&Ehh$doy==((xl-1)+i)],code=0,lwd=3,col=rgb(0/255,0/255,0/255,.6))
	}
	axis(2, yseq,yseq*1000, cex.axis=9, las=2, lwd.ticks=4)
	mtext("Canopy", side=2, line=45,cex=8)
	mtext("Transpiration", side=2, line=32,cex=8)
	mtext(expression(paste("(mg m"^"-2"~"s"^"-1"~")")), side=2, line=18,cex=8)
	mtext("2016", side=3, line=5, cex=8)
	box(which="plot", lwd=bl)
	
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1),type="n", ylim=c(ylD,yhD), xlim=c(xl-x.off,xh+x.off), xlab=" ", ylab=" ", axes=FALSE, xaxs="i",
			yaxs="i")	
	points(datLmet$doy[datLmet$year==ys]+(datLmet$hour[datLmet$year==ys]/24),datLmet$D[datLmet$year==ys], type="l", col=colL, lwd=12)
	points(datHmet$doy[datHmet$year==ys]+(datHmet$hour[datHmet$year==ys]/24),datHmet$D[datHmet$year==ys], type="l", col=colH, lwd=12)
	
	axis(2, yseqD, cex.axis=9, las=2, lwd.ticks=4)
	mtext("Vapor pressure", side=2, line=45,cex=8)
	mtext("deficit", side=2, line=32,cex=8)
	mtext("(KPa)", side=2, line=19,cex=8)
	
	box(which="plot", lwd=bl)
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1),type="n", ylim=c(ylP,yhP), xlim=c(xl-x.off,xh+x.off), xlab=" ", ylab=" ", axes=FALSE, xaxs="i",
			yaxs="i")		
	points(datPARL$doy[datPARL$year==ys]+(datPARL$hour[datPARL$year==ys]/24), datPARL$PAR[datPARL$year==ys], type="l", col=colL, lwd=12)
	points(datPARH$doy[datPARH$year==ys]+(datPARH$hour[datPARH$year==ys]/24), datPARH$PAR[datPARH$year==ys], type="l", col=colH, lwd=12)
	axis(2, yseqP, cex.axis=9, las=2, lwd.ticks=4)
	axis(1, xseq,rep(" ", length(xseq)), cex.axis=9, las=2, lwd.ticks=4)
	mtext("Photosynthetically", side=2, line=45,cex=8)
	mtext("active radiation", side=2, line=32,cex=8)
	mtext(expression(paste("("~mu~"mol m"^"-2"~"s"^"-1"~")")), side=2, line=18,cex=8)
	mtext(xseq, at=xseq,side=1, line=8, cex=6)
	box(which="plot", lwd=bl)
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1),type="n", ylim=c(yl,yh), xlim=c(xl2,xh2), xlab=" ", ylab=" ", axes=FALSE, xaxs="i",
			yaxs="i")	
	for(i in 1:(xh2-xl2)){
	points(Ehh$doy[Ehh$stand==1&Ehh$year==ys2&Ehh$doy==((xl2-1)+i)]+(Ehh$hour[Ehh$stand==1&Ehh$year==ys2&Ehh$doy==((xl2-1)+i)]/24),
	Ehh$Ehh[Ehh$stand==1&Ehh$year==ys2&Ehh$doy==((xl2-1)+i)],type="b",pch=19,  col=colL,cex=9,lwd=7)
	points(Ehh$doy[Ehh$stand==2&Ehh$year==ys2&Ehh$doy==((xl2-1)+i)]+(Ehh$hour[Ehh$stand==2&Ehh$year==ys2&Ehh$doy==((xl2-1)+i)]/24),
	Ehh$Ehh[Ehh$stand==2&Ehh$year==ys2&Ehh$doy==((xl2-1)+i)],pch=19 ,type="b", col=colH,cex=9,lwd=7)
	arrows(Ehh$doy[Ehh$year==ys2&Ehh$doy==((xl2-1)+i)]+(Ehh$hour[Ehh$year==ys2&Ehh$doy==((xl2-1)+i)]/24),
		Ehh$Ehh[Ehh$year==ys2&Ehh$doy==((xl2-1)+i)]-Ehh$se[Ehh$year==ys2&Ehh$doy==((xl2-1)+i)],
		Ehh$doy[Ehh$year==ys2&Ehh$doy==((xl2-1)+i)]+(Ehh$hour[Ehh$year==ys2&Ehh$doy==((xl2-1)+i)]/24),
		Ehh$Ehh[Ehh$year==ys2&Ehh$doy==((xl2-1)+i)]+Ehh$se[Ehh$year==ys2&Ehh$doy==((xl2-1)+i)],code=0,lwd=3,col=rgb(0/255,0/255,0/255,.6))
	}
	legend(xl2+.2,yh, c("low density","high density", "se"), col=c(colL,colH,rgb(0/255,0/255,0/255,.6)), pch=c(19,19,NA),lwd=c(3,3,3), cex=9,bty="n")
	mtext("2017", side=3, line=5, cex=8)
	box(which="plot", lwd=bl)
	
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1),type="n", ylim=c(ylD,yhD), xlim=c(xl2,xh2), xlab=" ", ylab=" ", axes=FALSE, xaxs="i",
			yaxs="i")	
	points(datLmet$doy[datLmet$year==ys2]+(datLmet$hour[datLmet$year==ys2]/24),datLmet$D[datLmet$year==ys2], type="l", col=colL, lwd=12)
	points(datHmet$doy[datHmet$year==ys2]+(datHmet$hour[datHmet$year==ys2]/24),datHmet$D[datHmet$year==ys2], type="l", col=colH, lwd=12)
	box(which="plot", lwd=bl)
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1),type="n", ylim=c(ylP,yhP), xlim=c(xl2,xh2), xlab=" ", ylab=" ", axes=FALSE, xaxs="i",
			yaxs="i")		
	points(datPARL$doy[datPARL$year==ys2]+(datPARL$hour[datPARL$year==ys2]/24), datPARL$PAR[datPARL$year==ys2], type="l", col=colL, lwd=12)
	points(datPARH$doy[datPARH$year==ys2]+(datPARH$hour[datPARH$year==ys2]/24), datPARH$PAR[datPARH$year==ys2], type="l", col=colH, lwd=12)
	axis(1, xseq2,rep(" ", length(xseq2)), cex.axis=9, las=2, lwd.ticks=4)
	mtext(xseq2, at=xseq2,side=1, line=8, cex=6)
	mtext("Day of year", side=1, outer=TRUE, line=-5, cex=8)
	box(which="plot", lwd=bl)	
	
dev.off()	
	


#################################################################
####gc model                                              #######
#################################################################
#68,
sDsub <- 3

datPP <- datRparm[datRparm$standDay==sDsub,]
datGP <- datgc[datgc$standDay==sDsub,]

#declare plot variables

wd <- 45
hd <- 32
yl <- 0
yh <- 35
xlD <- 0.5
xhD <- 2
xlP <- 0
xhP <- 600
colD <- "royalblue3"
colP <- "tomato3"
Dseq <- seq(0.6,1.8, by=.2)
Pseq <- seq(0,500, by =100)
Gseq <- seq(0,30, by=10)
bl <-2

ParFunc <- function(gref,PAR,b){
	gref*(1-exp(-b*PAR))

}

DFunc <- function(gref,D,S){
	gref*(1-S*log(D))

}
#look at when PAR is saturated

Pchange <- round_any(log(.2)/-datPP$MeanL,100)

colP <- ifelse(datGP$PAR<=Pchange, "black","grey57")
colD <- ifelse(datGP$PAR>=Pchange, "black","grey57")

#make a panel of 2
jpeg(paste0(dirP , "\\gc_response.jpg"), width=3200, height=1500, units="px", quality=100)
	#
	ab <- layout(matrix(seq(1,2), ncol=2, byrow=FALSE), width=rep(lcm(wd),2), height=rep(lcm(hd),2))
	
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1),type="n", ylim=c(yl,yh), xlim=c(xlP-1,xhP), xlab=" ", ylab=" ", axes=FALSE, xaxs="i",
			yaxs="i")
	points(datGP$PAR,datGP$g.c, pch=19, cex=5, col=colP)
	points(seq(1,xhP, by=.1), ParFunc(datPP$MeanG,seq(1,xhP, by=.1),datPP$MeanL), lwd=5)
	
	axis(1, Pseq,rep(" ", length(Pseq)), cex.axis=3, las=2, lwd.ticks=4)
	axis(2,Gseq, cex.axis=5, las=2, lwd.ticks=4)
	
	mtext( Pseq, at= Pseq,side=1, line=5, cex=5)
	mtext("Canopy stomatal conductance", side=2, line=16, cex=6)
	mtext(expression(paste("(mmol m"^"-2","s"^"-1",")")),, side=2, line=8, cex=6  )
	mtext("Photosynthetically active radiation", side=1, line=11, cex=6)
	mtext(expression(paste("(",mu,"mol m"^"-2","s"^"-1",")")), side=1, line=18,cex=6)
	box(which="plot", lwd=bl)
	par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1),type="n", ylim=c(yl,yh), xlim=c(xlD,xhD), xlab=" ", ylab=" ", axes=FALSE, xaxs="i",
			yaxs="i")
	points(datGP$D,datGP$g.c, pch=19, cex=5, col=colD)
	points(seq(.6,xhD, by=.01), DFunc(datPP$MeanG,seq(.6,xhD, by=.01),datPP$MeanS), type="l",lwd=5, lty=1)
	axis(1, Dseq,rep(" ", length( Dseq)), cex.axis=3, las=2, lwd.ticks=4)
	mtext( Dseq, at= Dseq,side=1, line=5, cex=5)
	mtext("Vapor pressure deficit", side=1, line=11, cex=6)
	mtext("(KPa)", side=1, line=17,cex=6)
	box(which="plot", lwd=bl)
	
dev.off()	
