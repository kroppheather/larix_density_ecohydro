###########################################################################
###########################################################################
############## Created by Heather Kropp in October 2017      ##############
############## This script is to be run for all analyses     ##############
############## of canopy stomatal conductance calculated     ##############
############## from sapflow.                                 ##############
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
# libraries
library(snow)
library(snowfall)
library(coda)
library(mcmcplots)

#################################################################
####indicate if this is a spatial model                   #######
#################################################################
#1 indicates uses coordinates 0 no
spatialmodel <- 0

#################################################################
####specify directories                                   #######
#################################################################
#model output
saveMdir <- c("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\gc_model\\run34")
#model code
modCode <- "c:\\Users\\hkropp\\Documents\\GitHub\\larch_density_ecohydro\\gc_model\\gc_model_code_antC.r"



#################################################################
####read in datafiles                                     #######
#################################################################

# airport pressure and precip data
datAirP <- read.csv("c:\\Users\\hkropp\\Google Drive\\viperSensor\\airport\\airport.csv")

#canopy rh and temperature
datRH <- read.csv("c:\\Users\\hkropp\\Google Drive\\viperSensor\\met\\RH.VP4.csv")
datTC <- read.csv("c:\\Users\\hkropp\\Google Drive\\viperSensor\\met\\TempC.VP4.csv")

#PAR
datPAR <- read.csv("c:\\Users\\hkropp\\Google Drive\\viperSensor\\met\\PAR.QSOS PAR.csv")

#soil temp
datStemp <- read.csv("c:\\Users\\hkropp\\Google Drive\\viperSensor\\soil\\tempS.GS3.csv")


#################################################################
####organize met data                                     #######
#################################################################
#organize soil temperature
#aggregate two sensors
datSsh <- aggregate(datStemp$tempS.GS3[datStemp$sensorZ==5],
		by=list(datStemp$doy[datStemp$sensorZ==5],
				datStemp$year[datStemp$sensorZ==5],
				datStemp$site[datStemp$sensorZ==5]),FUN="mean",na.action=na.omit)

colnames(datSsh) <- c("doy","year","site","T.s")
datSsh$stand <- ifelse(datSsh$site=="hd",2,1)			

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
####organize data for model                               #######
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
gcALLn <- aggregate(gcALL$g.c, by=list(gcALL$hour,gcALL$doy,gcALL$year,gcALL$stand), FUN="length")
#doesn't need to be averaged since the same but just for matching
gcALLagD <- aggregate(gcALL$D, by=list(gcALL$hour,gcALL$doy,gcALL$year,gcALL$stand), FUN="mean")
gcALLagP <- aggregate(gcALL$PAR, by=list(gcALL$hour,gcALL$doy,gcALL$year,gcALL$stand), FUN="mean")
colnames(gcALLag) <- c("hour","doy","year","stand","g.c")
gcALLag$D <- gcALLagD$x
gcALLag$PAR <- gcALLagP$x

#eliminate half hours where there weren't at least 3 measurements in a stand
gcALLsamp <-gcALLag[gcALLn$x>=3,]


#now make sure there are enough observaions in the day

#first check how many tree X day X stand observations there are
nCheck1 <- aggregate(gcALLsamp$g.c, by=list(gcALLsamp$doy,gcALLsamp$year, gcALLsamp$stand), FUN="length")
colnames(nCheck1) <- c("doy", "year","stand", "n")
nCheck1 <- nCheck1[nCheck1$n>=4,]
#now filter days that don't at least 4 observations
nfilter <- nCheck1[,1:3]

gcALLf <- join(gcALLsamp, nfilter, by=c("doy", "year", "stand"), type="inner")

#lastly filter days that don't at least get to 0.75 D

Dmax <- aggregate(gcALLf$D, by=list(gcALLf$doy,gcALLf$year,gcALLf$stand), FUN="max")
colnames(Dmax) <- c("doy","year","stand","Dmax")
Dmaxsub <- Dmax[Dmax$Dmax>=0.75,]

DmaxJ <- Dmaxsub[,1:3]

#now make sure only higher D days are included:
gcALLft <- join(gcALLf,DmaxJ, by=c("doy", "year", "stand"), type="inner")


#get unique combinations to generate IDS
#standDayIDS
standDay <- unique(data.frame(doy=gcALLft$doy,year=gcALLft$year,stand=gcALLft$stand))
standDay$standDay <-seq(1, dim(standDay)[1])

#join back into gcAll
gcALL2 <- join(gcALLft, standDay, by=c("doy", "year", "stand"), type="left")

#get average daily temperature for a covariate
TairH <- aggregate(datHmet$Temp, by=list(datHmet$doy,datHmet$year), FUN="mean")
colnames(TairH) <- c("doy","year","Tair")
TairH$stand <- rep(2, dim(TairH)[1])

TairL <- aggregate(datLmet$Temp, by=list(datLmet$doy,datLmet$year), FUN="mean")
colnames(TairL) <- c("doy","year","Tair")
TairL$stand <- rep(1, dim(TairL)[1])

Tair <- rbind(TairH, TairL)

#add air temp into the stand day da
standDay2 <- join(standDay,Tair, by=c("doy","year", "stand"), type="left")

#get 

#subset to get unique days
Days <- unique(data.frame(doy=standDay2$doy,year=standDay2$year))
Days <- Days[order(Days$year,Days$doy ),]
Days$Days <- seq(1, dim(Days)[1])

#take averages over previous 2 weeks
#now make a precip matrix that includes days into the past
Npast <- 60
precipmat <- matrix(rep(NA, dim(Days)[1]*Npast), ncol=Npast)

for(i in 1:dim(Days)[1]){
	for(j in 1:Npast){
		precipmat[i,j] <- datAirP$Pr.mm[datAirP$doy==(Days$doy[i]-j)&datAirP$year==Days$year[i]]
	
	}
}

#set up lag periods
lagStart <- c(1,4,8,15,22,36)
lagEnd <- c(3,7,14,21,35,60)

#take averages over lag periods

precipL <- matrix(rep(NA, dim(Days)[1]*length(lagStart)), ncol=length(lagStart))
for(i in 1:dim(Days)[1]){
	for(j in 1:length(lagStart)){
		precipL[i,j] <- sum(precipmat[i,lagStart[j]:lagEnd[j]])
	}	

}

standDay3  <- join(standDay2, Days, by=c("doy", "year"), type="left")

#match up thaw depth data
#create a stand ID in thaw depth
#take only relevant TD cols
#TDall needs 2 more days for hd 2016
TDchange <- TDall$TDday[TDall$year==2016&TDall$site=="hd"&TDall$doy==185]-TDall$TDday[TDall$year==2016&TDall$site=="hd"&TDall$doy==184]
TDtemp <- data.frame(doy=c(182,183), year=c(2016,2016), site=c("hd","hd"), TD=c(NA,NA),
		TDday=c(TDall$TDday[TDall$year==2016&TDall$site=="hd"&TDall$doy==184]-(TDchange*2),
			TDall$TDday[TDall$year==2016&TDall$site=="hd"&TDall$doy==184]-TDchange))
TDall <- rbind(TDtemp,TDall)


TDsub <- data.frame(doy =TDall$doy, year= TDall$year, stand = ifelse(TDall$site=="hd",2,1),TD=TDall$TDday)

#join in stand day
standDay4 <- join(standDay3, TDsub, by=c("doy","year","stand"), type="left")

#variables for centering
#air temp
airTmean<- mean(standDay2$Tair)
#thaw depth minimum
TDstart <- aggregate(standDay4$TD, by=list(standDay4$stand), FUN="min")
colnames(TDstart) <- c("stand", "TD")
TDstart$TD <- floor(TDstart$TD)

#join soil temp into the data.frame
standDay5 <- join(standDay4, datSsh, by=c("doy","year","stand"),type="left")
st5temp <- na.omit(standDay5)
plot(st5temp$T.s,st5temp$TD)
plot(st5temp$T.s,st5temp$Tair)
plot(st5temp$doy,st5temp$TD)
plot((st5temp$doy-200)^2,st5temp$TD)
plot(st5temp$doy,st5temp$T.s)
cor((st5temp$doy-200)^2,st5temp$T.s)
cor(st5temp$doy,st5temp$T.s)
cor(st5temp$T.s,st5temp$TD)
cor(st5temp$T.s,st5temp$Tair)
cor(st5temp$doy,st5temp$TD)
cor(st5temp$TD,(st5temp$doy-200)^2)
gcdoytest <- aggregate(gcALL2$g.c, by=list(gcALL2$doy,gcALL2$year,gcALL2$stand),
				FUN="mean")
colnames(gcdoytest)<-c("doy","year","stand","gc")
plot(gcdoytest$doy[gcdoytest$stand==1&gcdoytest$year==2016],
gcdoytest$gc[gcdoytest$stand==1&gcdoytest$year==2016],pch=19, col="royalblue",
	xlim=c(150,250), ylim=c(0,80))
points(gcdoytest$doy[gcdoytest$stand==1&gcdoytest$year==2017],gcdoytest$gc[gcdoytest$stand==1&gcdoytest$year==2017],pch=19, col="royalblue4")
points(gcdoytest$doy[gcdoytest$stand==2&gcdoytest$year==2016],
gcdoytest$gc[gcdoytest$stand==2&gcdoytest$year==2016],pch=19, col="tomato3",
	)
points(gcdoytest$doy[gcdoytest$stand==2&gcdoytest$year==2017],
	gcdoytest$gc[gcdoytest$stand==2&gcdoytest$year==2017],pch=19, col="tomato4")

points(seq(150,250),40-(.015*(seq(150,250)-200)^2),type="l")	
	
gcdoytest <- join(gcdoytest,standDay5, by=c("doy","year","stand"),type="left")

par(mfrow=c(1,2))
plot(gcdoytest$T.s[gcdoytest$stand==1],
gcdoytest$gc[gcdoytest$stand==1],pch=19, col="royalblue",
	xlim=c(0,10), ylim=c(0,80))
points(gcdoytest$T.s[gcdoytest$stand==2],	
gcdoytest$gc[gcdoytest$stand==2],pch=19, col="tomato3",
	xlim=c(0,10), ylim=c(0,80))
plot(gcdoytest$TD[gcdoytest$stand==1],
gcdoytest$gc[gcdoytest$stand==1],pch=19, col="royalblue",
	xlim=c(0,100), ylim=c(0,80))
points(gcdoytest$TD[gcdoytest$stand==2],	
gcdoytest$gc[gcdoytest$stand==2],pch=19, col="tomato3",
	xlim=c(0,100), ylim=c(0,80))

#try using porportion of rooting distribution thawed
#start with calculation from parameter values
#Rbeta[i]<-exp(log.beta[i])
#log.beta[i]<-(log(depth[i])*(alpha[DaySite[i]]-1))+(log(1-depth[i])*beta[DaySite[i]])	
datRparm <- read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\root_analysis\\siteDay\\siteDayparms.csv")	
#take the deepest profile for each site
hd.p <- datRparm[3,]	
ld.p <- datRparm[7,]
#beta scale
betaR <- function(Alpha,Dmode,Beta,Dprop){
	exp((log(Dprop)*(Alpha-1))+(log(1-Dprop)*Beta))

}	
sum(beta.t)
library(caTools)
plot(seq(0,hd.p$Ave.deep-.1,by=.1),dbeta(seq(0,hd.p$Ave.deep-.1,by=.1)/hd.p$Ave.deep,hd.p$alpha,hd.p$beta))
plot(seq(0,1,by=.01),qbeta(seq(0,1,by=.01),hd.p$alpha,hd.p$beta))
qbeta(.5,ld.p$alpha,ld.p$beta)
qbeta(.5,ld.p$alpha,ld.p$beta)*ld.p$Ave.deep
qbeta(seq(0,1,by=.01),hd.p$alpha,hd.p$beta)*hd.p$Ave.deep
hd.beta <- data.frame(prob=seq(0,1,by=.01),quant=qbeta(seq(0,1,by=.01),hd.p$alpha,hd.p$beta),
						depth=qbeta(seq(0,1,by=.01),hd.p$alpha,hd.p$beta)*hd.p$Ave.deep)

hd.prob <- data.frame(quant=seq(0,1,by=.01),depth=seq(0,1,by=.01)*hd.p$Ave.deep,
						prob=pbeta(seq(0,1,by=.01),hd.p$alpha,hd.p$beta))
						
pbeta(.1)
(hd.p$alpha-(1/3))/(hd.p$alpha+hd.p$beta-(2/3))*hd.p$Ave.deep
pbeta((hd.p$alpha-(1/3))/(hd.p$alpha+hd.p$beta-(2/3)),hd.p$alpha,hd.p$beta)

standDay5$pRoot <- ifelse(standDay5$stand==1,
						pbeta(standDay5$TD/ld.p$Ave.deep,ld.p$alpha,ld.p$beta),
						pbeta(standDay5$TD/hd.p$Ave.deep,hd.p$alpha,hd.p$beta))
gcdoytest$pRootf <- 1-standDay5$pRoot
par(mfrow=c(1,3))
plot(gcdoytest$pRoot[gcdoytest$stand==1],gcdoytest$gc[gcdoytest$stand==1],ylim=c(0,50),pch=19, col="royalblue")						
points(gcdoytest$pRoot[gcdoytest$stand==2],gcdoytest$gc[gcdoytest$stand==2],pch=19, col="tomato3")						

plot(gcdoytest$pRootf[gcdoytest$stand==1],gcdoytest$gc[gcdoytest$stand==1],ylim=c(0,50),pch=19, col="royalblue")						
points(gcdoytest$pRootf[gcdoytest$stand==2],gcdoytest$gc[gcdoytest$stand==2],pch=19, col="tomato3")						

plot(log(gcdoytest$pRoot[gcdoytest$stand==1]),gcdoytest$gc[gcdoytest$stand==1],ylim=c(0,50),pch=19, col="royalblue")						
points(log(gcdoytest$pRoot[gcdoytest$stand==2]),gcdoytest$gc[gcdoytest$stand==2],pch=19, col="tomato3")						

plot(gcdoytest$pRootf,gcdoytest$T.s)
gctempA<- na.omit(gcdoytest)
cor(gctempA$pRootf,gctempA$T.s)	
cor(gctempA$pRootf,gctempA$Tair)	
cor(gctempA$pRootf,(gctempA$doy-200)^2)
cor(gctempA$pRootf,gctempA$doy)


					
#################################################################
####model run                                             #######
#################################################################
#try running  model without stochastic antecedent precip in JAGS and with all variables

#new data stand.obs, NstandDayTree, standDayTree, stand, tree, N tree, thawD, thawstart(stand), Nstand, xC[i,y] DistA=sqrt(pow(xC[y]-xC[m],2)+ pow(y[r] - y[c], 2))
#data list

datalist <- list(Nobs=dim(gcALL2)[1], gs=gcALL2$g.c, stand.obs=gcALL2$stand, standDay=gcALL2$standDay,
					PAR=gcALL2$PAR,
					D=gcALL2$D, NstandDay=dim(standDay4)[1],
					stand=standDay4$stand, airT=standDay4$Tair,
					airTmean=airTmean,thawD=standDay4$TD, thawstart=TDstart$TD, 
					 Nstand=2,a.pr=precipL,days=standDay4$Days,Nlag=length(lagStart),Ndays=dim(Days)[1],Nparm=4 )

# set parameters to monitor
parms <-c( "a", "b", "d","S","gref","l.slope","rep.gs", "wpr","deltapr","pastpr")

# set the number of CPUs to be 3
sfInit(parallel=TRUE, cpus=3)

# assign the R2OpenBUGS library to each CPU
sfLibrary(R2OpenBUGS)	


#creating separate directory for each CPU process
folder1 <- paste0(saveMdir, "\\chain1")
folder2 <- paste0(saveMdir, "\\chain2")
folder3 <- paste0(saveMdir, "\\chain3")
dir.create(folder1); dir.create(folder2); dir.create(folder3)	
folderALL <- c(folder1, folder2, folder3)
#copy model code
for (i in 1:length(folderALL)){

	file.copy(modCode, paste0(folderALL[i], "\\model_code.txt"), overwrite=TRUE) 

}	

#get model started but run manually
parallel.bugs <- function(chain, x.data, params){
	folder <- ifelse(chain==1,"c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\gc_model\\run34\\chain1",
				ifelse(chain==2,"c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\gc_model\\run34\\chain2",
					"c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\gc_model\\run34\\chain3"))
 	
	
	# 5b. call openbugs
	bugs(data=x.data, inits=NULL,parameters.to.save=params,
             n.iter=10, n.chains=1, n.burnin=1, n.thin=1,
             model.file="model_code.txt", codaPkg=TRUE,
             OpenBUGS.pgm="C:/Program Files (x86)/OpenBUGS/OpenBUGS323/OpenBUGS.exe",debug=TRUE,
             working.directory=folder)	
}			 


# parallel.bugs on each of the 3 CPUs
sfLapply(1:3, fun=parallel.bugs,x.data=datalist, params=parms)
#after the small number of iterations runs, I make sure it uses a slice updater, run for a test of 11 samples,
#ran thinning by 350 for 5000 samples. First 2,000 are burn in.


folder1 <- paste0(saveMdir, "\\CODA_out\\chain1\\")
folder2 <- paste0(saveMdir, "\\CODA_out\\chain2\\")
folder3 <- paste0(saveMdir, "\\CODA_out\\chain3\\")




# 9. pull coda back out
codaobj1 <- read.bugs(c(paste0(folder1, "\\CODAchain1.txt"),
						paste0(folder2, "\\CODAchain1.txt")
						,paste0(folder3, "\\CODAchain1.txt")
						))


mcmcplot(codaobj1, parms=c( "a", "b", "d","wpr","deltapr"),  dir=paste0(saveMdir, "\\history"))


modSum <-summary(codaobj1) 


write.table(modSum$statistics, paste0(saveMdir, "\\out", "\\mod_stats.csv"), sep=",", row.names=TRUE)
write.table(modSum$quantiles, paste0(saveMdir, "\\out", "\\mod_quants.csv"), sep=",", row.names=TRUE)
write.table(standDay4, paste0(saveMdir, "\\out", "\\standDay.csv"), sep=",", row.names=FALSE)
write.table(gcALL2, paste0(saveMdir, "\\out", "\\gcdata.csv"), sep=",", row.names=FALSE)
#aggregate to compare means
