###########################################################################
###########################################################################
############## Created by Heather Kropp in October 2017      ##############
############## Extracts paramters and plots from gc model    ##############
###########################################################################



#libraries
library(plyr)

#set plot directory
plotDI <- "c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\gc_model\\plots\\run34"


#read in stand day data

daySD <- read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\gc_model\\run34\\out\\standDay.csv")
datgc <-read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\gc_model\\run34\\out\\gcdata.csv")
#################################################################
####read in parameters                                    #######
#################################################################
datM <- read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\gc_model\\run34\\out\\mod_stats.csv")
datQ <- read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\gc_model\\run34\\out\\mod_quants.csv")

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
datmugs<- datC[datC$parms3=="mugs",]
datprecip<- datC[datC$parms3=="pastpr",]
datprecip$Days <- rep(seq(1, 60),each=2)
colnames(datprecip)[1:13] <- paste0("p",colnames(datprecip)[1:13])
datprecip$stand <- rep(c(1,2), times=60)
#add ind
datS <- cbind(datS,daySD)
datgrefA <- cbind(datgref,daySD)
datparm <- datC[datC$parms3=="a"|datC$parms3=="b"|datC$parms3=="d",]

datgrefA <- join(datgrefA, datprecip, by=c("Days", "stand"),type="left")

datparm$stand <- rep(c(1,2), times=12)
datparm$pN <- rep(rep(c(1,2,3,4),each=2),times=3)


datw<- datC[datC$parms3=="wpr",]
datw$stand<- rep(c(1,2), times=6)
datw$wn <- rep(seq(1,6), each=2)
#################################################################
####goodness of fit plots                                 #######
#################################################################
plot(datgc$g.c,datrep$Mean, pch=19, xlim=c(0,100), ylim=c(0,100))
fit<-lm(datrep$Mean~datgc$g.c)
summary(fit)
abline(fit)
abline(0,1, lwd=2, col="red")


#Tair mean =14.13
#TDstart 1= 14, 2=11
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
Tmin <-5
Tmax <- 25
Thmin <- 0
Thmax <- 100
Pmin <-0
Pmax <- 30
colL <-"royalblue"
colH <- "tomato3"
axisC <-3


#set up regression line plot sequences
regL <- function(x,b1,b2,X0){
	b1+(b2*(x-X0))

}

Tseq <- seq(Tmin,Tmax, by=.1)
Pseq <- seq(Pmin,Pmax, by=.1)
Thseq <- seq(Thmin,Thmax, by=.1)

jpeg(paste0(plotDI , "\\regression_coeff.jpg"), width=3000, height=2000, units="px", quality=100)
	ab <- layout(matrix(seq(1,6), ncol=3, byrow=FALSE), width=rep(lcm(wd),6), height=rep(lcm(hd),6))
	
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
	legend(5,102, c("low density", "high density"), col=c(colL,colH), bty="n", cex=4, pch=19)
	
	if(datparm$Sig[datparm$parms3=="a"&datparm$stand==1&datparm$pN==2]==1){
		points(Tseq,regL(Tseq,datparm$Mean[datparm$parms3=="a"&datparm$stand==1&datparm$pN==1],
				datparm$Mean[datparm$parms3=="a"&datparm$stand==1&datparm$pN==2],14.13), type="l", lwd=4, col=colL)
	}else{
		abline(h=datparm$Mean[datparm$parms3=="a"&datparm$stand==1&datparm$pN==1], lwd=4, lty=3, col=colL)
	}
	
	if(datparm$Sig[datparm$parms3=="a"&datparm$stand==2&datparm$pN==2]==1){
		points(Tseq,regL(Tseq,datparm$Mean[datparm$parms3=="a"&datparm$stand==2&datparm$pN==1],
				datparm$Mean[datparm$parms3=="a"&datparm$stand==2&datparm$pN==2],14.13), type="l", lwd=4, col=colH)
	}else{
		abline(h=datparm$Mean[datparm$parms3=="a"&datparm$stand==2&datparm$pN==1], lwd=4, lty=3, col=colH)
	}	
box(which="plot")	


	
par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1), type="n", xlim=c(Tmin,Tmax), ylim=c(Smin,Smax), axes=FALSE, xaxs="i", yaxs="i",
			xlab=" ", ylab=" ")
	arrows(datgrefA$Tair,datS$X2.5.,datgrefA$Tair,datS$X97.5.,lwd=2,code=0)
	
	points(datgrefA$Tair[datgrefA$stand==1], datS$Mean[datS$stand==1],
			col=colL, pch=19, cex=4)
	points(datgrefA$Tair[datgrefA$stand==2], datS$Mean[datS$stand==2],
			col=colH, pch=19, cex=4)	
			
	if(datparm$Sig[datparm$parms3=="b"&datparm$stand==1&datparm$pN==2]==1){
		points(Tseq,regL(Tseq,datparm$Mean[datparm$parms3=="b"&datparm$stand==1&datparm$pN==1],
				datparm$Mean[datparm$parms3=="b"&datparm$stand==1&datparm$pN==2],14.13), type="l", lwd=4, col=colL)
	}else{
		abline(h=datparm$Mean[datparm$parms3=="b"&datparm$stand==1&datparm$pN==1], lwd=4, lty=3, col=colL)
	}
			
	if(datparm$Sig[datparm$parms3=="b"&datparm$stand==2&datparm$pN==2]==1){
		points(Tseq,regL(Tseq,datparm$Mean[datparm$parms3=="b"&datparm$stand==2&datparm$pN==1],
				datparm$Mean[datparm$parms3=="b"&datparm$stand==2&datparm$pN==2],14.13), type="l", lwd=4, col=colH)
	}else{
		abline(h=datparm$Mean[datparm$parms3=="b"&datparm$stand==2&datparm$pN==1], lwd=4, lty=3, col=colH)
	}			
			
			
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


	if(datparm$Sig[datparm$parms3=="a"&datparm$stand==1&datparm$pN==3]==1){
		points(Pseq,regL(Pseq,datparm$Mean[datparm$parms3=="a"&datparm$stand==1&datparm$pN==1],
				datparm$Mean[datparm$parms3=="a"&datparm$stand==1&datparm$pN==3],5), type="l", lwd=4, col=colL)
	}else{
		abline(h=datparm$Mean[datparm$parms3=="a"&datparm$stand==1&datparm$pN==1], lwd=4, lty=3, col=colL)
	}
	
	if(datparm$Sig[datparm$parms3=="a"&datparm$stand==2&datparm$pN==3]==1){
		points(Pseq,regL(Pseq,datparm$Mean[datparm$parms3=="a"&datparm$stand==2&datparm$pN==1],
				datparm$Mean[datparm$parms3=="a"&datparm$stand==2&datparm$pN==3],5), type="l", lwd=4, col=colH)
	}else{
		abline(h=datparm$Mean[datparm$parms3=="a"&datparm$stand==2&datparm$pN==1], lwd=4, lty=3, col=colH)
	}	



			
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

	if(datparm$Sig[datparm$parms3=="b"&datparm$stand==1&datparm$pN==3]==1){
		points(Pseq,regL(Pseq,datparm$Mean[datparm$parms3=="b"&datparm$stand==1&datparm$pN==1],
				datparm$Mean[datparm$parms3=="b"&datparm$stand==1&datparm$pN==3],5), type="l", lwd=4, col=colL)
	}else{
		abline(h=datparm$Mean[datparm$parms3=="b"&datparm$stand==1&datparm$pN==1], lwd=4, lty=3, col=colL)
	}
			
	if(datparm$Sig[datparm$parms3=="b"&datparm$stand==2&datparm$pN==3]==1){
		points(Pseq,regL(Pseq,datparm$Mean[datparm$parms3=="b"&datparm$stand==2&datparm$pN==1],
				datparm$Mean[datparm$parms3=="b"&datparm$stand==2&datparm$pN==3],5), type="l", lwd=4, col=colH)
	}else{
		abline(h=datparm$Mean[datparm$parms3=="b"&datparm$stand==2&datparm$pN==1], lwd=4, lty=3, col=colH)
	}	


			
	axis(1, seq(Pmin,Pmax, by=5), rep(" ", length(seq(Pmin,Pmax, by=5))), cex.axis=axisC, lwd.ticks=3)
	mtext(seq(Pmin,Pmax, by=5),at=seq(Pmin,Pmax, by=5), line=4, side=1, cex=3)
	mtext("Past precipitation over 2 months (mm)",side=1, line=8, cex=4)
box(which="plot")	



	
par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1), type="n", xlim=c(Thmin,Thmax), ylim=c(gmin,gmax), axes=FALSE, xaxs="i", yaxs="i",
			xlab=" ", ylab=" ")
	arrows(datgrefA$TD, datgrefA$X2.5.,datgrefA$TD, datgrefA$X97.5.,lwd=2,code=0 )
	points(datgrefA$TD[datgrefA$stand==1], datgrefA$Mean[datgrefA$stand==1],
			col=colL, pch=19, cex=4)
	points(datgrefA$TD[datgrefA$stand==2], datgrefA$Mean[datgrefA$stand==2],
			col=colH, pch=19, cex=4)

			
	if(datparm$Sig[datparm$parms3=="a"&datparm$stand==1&datparm$pN==4]==1){
		points(Thseq,regL(Thseq,datparm$Mean[datparm$parms3=="a"&datparm$stand==1&datparm$pN==1],
				datparm$Mean[datparm$parms3=="a"&datparm$stand==1&datparm$pN==4],14), type="l", lwd=4, col=colL)
	}else{
		abline(h=datparm$Mean[datparm$parms3=="a"&datparm$stand==1&datparm$pN==1], lwd=4, lty=3, col=colL)
	}
	
	if(datparm$Sig[datparm$parms3=="a"&datparm$stand==2&datparm$pN==4]==1){
		points(Thseq,regL(Thseq,datparm$Mean[datparm$parms3=="a"&datparm$stand==2&datparm$pN==1],
				datparm$Mean[datparm$parms3=="a"&datparm$stand==2&datparm$pN==4],11), type="l", lwd=4, col=colH)
	}else{
		abline(h=datparm$Mean[datparm$parms3=="a"&datparm$stand==2&datparm$pN==1], lwd=4, lty=3, col=colH)
	}	

box(which="plot")

par(mai=c(0,0,0,0))
	plot(c(0,1),c(0,1), type="n", xlim=c(Thmin,Thmax), ylim=c(Smin,Smax), axes=FALSE, xaxs="i", yaxs="i",
			xlab=" ", ylab=" ")
	arrows(datgrefA$TD, datS$X2.5.,datgrefA$TD, datS$X97.5.,lwd=2,code=0 )
	points(datgrefA$TD[datS$stand==1], datS$Mean[datS$stand==1],
			col=colL, pch=19, cex=4)
	points(datgrefA$TD[datS$stand==2], datS$Mean[datS$stand==2],
			col=colH, pch=19, cex=4)			
	
	axis(1, seq(Thmin,Thmax, by=10), rep(" ", length(seq(Thmin,Thmax, by=10))), cex.axis=axisC, lwd.ticks=3)
	mtext(seq(Thmin,Thmax, by=10),at=seq(Thmin,Thmax, by=10), line=4, side=1, cex=3)
	mtext("Thaw depth (cm)",side=1, line=8, cex=4)
	
	if(datparm$Sig[datparm$parms3=="b"&datparm$stand==1&datparm$pN==4]==1){
		points(Thseq,regL(Thseq,datparm$Mean[datparm$parms3=="b"&datparm$stand==1&datparm$pN==1],
				datparm$Mean[datparm$parms3=="b"&datparm$stand==1&datparm$pN==4],14), type="l", lwd=4, col=colL)
	}else{
		abline(h=datparm$Mean[datparm$parms3=="b"&datparm$stand==1&datparm$pN==1], lwd=4, lty=3, col=colL)
	}
			
	if(datparm$Sig[datparm$parms3=="b"&datparm$stand==2&datparm$pN==4]==1){
		points(Thseq,regL(Thseq,datparm$Mean[datparm$parms3=="b"&datparm$stand==2&datparm$pN==1],
				datparm$Mean[datparm$parms3=="b"&datparm$stand==2&datparm$pN==4],11), type="l", lwd=4, col=colH)
	}else{
		abline(h=datparm$Mean[datparm$parms3=="b"&datparm$stand==2&datparm$pN==1], lwd=4, lty=3, col=colH)
	}	
	
	
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
#lagStart <- c(1,4,8,15,22,36)
#lagEnd <- c(3,7,14,21,35,60)


#set up x for low
xseq1 <- c(1,4,7,10,13,16)
xseq2 <- c(2,5,8,11,14,17)
xl <-0
xh <-18

colL <-"royalblue"
colH <- "tomato3"


jpeg(paste0(plotDI , "\\weights.jpg"), width=1700, height=1500, units="px", quality=100)
	par(mai=c(3,3,3,3))
	plot(c(0,1),c(0,1), type="n", xlim=c(xl,xh), ylim=c(0,1), xlab=" ", ylab=" ",
		xaxs="i", yaxs="i", axes=FALSE)
	abline(h=1/6, lty=3, lwd=2)	
	for(i in 1:6){
	polygon(c(xseq1[i]-.5,xseq1[i]-.5,xseq1[i]+.5,xseq1[i]+.5),
			c(0,datw$Mean[datw$stand==1][i],datw$Mean[datw$stand==1][i],0), col=colL, border=FALSE)
	polygon(c(xseq2[i]-.5,xseq2[i]-.5,xseq2[i]+.5,xseq2[i]+.5),
			c(0,datw$Mean[datw$stand==2][i],datw$Mean[datw$stand==2][i],0), col=colH, border=FALSE)
	}	
	arrows(xseq1,datw$X2.5.[datw$stand==1],xseq1,datw$X97.5.[datw$stand==1], lwd=2, code=0)
	arrows(xseq2,datw$X2.5.[datw$stand==2],xseq2,datw$X97.5.[datw$stand==2], lwd=2, code=0)
	legend(1,1, c("low density", "high density", "uniform average"), 
			col=c(colL,colH, "black"),pch=c(15,15,NA), lty=c(NA,NA,3), lwd=c(NA,NA,2), cex=3, bty="n")
	axis(2, seq(0,1, by=.1), cex.axis=3, las=2, lwd.ticks=3)
	axis(1, c(-1,1.5,4.5,7.5,10.5,13.5,16.5,20), rep(" ", 8), lwd.ticks=3)
	mtext(c("1-3 day", "4-7 days", "8-14 days", "15-21 days", "22-35 days", "36-60 days"),
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