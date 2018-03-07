#library
library(plyr)
#set working directory
setwd("c:\\Users\\hkropp\\Google Drive\\root_analysis")

#read in data file

datR<-read.csv("density_fr.csv")

#read in soil profile data 
datSP<-read.csv("soil profile all.csv")


datR$vol<-datR$dim1*datR$dim2*(datR$depth.end-datR$depth.start)
datR$mid.pt<-datR$depth.start+((datR$depth.end-datR$depth.start)/2)
#calculate densidy in mg/cm3
datR$rdens<-(datR$fine.root.biomass/datR$vol)*1000

############################################
###look specifically  by layer type

#get the end of each layer
dEL<-aggregate(datR$depth.end, by=list(datR$soil.type,datR$location,datR$rep,datR$stand),FUN="max")
dSL<-aggregate(datR$depth.start, by=list(datR$soil.type,datR$location,datR$rep,datR$stand),FUN="min")
#get total root biomass in the layer
dRB<-aggregate(datR$fine.root.biomass, by=list(datR$soil.type,datR$location,datR$rep,datR$stand),FUN="sum")
#total volume of the layer
dV<-aggregate(datR$vol, by=list(datR$soil.type,datR$location,datR$rep,datR$stand),FUN="sum")
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
#make a plot of the root biomass by depth for all points
par(mai=c(1,1,1,1))
plot(c(0,1),c(0,1), type="n",ylim=c(22,0),xlim=c(-.1,20), axes=FALSE, xlab=" ", ylab=" ", yaxs="i",
		xaxs="i")
	points(datR$rdens[datR$stand=="hd"],datR$mid.pt[datR$stand=="hd"], pch=19, col="palegreen4",cex=2)	
	points(datR$rdens[datR$stand=="ld"],datR$mid.pt[datR$stand=="ld"], pch=19, col="royalblue1",cex=2)
axis(1, seq(-2,20, by=2), cex.axis=1.5)
axis(2, seq(24,0, by=-2), las=2, cex.axis=1.5)	
mtext(expression(paste("Root density (mg cm"^"-3",")")), side=1, line=3.5, cex=2)
mtext("Depth (cm)", side=2, line=3, cex=2)	

###########################################################
#make a barplot 
#of density in each layer
#add a plotting order to layR

#order is moss, organic, mineral
#start with hd in each layer

layR$pcol<-rep(c(rgb(205/255,79/255,57/255,.9),"royalblue"), each=3)


#setup plot layout
wd<-18
hd<-18

ac<-layout(matrix(seq(1,2),ncol=2), width=rep(lcm(wd),2),height=rep(lcm(hd),2))
layout.show(ac)

par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n",ylim=c(6.1,-.1),xlim=c(-.1,8), axes=FALSE, xlab=" ", ylab=" ", yaxs="i",
		xaxs="i")
	for(i in 1:dim(layR)[1]){
	polygon(c(0,layR$r.d[i],layR$r.d[i],0),c(layR$pseq[i]-1,layR$pseq[i]-1,layR$pseq[i],layR$pseq[i]),
			col=layR$pcol[i])	
	}
	arrows(layR$r.d-layR$se,layR$pseq-.5,layR$r.d+layR$se,layR$pseq-.5,code=0,lwd=2)
axis(2, c(1,3,5), c("moss", "organic", "mineral < 20 cm"), las=2, cex.axis=2)
axis(1, seq(0,7, by=1), cex.axis=2)
box(which="plot")
legend(4,0,c("high density","low density"), fill=c(rgb(205/255,79/255,57/255,.9),"royalblue"),bty="n", cex=2)
mtext("Soil layer", side=2, cex=3, line=12)
mtext(expression(paste("Root biomass mg cm"^"-3")), side=1, cex=3, line=5)
text(layR$r.d+layR$se+.5,layR$pseq-.5,layR$sL,cex=2)
#plot thickness
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n",ylim=c(20,0),xlim=c(0,2), axes=FALSE, xlab=" ", ylab=" ", yaxs="i",
		xaxs="i")
	for(i in 1:2){
		polygon(c(i-1,i-1,i,i),c(0,Green$thick[i],Green$thick[i],0), col="palegreen4")
	}

	for(i in 1:2){
		polygon(c(i-1,i-1,i,i),
		c(Green$thick[i],Green$thick[i]+Brown$thick[i],Green$thick[i]+Brown$thick[i],Green$thick[i]), 
		col="burlywood4")
	}

		for(i in 1:2){
		polygon(c(i-1,i-1,i,i),
		c(Green$thick[i]+Brown$thick[i],Green$thick[i]+Brown$thick[i]+Organic$thick[i],
		Green$thick[i]+Brown$thick[i]+Organic$thick[i],Green$thick[i]+Brown$thick[i]), 
		col="saddlebrown")
	}

	
	
		arrows(c(.5,1.5), Green$thick-Green$se,c(.5,1.5), Green$thick+Green$se, lwd=2, code=0)
		
			arrows(c(.5,1.5), Green$thick+Brown$thick-Brown$se,c(.5,1.5), 
					Green$thick+Brown$thick+Brown$se, lwd=2, code=0)
		arrows(c(.5,1.5), Green$thick+Brown$thick+Organic$thick-Organic$se,c(.5,1.5), 
					Green$thick+Brown$thick+Organic$thick+Organic$se, lwd=2, code=0)
	
	axis(4,seq(20,0, by=-2),las=2, cex.axis=2)
	box(which="plot")	
	axis(1, c(.5,1.5), c("high density", "low density"), cex.axis=2)
mtext("Soil layer depth", side=4, cex=3, line=8)
mtext("Stand", side=1, cex=3, line=5)

legend(1,13,c("green moss", "brown moss", "fibric organic"), fill=c("palegreen4", "burlywood4","saddlebrown"), bty="n", cex=2)