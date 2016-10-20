#############################################
##########Soil Profile Data #################

setwd("c:\\Users\\hkropp\\Google Drive\\root_analysis")
#read in data describing the profile
datP<-read.csv("soil_prof_desc.csv")

#green data frame to omit NAs
green<-na.omit(data.frame(site=datP$site,location=datP$location,rep=datP$rep,green=datP$green))
#brown data frame
brown<-na.omit(data.frame(site=datP$site,location=datP$location,rep=datP$rep,brown=datP$brown))
#organic dataframe
organic<-na.omit(data.frame(site=datP$site,location=datP$location,rep=datP$rep,org=datP$org))

#aggregrate the upper profile characterstics
orgD<-aggregate(organic$org, by=list(organic$location,organic$site), FUN="mean")
colnames(orgD)<-c("location", "site", "depth")
orgDs<-aggregate(organic$org, by=list(organic$location,organic$site), FUN="sd")
colnames(orgDs)<-c("location", "site", "sd")
#green
greenD<-aggregate(green$green, by=list(green$location,green$site), FUN="mean")
colnames(greenD)<-c("location", "site", "depth")
greenDs<-aggregate(green$green, by=list(green$location,green$site), FUN="sd")
colnames(greenDs)<-c("location", "site", "sd")

#brown
brownD<-aggregate(brown$brown, by=list(brown$location,brown$site), FUN="mean")
colnames(brownD)<-c("location", "site", "depth")
brownDs<-aggregate(brown$brown, by=list(brown$location,brown$site), FUN="sd")
colnames(brownDs)<-c("location", "site", "sd")

#read in thaw depth data
datF<-read.csv("active_depth.csv")
FD<-aggregate(datF$Frozen.Depth, by=list(datF$period,datF$Loc,datF$Site), FUN="mean")
FDs<-aggregate(datF$Frozen.Depth, by=list(datF$period,datF$Loc,datF$Site), FUN="sd")
colnames(FD)<-c("time", "location", "site","depth")
colnames(FDs)<-c("time", "location", "site","sd")
#make a dataframe for each site and location

HighS<-data.frame(type=c("green", "brown", "organic", "TD1","TD2","TD3","TD4"), 
				depth=c(greenD$depth[greenD$location=="s"&greenD$site=="h"],
						brownD$depth[brownD$location=="s"&brownD$site=="h"],
						orgD$depth[orgD$location=="s"&orgD$site=="h"],
						FD$depth[FD$time==1&FD$location=="s"&FD$site=="h"],
						FD$depth[FD$time==2&FD$location=="s"&FD$site=="h"],
						FD$depth[FD$time==3&FD$location=="s"&FD$site=="h"],
						FD$depth[FD$time==4&FD$location=="s"&FD$site=="h"]),
				sd=c(greenDs$sd[greenDs$location=="s"&greenDs$site=="h"],
						brownDs$sd[brownDs$location=="s"&brownDs$site=="h"],
						orgDs$sd[orgDs$location=="s"&orgDs$site=="h"],
						FDs$sd[FD$time==1&FD$location=="s"&FD$site=="h"],
						FDs$sd[FD$time==2&FD$location=="s"&FD$site=="h"],
						FDs$sd[FD$time==3&FD$location=="s"&FD$site=="h"],
						FDs$sd[FD$time==4&FD$location=="s"&FD$site=="h"])
						)
HighS$low<-HighS$depth-HighS$sd						
HighS$high<-HighS$depth+HighS$sd	
HighT<-data.frame(type=c("green", "brown", "organic", "TD1","TD2","TD3","TD4"), 
				depth=c(greenD$depth[greenD$location=="t"&greenD$site=="h"],
						brownD$depth[brownD$location=="t"&brownD$site=="h"],
						orgD$depth[orgD$location=="t"&orgD$site=="h"],
						FD$depth[FD$time==1&FD$location=="t"&FD$site=="h"],
						FD$depth[FD$time==2&FD$location=="t"&FD$site=="h"],
						FD$depth[FD$time==3&FD$location=="t"&FD$site=="h"],
						FD$depth[FD$time==4&FD$location=="t"&FD$site=="h"]),
				sd=c(greenDs$sd[greenDs$location=="t"&greenDs$site=="h"],
						brownDs$sd[brownDs$location=="t"&brownDs$site=="h"],
						orgDs$sd[orgDs$location=="t"&orgDs$site=="h"],
						FDs$sd[FD$time==1&FD$location=="t"&FD$site=="h"],
						FDs$sd[FD$time==2&FD$location=="t"&FD$site=="h"],
						FDs$sd[FD$time==3&FD$location=="t"&FD$site=="h"],
						FDs$sd[FD$time==4&FD$location=="t"&FD$site=="h"]))
						
						
						
HighT$low<-HighT$depth-HighT$sd						
HighT$high<-HighT$depth+HighT$sd

LowS<-data.frame(type=c("green", "brown", "organic", "TD1","TD2","TD3","TD4"), 
				depth=c(greenD$depth[greenD$location=="s"&greenD$site=="l"],
						brownD$depth[brownD$location=="s"&brownD$site=="l"],
						orgD$depth[orgD$location=="s"&orgD$site=="l"],
						FD$depth[FD$time==1&FD$location=="s"&FD$site=="l"],
						0,
						FD$depth[FD$time==3&FD$location=="s"&FD$site=="l"],
						FD$depth[FD$time==4&FD$location=="s"&FD$site=="l"]),
				sd=c(greenDs$sd[greenDs$location=="s"&greenDs$site=="l"],
						brownDs$sd[brownDs$location=="s"&brownDs$site=="l"],
						orgDs$sd[orgDs$location=="s"&orgDs$site=="l"],
						FDs$sd[FD$time==1&FD$location=="s"&FD$site=="l"],
						0,
						FDs$sd[FD$time==3&FD$location=="s"&FD$site=="l"],
						FDs$sd[FD$time==4&FD$location=="s"&FD$site=="l"]))
LowS$low<-LowS$depth-LowS$sd						
LowS$high<-LowS$depth+LowS$sd

LowT<-data.frame(type=c("green", "brown", "organic", "TD1","TD2","TD3","TD4"),
				depth=c(greenD$depth[greenD$location=="t"&greenD$site=="l"],
						brownD$depth[brownD$location=="t"&brownD$site=="l"],
						orgD$depth[orgD$location=="t"&orgD$site=="l"],
						FD$depth[FD$time==1&FD$location=="t"&FD$site=="l"],
						0,
						FD$depth[FD$time==3&FD$location=="t"&FD$site=="l"],
						FD$depth[FD$time==4&FD$location=="t"&FD$site=="l"]),						
				sd=c(greenDs$sd[greenDs$location=="t"&greenDs$site=="l"],
						brownDs$sd[brownDs$location=="t"&brownDs$site=="l"],
						orgDs$sd[orgDs$location=="t"&orgDs$site=="l"],
						FDs$sd[FD$time==1&FD$location=="t"&FD$site=="l"],
						0,
						FDs$sd[FD$time==3&FD$location=="t"&FD$site=="l"],
						FDs$sd[FD$time==4&FD$location=="t"&FD$site=="l"]))
LowT$low<-LowT$depth-LowT$sd						
LowT$high<-LowT$depth+LowT$sd

#make a barplot of the profiles

a<-layout(matrix(c(1), ncol=1), width=c(lcm(40)), height=c(lcm(20)))
layout.show(a)

#set up plot variables
xseqHSs<-seq(1,31,by=5)
xseqHSe<-xseqHSs+1
xseqHTs<-seq(2,32,by=5)
xseqHTe<-xseqHTs+1


xseqLSs<-seq(3,33,by=5)
xseqLSe<-xseqLSs+1
xseqLTs<-seq(4,34,by=5)
xseqLTe<-xseqLTs+1

xseqLSa<-xseqLSs+.5
xseqLTa<-xseqLTs+.5

xseqHSa<-xseqHSs+.5
xseqHTa<-xseqHTs+.5


xu<-36
xl<-0
yu<-105
yl<-0

layout.show(a)
#make empty plot
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(xl,xu), ylim=c(yl,yu), xlab=" ", ylab=" ", axes=FALSE, xaxs="i",yaxs="i")
#add bars
for(i in 1:7){
#high tree
	polygon(c(xseqHTs[i],xseqHTs[i],xseqHTe[i],xseqHTe[i]), c(0,HighT$depth[i],HighT$depth[i],0), col="royalblue4")
#low tree	
	polygon(c(xseqLTs[i],xseqLTs[i],xseqLTe[i],xseqLTe[i]), c(0,LowT$depth[i],LowT$depth[i],0), col="sienna4")
#high shrub	
	polygon(c(xseqHSs[i],xseqHSs[i],xseqHSe[i],xseqHSe[i]), c(0,HighS$depth[i],HighS$depth[i],0), col="royalblue1")
#low shrub	
	polygon(c(xseqLSs[i],xseqLSs[i],xseqLSe[i],xseqLSe[i]), c(0,LowS$depth[i],LowS$depth[i],0), col="sienna3")	
	
	}
#add error bars
arrows(xseqHTa,HighT$low,xseqHTa,HighT$high, code=0)
arrows(xseqLTa,LowT$low,xseqLTa,LowT$high, code=0)
arrows(xseqHSa,HighS$low,xseqHSa,HighS$high, code=0)
arrows(xseqLSa,LowS$low,xseqLSa,LowS$high, code=0)
#add panel box
box(which="plot")
#add axis
axis(2, seq(0,105, by=5), las=2, cex.axis=1.25)
axis(1, xseqHTe, c("green", "brown", "organic", "TD Early July ", "TD Mid July", "TD End July", "TD End August"), cex.axis=1.25)
mtext("Depth in Soil Profile (cm)", side=2, cex=2, line=3)
mtext("Type",  line=3, side=1, cex=2)

legend(1,100, c("High density shrub", "High density tree", "Low density shrub","Low density tree"), 
		fill=c("royalblue1","royalblue4","sienna3","sienna4"), bty="n", cex=1.5)
		
####################################################################
#################Make a plot if the model results  #################
#################and the root biomass across depth #################

#set working directory
setwd("c:\\Users\\hkropp\\Google Drive\\root_analysis")
#read in data
datR<-read.csv("fine_root_out.csv")
#read in root profile parameters
datP<-read.csv("parameters_3a407f3.csv")
#read in median root profile estimate
datM<-read.csv("rmed_3a407f3.csv")
#read in index and depth info
datD<-read.csv("total_root_loc_site_period.csv")
#read in data for estimated depth increments




#run function on parameter values
D.seq<-matrix(rep(0,100*14), ncol=14)
D.seqUN<-matrix(rep(0,100*14), ncol=14)
for(i in 1:14){
	D.seq[,i]<-seq(.1,datD$A.depth[i],length.out=100)/datD$A.depth[i]
	D.seqUN[,i]<-seq(.1,datD$A.depth[i],length.out=100)
}



#try an overlay plot where the root function 0-1 is plotted to the average active layer depth
#and the root biomass is represented with points
lw<-10
lh<-11

ab<-layout(matrix(seq(1,8), ncol=4, byrow=TRUE), width=c(lcm(lw),lcm(lw),lcm(lw),lcm(lw),lcm(lw),lcm(lw),lcm(lw),lcm(lw)),
				height=c(lcm(lh),lcm(lh),lcm(lh),lcm(lh),lcm(lh),lcm(lh),lcm(lh),lcm(lh)))
				
layout.show(ab)
#find the maximum measurement of roots
depmax<-aggregate(datR$depth.midpoint, by=list(datR$period,datR$loc,datR$site), FUN="max")
#look at diff between max and ave
Ddiff<-depmax$x-datD$A.depth
#see what the highest point is to make plots around
Dhigh<-ifelse(depmax$x>=datD$A.depth,depmax$x,datD$A.depth)
#highest in high density
yuH<-max(Dhigh[1:8])+4
yuL<-max(Dhigh[9:14])+2.5
#make a plot of biomass for high density on period 1

layout.show(ab)
#start by doing all plots across the same depth range
par(mai=c(0,0,0,0))
plot(c(0,1),c(0,1), type="n", xlim=c(0,12.5),ylim=c(yuH,0), xlab=" ", ylab=" ", axes=FALSE,yaxs="i",xaxs="i")
points(datR$bio.mg.cm3[datR$site=="h"&datR$loc=="s"&datR$period==1],datR$depth.midpoint[datR$site=="h"&datR$loc=="s"&datR$period==1], 
		col="royalblue1", pch=19)
points(datR$bio.mg.cm3[datR$site=="h"&datR$loc=="t"&datR$period==1],datR$depth.midpoint[datR$site=="h"&datR$loc=="t"&datR$period==1], 
		col="royalblue4", pch=19)
points(R.mean[,1],D.seqUN[,1], type="l", lwd=2, col="royalblue1")		
polygon(c(R.low[,1],rev(R.high[,1])), c(D.seqUN[,1], rev(D.seqUN[,1])), col="lightskyblue1")

head(R.low[,1])