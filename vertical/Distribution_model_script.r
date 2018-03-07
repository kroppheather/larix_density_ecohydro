#load libraries
library(R2OpenBUGS)
library(coda)
library(plyr)

#set working directory
setwd("c:\\Users\\hkropp\\Google Drive\\root_analysis")
#read in data
datR<-read.csv("fine_root_out.csv")
#aggregate data for total root biomass across the profile
rootT<-aggregate(datR$bio.mg.cm3, by=list(datR$loc.id, datR$period), FUN="sum")
colnames(rootT)<-c("loc", "period","root")
#need a location index for period and loc because is a sparse array
#this location index preserves both rep and location type
rootT$loc.period<-seq(1,42)
#create loc.period index for the biomass obs
for(i in 1:dim(datR)[1]){
	for(j in 1:dim(rootT)[1]){
	if(rootT$loc[j]==datR$loc.id[i]&rootT$period[j]==datR$period[i])
	datR$loc.period[i]<-rootT$loc.period[j]
	}
}
#loc.period 1-6 is high density (site 1) and 7-12 is low density (site 2)
datR$Site<-ifelse(datR$loc.id<=6,1,2)

#get list of day and site
DaySiteTable<-aggregate(datR$bio.mg.cm3, by=list(datR$period,datR$Site), FUN="mean")
DaySiteTable$daySind<-seq(1,7)
#set up index for daysite
for(i in 1:dim(datR)[1]){
	for(j in 1:dim(DaySiteTable)[1]){
		if(datR$Site[i]==DaySiteTable$Group.2[j]&datR$period[i]==DaySiteTable$Group.1[j]){
			datR$DaySite[i]<-DaySiteTable$daySind[j]
		}
	}

}
#create an index that varies by  period,site, loc,
#set up a table that varies by this
#also look at how R tot varies within these groups
#loc.type 1=shrub 2=tree
#site 1 is high density and 2 is low density
rootT$site<-ifelse(rootT$loc<=6,1,2)


rootT.spt<-aggregate(rootT$root, by=list(rootT$period,  rootT$site), FUN="sum")
colnames(rootT.spt)<-c("period","site","Tot.bio")

rootT.spt$spt.id<-seq(1,dim(rootT.spt)[1])
#set up the index for day and type and period
for(i in 1:dim(datR)[1]){
	for(j in 1:dim(rootT.spt)[1]){
		if(rootT.spt$period[j]==datR$period[i]&rootT.spt$site[j]==datR$Site[i]){
			datR$spt.id[i]<-rootT.spt$spt.id[j]
	
		}
	}
}

#get the average depth of the active layer for each site and depth
datA<-read.csv("active_depth.csv")
datA$siteid<-ifelse(datA$Site=="l",2,1)
Dave<-aggregate(datA$Frozen.Depth, by=list(datA$period,datA$siteid), FUN="mean")
colnames(Dave)<-c("period","site","Ave.deepest")
#join to rootT.spt
Rall<-join(rootT.spt,Dave,by=c("period","site"), type="left")
write.table(Rall,"c:\\Users\\hkropp\\Google Drive\\root_analysis\\siteDay\\Depth.csv",sep=",",row.names=FALSE)
D.seq<-matrix(rep(0,100*7), ncol=7)
D.seqUN<-matrix(rep(0,100*7), ncol=7)
for(i in 1:7){
	D.seq[,i]<-seq(.1,Rall$Ave.deepest[i],length.out=100)/Rall$Ave.deepest[i]
	D.seqUN[,i]<-seq(.1,Rall$Ave.deepest[i],length.out=100)
}

#set up datalist for the model
#data variables:
#Nobs: number of observations of root biomass
#r.bio: root biomass observation at each depth
#r.tot: total root biomass in the profile (sum of r.bio for each loc and sample depth)
#loc.period: Day index for sample period varies by root biomass observations
#depth: relative depth in the active layer for each root biomass observation
#Nday: number of sample periods


####get a better idea of starting values for root tot
#set up site for root tot
rootT$site<-ifelse(rootT$loc<=6,1,2)
Rdaysite<-aggregate(rootT$root, by=list(rootT$period,rootT$site), FUN="mean")


Rdatalist<-list(Nobs=dim(datR)[1], r.bio=datR$bio.mg.cm3
				,depth=datR$mid.norm, 
				DaySite=datR$DaySite, Ndaysite=7, 
				A.depth=Rall$Ave.deepest, depth.est=D.seq)
				
initlist<-list(list(
	Dmode = c(
	0.4971,0.4316,0.1409,0.289,0.9402,
	0.3792,0.2955),
	beta = c(
	1.408,2.081,2.948,2.878,1.066,
	1.527,3.078),
	r.tot = c(
	7.22,15.89,10.64,16.22,29.05,
	11.39,15.45),
	sig.bio = 1.603),
	list(
	Dmode = c(
	0.3358,0.1366,0.2249,0.1618,0.9426,
	0.5519,0.1454),
	beta = c(
	1.79,2.516,1.809,1.773,1.092,
	1.273,1.669),
	r.tot = c(
	12.91,10.29,6.6,5.657,29.14,
	7.401,2.975),
	sig.bio = 1.728),
	list(
	Dmode = c(
	0.8412,0.4699,0.1727,0.2418,0.9443,
	0.4566,0.3141),
	beta = c(
	1.173,1.92,3.748,2.061,1.056,
	1.734,3.637),
	r.tot = c(
	8.461,13.41,16.42,9.697,25.63,
	14.17,20.45),
	sig.bio = 1.522))
				
				
initmodel<-bugs(data=Rdatalist,model.file="c:\\Users\\hkropp\\Documents\\GitHub\\Siberia_root_profile\\vertical\\Distrubution_model_code.txt",
				inits=initlist,parameters.to.save=c("alpha","beta","deviance","sig.bio", "mu.bio", "Dmode", "bio.est",
													"Rbeta","r.med", "r.mean", "r.mode","r.tot","r.rep", "med.diff"),
				n.iter=4000,n.chains=3,n.burnin=2000,n.thin=25,
				working.directory="c:\\Users\\hkropp\\Google Drive\\root_analysis\\siteDay",
				debug=TRUE, codaPkg=TRUE)
				
				
		