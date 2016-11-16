#load libraries
library(R2OpenBUGS)
library(coda)

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
rootT$loc.type<-ifelse(rootT$loc<=3,1,
				ifelse(rootT$loc>3&rootT$loc<=6,2,
				ifelse(rootT$loc>6&rootT$loc<=9,1,
				ifelse(rootT$loc>9&rootT$loc<=12,2,NA))))
rootT.spt<-aggregate(rootT$root, by=list(rootT$period, rootT$loc.type, rootT$site), FUN="mean")
colnames(rootT.spt)<-c("period","location","site","Tot.bio")
datR$st.id<-ifelse(datR$loc=="s",1,2)
rootT.spt$spt.id<-seq(1,dim(rootT.spt)[1])
#set up the index for day and type and period
for(i in 1:dim(datR)[1]){
	for(j in 1:dim(rootT.spt)[1]){
		if(rootT.spt$location[j]==datR$st.id[i]&rootT.spt$period[j]==datR$period[i]&rootT.spt$site[j]==datR$Site[i]){
			datR$spt.id[i]<-rootT.spt$spt.id[j]
	
		}
	}
}

#examine the depth that the mode occurs at for each sample point to 
#help with an informative prior
#get the mode for each period and location of observation
lp.mode<-aggregate(datR$bio.mg.cm3, by=list(datR$loc.id,datR$period), FUN="max")
#get the depths associated with this data
lp.row<-list()
lpm.row<-numeric(0)
for(i in 1:42){
	lp.row[[i]]<-which(datR$bio.mg.cm3==lp.mode$x[i]&datR$loc.id==lp.mode$Group.1[i]
				&datR$period==lp.mode$Group.2[i])
	lpm.row[i]<-lp.row[[i]][1]
}

#grab rows with the mode
depM.lp<-datR$mid.norm[lpm.row]
site.lp<-datR$Site[lpm.row]
#add to data frame
lp.mode$depM<-depM.lp
lp.mode$site<-site.lp
#get the average depth for each period and site
p.mode<-aggregate(lp.mode$depM, by=list(lp.mode$Group.2,lp.mode$site), FUN="mean")
p.modesd<-aggregate(lp.mode$depM, by=list(lp.mode$Group.2,lp.mode$site), FUN="sd")
p.mode$low<-p.mode$x-p.modesd$x
p.mode$high<-p.mode$x+p.mode$x

p.mode$high<-ifelse(p.mode$high>1,.999,p.mode$high)

####Need to summarize p.mode for new index by location, site, and period
#add site id and location id to lp.mode
lp.mode$site<-ifelse(lp.mode$Group.1<=6,1,2)
lp.mode$loc.type<-ifelse(lp.mode$Group.1<=3,1,
				ifelse(lp.mode$Group.1>3&lp.mode$Group.1<=6,2,
				ifelse(lp.mode$Group.1>6&lp.mode$Group.1<=9,1,
				ifelse(lp.mode$Group.1>9&lp.mode$Group.1<=12,2,NA))))
#now add spt id
for(i in 1:dim(lp.mode)[1]){
	for(j in 1:dim(rootT.spt)[1]){
		if(rootT.spt$period[j]==lp.mode$Group.2[i]&rootT.spt$site[j]==lp.mode$site[i]&rootT.spt$location[j]==lp.mode$loc.type[i]){
			lp.mode$spt.id[i]<-rootT.spt$spt.id[j]
		}
	}
}
#calculate mode at spt
spt.mode<-aggregate(lp.mode$depM, by=list(lp.mode$spt.id), FUN="mean")
spt.modesd<-aggregate(lp.mode$depM, by=list(lp.mode$spt.id), FUN="sd")

spt.mode$low<-spt.mode$x-spt.modesd$x
spt.mode$high<-spt.mode$x+spt.mode$x

spt.mode$high<-ifelse(spt.mode$high>1,.999,spt.mode$high)

#get the average depth of the active layer for each site and depth
datA<-read.csv("active_depth.csv")
datA$siteid<-ifelse(datA$Site=="l",2,1)
Dave<-aggregate(datA$Frozen.Depth, by=list(datA$period,datA$siteid), FUN="mean")
colnames(Dave)<-c("period","siteid","Ave.deepest")

datA$locid<-ifelse(datA$Loc=="s",1,2)
#set up spt id for active depth
for(i in 1:dim(datA)[1]){
	for(j in 1:dim(rootT.spt)[1]){
		if(rootT.spt$period[j]==datA$period[i]&rootT.spt$location[j]==datA$locid[i]&rootT.spt$site[j]==datA$siteid[i]){
			datA$spt.id[i]<-rootT.spt$spt.id[j]
		
		}
	
	
	}

}

#aggregate the thaw depth by spt
AD.spt<-aggregate(datA$Frozen.Depth,by=list(datA$spt.id), FUN="mean")
colnames(AD.spt)<-c("spt.id", "A.depth")
#add A.depth to root.spt and output for plotting use
rootT.spt$A.depth<-AD.spt$A.depth
write.table(rootT.spt, "total_root_loc_site_period.csv", sep=",", row.names=FALSE)

D.seq<-matrix(rep(0,100*14), ncol=14)
D.seqUN<-matrix(rep(0,100*14), ncol=14)
for(i in 1:14){
	D.seq[,i]<-seq(.1,rootT.spt$A.depth[i],length.out=100)/rootT.spt$A.depth[i]
	D.seqUN[,i]<-seq(.1,rootT.spt$A.depth[i],length.out=100)
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


Rdatalist<-list(Nobs=dim(datR)[1], r.bio=datR$bio.mg.cm3, 
				loc.period=datR$loc.period,depth=datR$mid.norm,Nday=4, Day=datR$period, Dlow=spt.mode$low, Dhigh=spt.mode$high,
				DaySite=datR$DaySite, Ndaysite=7, DaySiteLoc=datR$spt.id, Ndaysiteloc=dim(rootT.spt)[1],
				A.depth=AD.spt$A.depth, depth.est=D.seq)
				
initlist<-list(list(
	Dmode = c(
	0.4971,0.4316,0.1409,0.289,0.9402,
	0.3792,0.2955,0.4009,0.03711,0.08596,
	0.01982,0.1294,0.05219,0.1008),
	beta = c(
	1.408,2.081,2.948,2.878,1.066,
	1.527,3.078,18.76,5.493,2.47,
	17.47,4.412,7.109,5.402),
	r.tot = c(
	7.22,15.89,10.64,16.22,29.05,
	11.39,15.45,14.95,12.61,1.556,
	6.858,27.37,10.29,20.35),
	sig.bio = 1.603),
	list(
	Dmode = c(
	0.3358,0.1366,0.2249,0.1618,0.9426,
	0.5519,0.1454,0.6728,0.02984,0.09967,
	0.03816,0.1173,0.1174,0.1295),
	beta = c(
	1.79,2.516,1.809,1.773,1.092,
	1.273,1.669,1.487,7.682,5.184,
	11.17,5.355,5.239,4.18),
	r.tot = c(
	12.91,10.29,6.6,5.657,29.14,
	7.401,2.975,7.329,17.97,9.468,
	9.644,23.09,18.03,16.88),
	sig.bio = 1.728),
	list(
	Dmode = c(
	0.8412,0.4699,0.1727,0.2418,0.9443,
	0.4566,0.3141,0.5824,0.02884,0.08139,
	0.05799,0.1445,0.06872,0.1481),
	beta = c(
	1.173,1.92,3.748,2.061,1.056,
	1.734,3.637,12.48,9.566,4.281,
	5.851,4.566,1.554,3.879),
	r.tot = c(
	8.461,13.41,16.42,9.697,25.63,
	14.17,20.45,1.046,27.74,3.505,
	9.974,26.71,2.017,16.3),
	sig.bio = 1.522))
				
				
initmodel<-bugs(data=Rdatalist,model.file="c:\\Users\\hkropp\\Documents\\GitHub\\Siberia_root_profile\\Distrubution_model_code.txt",
				inits=initlist,parameters.to.save=c("alpha","beta","deviance","sig.bio", "mu.bio", "Dmode", "bio.est",
													"Rbeta","r.med", "r.mean", "r.mode","r.tot","r.rep", "med.diff"),
				n.iter=4000,n.chains=3,n.burnin=2000,n.thin=25,
				working.directory="c:\\Users\\hkropp\\Google Drive\\root_analysis",
				debug=TRUE, codaPkg=TRUE)