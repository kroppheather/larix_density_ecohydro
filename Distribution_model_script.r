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

#examine the depth that the mode occurs at for each sample point to 
#help with an informative prior
#get the mode for each period and location of observation
lp.mode<-aggregate(datR$bio.mg.cm3, by=list(datR$loc.id,datR$period), FUN="max")
#get the depths associated with this data
lp.row<-list()
lpm.row<-numeric(0)
for(i in 1:42){
	lp.row[[i]]<-which(datR$bio.mg.cm3==lp.mode$x[i]&datR$loc.id==lp.mode$Group.1[i]&datR$period==lp.mode$Group.2[i])
	lpm.row[i]<-lp.row[[i]][1]
}

depM.lp<-datR$mid.norm[lpm.row]
site.lp<-datR$Site[lpm.row]
lp.mode$depM<-depM.lp
lp.mode$site<-site.lp
#get the average depth for each period
p.mode<-aggregate(lp.mode$depM, by=list(lp.mode$Group.2,lp.mode$site), FUN="mean")
p.modesd<-aggregate(lp.mode$depM, by=list(lp.mode$Group.2,lp.mode$site), FUN="sd")
p.mode$low<-p.mode$x-p.modesd$x
p.mode$high<-p.mode$x+p.mode$x

p.mode$high<-ifelse(p.mode$high>1,.999,p.mode$high)
#set up datalist for the model
#data variables:
#Nobs: number of observations of root biomass
#r.bio: root biomass observation at each depth
#r.tot: total root biomass in the profile (sum of r.bio for each loc and sample depth)
#loc.period: Day index for sample period varies by root biomass observations
#depth: relative depth in the active layer for each root biomass observation
#Nday: number of sample periods
Rdatalist<-list(Nobs=dim(datR)[1], r.bio=datR$bio.mg.cm3,r.tot=rootT$root, 
				loc.period=datR$loc.period,depth=datR$mid.norm,Nday=4, Day=datR$period, Dlow=p.mode$low, Dhigh=p.mode$high,
				DaySite=datR$DaySite, Ndaysite=7)
				
initslist<-list(list(beta=c(2,2,2,2,2,2,2),Dmode=c(.63,.38,.19,.27,.11,.11,.08), sig.bio=3), 
				list(beta=c(3,3,3,33,3,3),Dmode=c(.64,.39,.2,.28,.12,.12,.09), sig.bio=1),
				list(beta=c(2.5,2.5,2.5,2.5,2.5,2.5,2.5),Dmode=c(.60,.35,.16,.26,.10,.10,.05), sig.bio=2))
				
				
initmodel<-bugs(data=Rdatalist,model.file="c:\\Users\\hkropp\\Documents\\GitHub\\Siberia_root_profile\\Distrubution_model_code.txt",
				inits=initslist,parameters.to.save=c("alpha","beta","deviance","sig.bio", "mu.bio", "Dmode", "Rbeta"),
				n.iter=4000,n.chains=3,n.burnin=1000,n.thin=1,
				working.directory="c:\\Users\\hkropp\\Google Drive\\root_analysis",
				debug=TRUE, codaPkg=TRUE)