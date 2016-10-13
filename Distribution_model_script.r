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
###FIX this
for(i in 1:dim(datR)[1]){
	for(j in 1:dim(rootT)[1]){
	if(rootT$loc[j]==datR$loc.id[i]&rootT$period[j]==datR$period[i])
	datR$loc.period[i]<-rootT$loc.period[j]
	}
}


#set up datalist for the model
#data variables:
#Nobs: number of observations of root biomass
#r.bio: root biomass observation at each depth
#r.tot: total root biomass in the profile (sum of r.bio for each loc and sample depth)
#loc.period: Day index for sample period varies by root biomass observations
#depth: relative depth in the active layer for each root biomass observation
#Nday: number of sample periods
Rdatalist<-list(Nobs=dim(datR)[1], r.bio=datR$bio.mg.cm3,r.tot=rootT$root, 
				loc.period=datR$loc.period,depth=datR$mid.norm,Nday=4, Day=datR$period)
				
initslist<-list(list(alpha=2,beta=5, sig.bio=3), 
				list(alpha=3,beta=6, sig.bio=1),
				list(alpha=2.5,beta=4, sig.bio=2))
				
				
initmodel<-bugs(data=Rdatalist,model.file="c:\\Users\\hkropp\\Documents\\GitHub\\Siberia_root_profile\\Distrubution_model_code.txt",
				inits=initslist,parameters.to.save=c("alpha","beta","deviance","sig.bio"),
				n.iter=4000,n.chains=3,n.burnin=1000,n.thin=1,
				working.directory="c:\\Users\\hkropp\\Google Drive\\root_analysis",
				debug=TRUE, codaPkg=TRUE)