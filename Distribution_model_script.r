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
#set up 
rootT$loc.type<-ifelse(rootT$loc<=3,1,
				ifelse(rootT$loc>3&rootT$loc<=6,2,
				ifelse(rootT$loc>6&rootT$loc<=9,1,
				ifelse(rootT$loc>9&rootT$loc<=12,2,NA))))
rootT.spt<-aggregate(rootT$root, by=list(rootT$period, rootT$loc.type, rootT$site), FUN="mean")
colnames(rootT.spt)<-c("period","location","site")
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

####Need to summarize p.mode for new index by location site and period


#get the average depth of the active layer for each site and depth
datA<-read.csv("active_depth.csv")
datA$siteid<-ifelse(datA$Site=="l",2,1)
Dave<-aggregate(datA$Frozen.Depth, by=list(datA$period,datA$siteid), FUN="mean")
colnames(Dave)<-c("period","siteid","Ave.deepest")
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
				loc.period=datR$loc.period,depth=datR$mid.norm,Nday=4, Day=datR$period, Dlow=p.mode$low, Dhigh=p.mode$high,
				DaySite=datR$DaySite, Ndaysite=7, Adeep=Dave$Ave.deepest,DaySiteLoc=datR$spt.id, Ndaysiteloc=dim(rootT.spt)[1])
				
initlist<-list(list(
					Dmode = c(
					0.9936,0.6018,0.2041,0.3438,0.05979,
					0.104,0.1277),
					beta = c(
					1.004,1.257,2.7,1.911,4.934,
					4.644,4.531),r.tot=c(10,8,9,9,14,8,11),
					sig.bio = 1.563),
					list(
					Dmode = c(
					0.8974,0.5002,0.3488,0.2259,0.05694,
					0.07278,0.06833),
					beta = c(
					1.112,1.321,2.065,2.118,4.459,
					4.869,6.078),r.tot=c(11,9,10,10,15,9,12),
					sig.bio = 1.439),
					list(
					Dmode = c(
					0.9814,0.3516,0.2427,0.1938,0.05526,
					0.1764,0.08517),r.tot=c(9,7,8,8,13,7,10),
					beta = c(
					1.012,1.532,2.431,2.598,5.045,
					3.31,4.971),
					sig.bio = 1.414))
				
				
initmodel<-bugs(data=Rdatalist,model.file="c:\\Users\\hkropp\\Documents\\GitHub\\Siberia_root_profile\\Distrubution_model_code.txt",
				inits=initlist,parameters.to.save=c("alpha","beta","deviance","sig.bio", "mu.bio", "Dmode", "Rbeta","Dmed", "Dmean", "r.tot","r.rep"),
				n.iter=4000,n.chains=3,n.burnin=2000,n.thin=25,
				working.directory="c:\\Users\\hkropp\\Google Drive\\root_analysis",
				debug=TRUE, codaPkg=TRUE)