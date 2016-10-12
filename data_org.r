#######this script organizes data and excludes any issues with data #########
####### output can be used for analysis                             #########
library(plyr)
#set working directory
setwd("c:\\Users\\hkropp\\Google Drive\\viper_energy\\Root\\csv_forR")

#read in data set
datr<-read.csv("compiled data.csv")

#check all sample dates for each site
sampleN<-unique(data.frame(doy=datr$DOY, site=datr$site))

#check that there are no duplicates

check<-aggregate(datr$bio.mg.cm3,
			by=list(datr$depth.midpoint,datr$status,datr$type,datr$rep,datr$loc,datr$DOY,datr$site),
			FUN="length")
			
#filter out dead observations because not interested in it
#and dead were not measured in later sampling periods
check.l<-check[check$Group.2=="l",]

#not looking at course roots for initial analyses as well
#just look at fine
fine.ch<-check.l[check.l$Group.3=="f",]

#see where there are duplicate observations
fine.ch[fine.ch$x>1,]
			
						
#just exclude duplicate observations because
#it is not possible to distinguish issues
fine.list<-fine.ch[fine.ch$x==1,]			
colnames(fine.list)<-c("depth.midpoint", "status", "type", "rep", "loc", "DOY", "site", "length")
#now need to pull out data for these values from orignal
datr.sub<-data.frame(datr[,1:7], datr[,16:17])
fr.df<-join(fine.list,datr.sub, by=c("DOY", "site", "status", "loc","rep","type","depth.midpoint"),type="left")

#set up a sample time frame index for the four sampling periods
sampleN$ind<-c(1,1,1,1,2,3,3,4,4)
	

for(i in 1:dim(fr.df)[1]){
	for(j in 1:dim(sampleN)[1]){
		if(sampleN$doy[j]==fr.df$DOY[i]){
			fr.df$period[i]<-sampleN$ind[j]
		}
		
	}
}

#plot to get an idea of data
pw<-windows(14)
yrange<-list()
for(i in 1:4){
yrange[[i]]<-range(fr.df$bio.mg.cm3[fr.df$period==i])
pw[i]<-windows(14)
plot(c(0,1),c(0,1), xlim=c(0,yrange[[i]][2]), ylim=c(100,0), type="n", xlab=" Root biomass (mg/cm3)", ylab="midpoint (cm)",
main=paste(i))
#high density
points(fr.df$bio.mg.cm3[fr.df$site=="h"&fr.df$loc=="t"&fr.df$period==i], fr.df$depth.midpoint[fr.df$site=="h"&fr.df$loc=="t"&fr.df$period==i],
					col="forestgreen", pch=16)

points(fr.df$bio.mg.cm3[fr.df$site=="h"&fr.df$loc=="s"&fr.df$period==i], fr.df$depth.midpoint[fr.df$site=="h"&fr.df$loc=="s"&fr.df$period==i],
					col="olivedrab3", pch=16)
#low density
points(fr.df$bio.mg.cm3[fr.df$site=="l"&fr.df$loc=="t"&fr.df$period==i], fr.df$depth.midpoint[fr.df$site=="l"&fr.df$loc=="t"&fr.df$period==i],
					col="royalblue4", pch=16)

points(fr.df$bio.mg.cm3[fr.df$site=="l"&fr.df$loc=="s"&fr.df$period==i], fr.df$depth.midpoint[fr.df$site=="l"&fr.df$loc=="s"&fr.df$period==i],
					col="deepskyblue", pch=16)
}
#generate IDs for location and tree vs shrub
Nloc<-unique(data.frame(loc=fr.df$loc, site=fr.df$site, rep=fr.df$rep))
Nloc$loc.id<-seq(1,dim(Nloc)[1])
Nloc$type.id<-ifelse(Nloc$site=="h",1,2)
#set up id to go with root biomass data
for(i in 1:dim(fr.df)[1]){
	for(j in 1: dim(Nloc)[1]){
		if(Nloc$site[j]==fr.df$site[i]&Nloc$rep[j]==fr.df$rep[i]&Nloc$loc[j]==fr.df$loc[i]){
		fr.df$loc.id[i]<-Nloc$loc.id[j]
			fr.df$type.id[i]<-Nloc$type.id[j]
	
		}
		
	}

}

##organize active layer depth data

datA<-read.csv("active_depth.csv")

#set up same ids as above for sample period, location and tree/shrub

for(i in 1:dim(datA)[1]){
	for(j in 1:dim(sampleN)[1]){
		if(sampleN$doy[j]==datA$DOY[i]){
			datA$period[i]<-sampleN$ind[j]
		}
		
	}
}

#set up id to for locations
for(i in 1:dim(datA)[1]){
	for(j in 1: dim(Nloc)[1]){
		if(Nloc$site[j]==datA$Site[i]&Nloc$rep[j]==datA$Rep[i]&Nloc$loc[j]==datA$Loc[i]){
			datA$loc.id[i]<-Nloc$loc.id[j]
			datA$type.id[i]<-Nloc$type.id[j]

		}
		
	}

}

#create a normalized midpoint based on total active layer depth
for(i in 1:dim(fr.df)[1]){
	for(j in 1:dim(datA)[1]){
		if(fr.df$period[i]==datA$period[j]&fr.df$loc.id[i]==datA$loc.id[j]){
		fr.df$mid.norm[i]<-round(fr.df$depth.midpoint[i]/datA$Frozen.Depth[j],2)
		fr.df$deepest[i]<-datA$Frozen.Depth[j]
		}
	
	}


}

write.table(fr.df, "fine_root_out.csv", sep=",", row.names=FALSE)
