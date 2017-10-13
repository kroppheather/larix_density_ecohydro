###########################################################################
###########################################################################
############## Created by Heather Kropp in October 2017      ##############
############## Creates plots of resitivity data              ##############
###########################################################################
###########################################################################
library(interp)
library(plyr)

#################################################################
####read in data                                          #######
#################################################################

#read in info about resistivity files to process
datRQ <- read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\resistivity\\resistivity_quality.csv")


#read in resistivity files
datR <- list()
for(i in 1:dim(datRQ)[1]){
	datR[[i]] <- read.csv(paste0("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\resistivity\\file_out\\",datRQ$filename[i],".csv"))
}


#read in soil moisture
datVWC <- read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\individual_data\\vwc_resitivity.csv")

#read in thaw depth
datTD <- read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\individual_data\\resitivity_thaw17.csv")


#directory to save plots in

plotDI <- "c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\resistivity_plot\\out_R"


#################################################################
####deal with any electordes measured from other side     #######
####of the start of the transect tape                     #######
#################################################################
to.fix <- which(datRQ$reversed.start=="y")

for(i in 1:length(to.fix)){
	datR[[to.fix[i]]]$X <- datRQ$transect.start[to.fix[i]]-datR[[to.fix[i]]]$X 

}

#################################################################
####offset paired data on transects that didn't           #######
#### start at zero                                        #######
#################################################################
to.offs <- data.frame(filename=as.character(datRQ$filename[which(datRQ$transect.start>0&datRQ$reversed.start=="n")]))
to.offs$offD <-datRQ$transect.start[which(datRQ$transect.start>0&datRQ$reversed.start=="n")]/100
#all distances are in cm in both files so divide all by 100
datVWC$x <- datVWC$transect.distance/100
datTD$x <- datTD$distance.on.transect/100
datVWC$file.name <- as.character(datVWC$file.name)
datTD$file_label <- as.character(datTD$file_label)

#now offest distances that don't start at zero
for(i in 1:dim(to.offs)[1]){
	datVWC$x[datVWC$file.name==to.offs$filename[i]] <- datVWC$x[datVWC$file.name==to.offs$filename[i]]-to.offs$offD[i]
	datTD$x[datTD$file_label==to.offs$filename[i]] <- datTD$x[datTD$file_label==to.offs$filename[i]]-to.offs$offD[i]
}

#################################################################
####interpolate for plots                                 #######
#################################################################

#set limits for plotting
#get range for plots

yh <- numeric(0)
yl <- numeric(0)
xh <- numeric(0)
xl <- numeric(0)
rspInt <- list()
res.sp <- list()

for(i in 1:dim(datRQ)[1]){
	yh[i] <-0
	yl[i] <-min(datR[[i]]$Depth)
	xl[i] <- 0
	xh[i] <- max(datR[[i]]$X)

	rspInt <- interp(x=datR[[i]]$X, y=datR[[i]]$Depth, z=datR[[i]]$Resistivity, xo=seq(xl[i],xh[i], length.out=600),
					yo=seq(yl[i],yh[i], length.out=600), extrap=FALSE)

	#deconstruct into a vector
	res.sp[[i]]  <- data.frame(x=rep(rspInt$x, times=600), y=rep(rspInt$y, each=600), z=as.vector(rspInt$z))	


}	
#################################################################
####create color for plots                                #######
#################################################################

a <-colorRampPalette(c("royalblue4","lightcyan2","forestgreen", "darkorange3", "firebrick3"))(11)

#################################################################
####match resistivity to plot color                       #######
#################################################################
DBrange <- list()
lrdb <- list()
logbreak <-list()
rdbreak <- list()
for(i in 1:dim(datRQ)[1]){
	#make breaks 
	DBrange[[i]] <-range(na.omit(res.sp[[i]]$z))
	#pull the upper value because want breaks above
	DBrange[[i]][1] <- round_any(DBrange[[i]][1],10, ceiling)
	DBrange[[i]][2] <- round_any(DBrange[[i]][2],10, ceiling)
	#take the log of these range values
	lrdb[[i]] <- log(DBrange[[i]])
	logbreak[[i]]<-seq(lrdb[[i]][1],lrdb[[i]][2] , length.out=11)
	rdbreak[[i]]<- exp(logbreak[[i]])
	

res.sp[[i]]$col <- ifelse(is.na(res.sp[[i]]$z), "white",
			ifelse(res.sp[[i]]$z<=rdbreak[[i]][1],a[1],
			ifelse(res.sp[[i]]$z>rdbreak[[i]][1]&res.sp[[i]]$z<=rdbreak[[i]][2],a[2],
			ifelse(res.sp[[i]]$z>rdbreak[[i]][2]&res.sp[[i]]$z<=rdbreak[[i]][3],a[3],
			ifelse(res.sp[[i]]$z>rdbreak[[i]][3]&res.sp[[i]]$z<=rdbreak[[i]][4],a[4],
			ifelse(res.sp[[i]]$z>rdbreak[[i]][4]&res.sp[[i]]$z<=rdbreak[[i]][5],a[5],
			ifelse(res.sp[[i]]$z>rdbreak[[i]][5]&res.sp[[i]]$z<=rdbreak[[i]][6],a[6],
			ifelse(res.sp[[i]]$z>rdbreak[[i]][6]&res.sp[[i]]$z<=rdbreak[[i]][7],a[7],
			ifelse(res.sp[[i]]$z>rdbreak[[i]][7]&res.sp[[i]]$z<=rdbreak[[i]][8],a[8],
			ifelse(res.sp[[i]]$z>rdbreak[[i]][8]&res.sp[[i]]$z<=rdbreak[[i]][9],a[9],
			ifelse(res.sp[[i]]$z>rdbreak[[i]][9]&res.sp[[i]]$z<=rdbreak[[i]][10],a[10],
			ifelse(res.sp[[i]]$z>rdbreak[[i]][10]&res.sp[[i]]$z<=rdbreak[[i]][11],a[11],"black"
			))))))))))))
}
#################################################################
####potentially unused                                    #######
#################################################################

#now make an interpolation for the figure

#break.fig <- data.frame(x=rep(c(0,.5,1), each=11), y=rep(seq(0,1, length=11), times=3), z=rep(lab.break, times=3)	)



#################################################################
####subset vwc and match soil vwc to plot color           #######
#################################################################
#subset vwc
vwcS <- list()
for(i in 1:dim(datRQ)[1]){
	vwcS[[i]] <- datVWC[datVWC$file.name==datRQ$filename[i],]
}
#create color range for soil moisture
vrange <- list()
lvbreaks <- list()
vupper <-list()
for(i in 1:dim(datRQ)[1]){

	vrange[[i]] <- range(na.omit(vwcS[[i]]$Mineral.VWC))
	#break up into 11
	vrange[[i]][1] <-0.01
	vrange[[i]][2] <- round_any(vrange[[i]][2],.1, ceiling)
	lvbreaks[[i]] <- seq(log(vrange[[i]][1]), log(vrange[[i]][2]), length=11)
	vupper[[i]] <- exp(lvbreaks[[i]])


vwcS[[i]]$col <- ifelse(vwcS[[i]]$Mineral.VWC<=vupper[[i]][1],a[11],
			ifelse(vwcS[[i]]$Mineral.VWC>vupper[[i]][1]&vwcS[[i]]$Mineral.VWC<=vupper[[i]][2],a[10],
			ifelse(vwcS[[i]]$Mineral.VWC>vupper[[i]][2]&vwcS[[i]]$Mineral.VWC<=vupper[[i]][3],a[9],
			ifelse(vwcS[[i]]$Mineral.VWC>vupper[[i]][3]&vwcS[[i]]$Mineral.VWC<=vupper[[i]][4],a[8],
			ifelse(vwcS[[i]]$Mineral.VWC>vupper[[i]][4]&vwcS[[i]]$Mineral.VWC<=vupper[[i]][5],a[7],
			ifelse(vwcS[[i]]$Mineral.VWC>vupper[[i]][5]&vwcS[[i]]$Mineral.VWC<=vupper[[i]][6],a[6],
			ifelse(vwcS[[i]]$Mineral.VWC>vupper[[i]][6]&vwcS[[i]]$Mineral.VWC<=vupper[[i]][7],a[5],
			ifelse(vwcS[[i]]$Mineral.VWC>vupper[[i]][7]&vwcS[[i]]$Mineral.VWC<=vupper[[i]][8],a[4],
			ifelse(vwcS[[i]]$Mineral.VWC>vupper[[i]][8]&vwcS[[i]]$Mineral.VWC<=vupper[[i]][9],a[3],
			ifelse(vwcS[[i]]$Mineral.VWC>vupper[[i]][9]&vwcS[[i]]$Mineral.VWC<=vupper[[i]][10],a[2],
			ifelse(vwcS[[i]]$Mineral.VWC>vupper[[i]][10]&vwcS[[i]]$Mineral.VWC<=vupper[[i]][11],a[1],"black")))))))))))
}


#################################################################
####subset thaw depth                                     #######
#################################################################
tdS <- list()
for(i in 1:dim(datRQ)[1]){
	tdS[[i]] <- datTD[datTD$file_label==datRQ$filename[[i]],]
}


#################################################################
####make resistivity plots                                #######
#################################################################
#get depth coordinates
yl <- numeric(0)
yh <- numeric(0)
xl <- numeric(0)
yh <- numeric(0)
for(i in 1:dim(datRQ)[1]){
	yl[i] <- max(tdS[[i]]$thaw.depth/100,abs(datR[[i]]$Depth))
	yh[i] <- 0
	xl[i] <- 0
	xh[i] <- max(datR[[i]]$X)
}
yoff <- .05
xoff <- .2
fig.yup<- 1/11
fig.lab<- seq(1,11)*fig.yup



for(i in 1:dim(datRQ)[1]){			
	jpeg(paste0(plotDI, "\\",datRQ$filename[i],".jpg") , width=1500, height=700, units="px", quality=100)			
	ab <- layout(matrix(seq(1,3), ncol=3), width=c(lcm(36), lcm(4), lcm(4)), height=c(lcm(15), lcm(15), lcm(15)))
	layout.show(ab)
	par(mai=c(0,0,0,0))		
	plot(c(0,1), c(0,1), type="n", ylim=c(-(yl[i]+yoff),yh[i]), xlim=c(xl[i],xh[i]), ylab=" ", xlab=" ", axes=FALSE, yaxs="i", xaxs="i")		
	#plot resistivity
	points(res.sp[[i]]$x,res.sp[[i]]$y,pch=19, col=res.sp[[i]]$col)
	#plot thaw depth
	points(tdS[[i]]$x, -tdS[[i]]$thaw.depth/100, type="l", lwd=5, col="thistle4")
	#axes
	axis(2, seq(-1,0, by=.05), las=2, cex.axis=2.5)
	mtext("Depth (m)", side=2, line=8, cex=3)
	axis(1, seq(0.1,2.5, by=.1), cex.axis=2.5)
	mtext("Transect distance (m)", side=1, line=5, cex=3)
	#soil moisture
	for(j in 1:dim(vwcS[[i]])[1]){
		polygon(c(vwcS[[i]]$x[j],vwcS[[i]]$x[j],vwcS[[i]]$x[j]+.1,vwcS[[i]]$x[j]+.1), c(-.01,0,0,-.01),
				col=vwcS[[i]]$col[j],border=FALSE)

	}
	#add legend
	legend(1,-.45, c("thaw depth"), lwd=5, col="thistle4", bty="n", cex=3)

	par(mai=c(0,.5,0,.5))
	plot(c(0,1), c(0,1), type="n", ylim=c(0,1), xlim=c(0,1), ylab=" ", xlab=" ", axes=FALSE, yaxs="i", xaxs="i")
	#plot the restivity legend
	for(j in 1:length(a)){
		polygon(c(0,0,1,1), c(fig.yup*(j-1),fig.yup*j,fig.yup*j,fig.yup*(j-1)), col=a[j], border=FALSE)

	}
	#axis labels
	axis(4, c(0,fig.lab), lab=c(0,round(rdbreak[[i]],0)), las=2, cex.axis=2.5 )
	mtext("Resistivity ", side=4, line=8, cex=3)
	#plot the vwc legend
	par(mai=c(0,1,0,0))
	plot(c(0,1), c(0,1), type="n", ylim=c(0,1), xlim=c(0,1), ylab=" ", xlab=" ", axes=FALSE, yaxs="i", xaxs="i")

	for(j in 1:length(a)){
		polygon(c(0,0,1,1), c(fig.yup*(j-1),fig.yup*j,fig.yup*j,fig.yup*(j-1)), col=a[j], border=FALSE)

	}

	axis(4, c(0,fig.lab), c(round(rev(vupper[[i]]),3),0), las=2, cex.axis=2.5 )
	mtext("Volumetric soil moisture ", side=4, line=9, cex=3)
	dev.off()

}

#################################################################
####compare restivity to soil moisture                    #######
#################################################################
#start by being conservative and don't include anything
#with a quality flag of 2 until they can be investigated
comp1 <- which(datRQ$qualityID!=2,)
sitesc1 <- datRQ[comp1,]
#c3, d1 have increments in 5 and are facing the side
#rest are increments of 10
siteflag <- ifelse(sitesc1=="C3",0,
			ifelse(sitesc1=="D1",0,1))
		
vwSc1 <- list()
resc1 <- list()
comp1ALL <- list()
datAR <-list()
for(i in 1:length(comp1)){
	vwSc1[[i]] <- vwcS[[comp1[i]]]
	resc1[[i]] <-datR[[comp1[i]]][datR[[comp1[i]]]$Depth>-0.05,]
	resc1[[i]]$filename <- sitesc1$filename[i]
	#aggregate all across depth
	resc1[[i]]$X <- round_any(resc1[[i]]$X,.05, floor)
	datAR[[i]]<- aggregate(resc1[[i]]$Resistivity, by=list(resc1[[i]]$filename,resc1[[i]]$X), FUN="mean")
	colnames(datAR[[i]])<-c("filename", "X", "Resistivity")
	
	#assume the value in the middle is the same as the start so just match
	#or will average and than assume the middle represents the meas at the start

	if(siteflag[i]==1){
		vwSc1[[i]]$X <-vwSc1[[i]]$x+.05
	}else{vwSc1[[i]]$X <-vwSc1[[i]]$x}
	
	datAR[[i]]$X <- as.character(datAR[[i]]$X)
	vwSc1[[i]]$X <- as.character(vwSc1[[i]]$X)
	comp1ALL[[i]] <- join( vwSc1[[i]],datAR[[i]], by="X", type="inner")
	}

compC1 <- ldply(comp1ALL,data.frame)

#there is one really low resitivity that I think is a mistake due to some sort of interferience
#and it is a really huge outlier in the resistivity range
#correct
compC1 <- compC1[compC1$Resistivity>5,]

jpeg(paste0(plotDI, "\\","swc_resist_comp.jpg") , width=1500, height=700, units="px", quality=100)
par(mai=c(1,1,1,1))
plot(log(compC1$Resistivity),compC1$Mineral.VWC, pch=19, ylab="Observed SWC (m3/m3)", xlab="Log Resistivity", cex.lab=2,
		axes=FALSE)	
axis(1, seq(0,9), cex.axis=1.5)
axis(2, seq(-.1,.9, by=.1), cex.axis=1.5, las=2)	
fitC1 <- lm(compC1$Mineral.VWC~log(compC1$Resistivity))
summary(fitC1)
abline(fitC1, lwd=2)

text(6.5,.65,paste0("SWC = ", round(fitC1$coefficients[1],2), "+ ",round(fitC1$coefficients[2],2), "* log(Resistivity)"),
		cex=2)
text(6.5,.55,paste0("R2 = ", round(summary(fitC1)$r.squared,2)),
		cex=2)
dev.off()		