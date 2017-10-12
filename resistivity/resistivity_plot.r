###########################################################################
###########################################################################
############## Created by Heather Kropp in October 2017      ##############
############## Creates plots of resitivity data              ##############
###########################################################################
###########################################################################
library(interp)
library(plyr)
#read in resistivity file
rDB1 <- read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\resistivity\\DB1_model.csv")

#read in soil moisture
datVWC <- read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\individual_data\\vwc_resitivity.csv")

#directory to save plots in

plotDI <- "c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\resistivity_plot"

#subset
vDB1 <-datVWC[datVWC$file.name=="DB1",]
#calculate correct dist in m (start at 20 because broken tape)
vDB1$x <- (vDB1$transect.distance-20)/100

#get range of resistivity
range(rDB1$Resistivity)
#set limits for plotting
#get range for plots
yh <-0
yl <-min(rDB1$Depth)
xl <- 0
xh <- max(rDB1$X)

xo<-seq(xl,xh, length.out=600)
yo<-seq(yl,yh, length.out=600)



DB1i <- interp(x=rDB1$X, y=rDB1$Depth, z=rDB1$Resistivity, xo=seq(xl,xh, length.out=600),
				yo=seq(yl,yh, length.out=600), extrap=FALSE)

#deconstruct into a vector
DB1sp <- data.frame(x=rep(DB1i$x, times=600), y=rep(DB1i$y, each=600), z=as.vector(DB1i$z))	

DBrange <-range(na.omit(DB1sp$z))
	
#create color

a <-colorRampPalette(c("royalblue4","lightcyan2","forestgreen", "darkorange3", "firebrick3"))(11)
#make breaks 
DBrange <-range(na.omit(DB1sp$z))
#pull the upper value because want breaks above
DBrange[1] <- round_any(DBrange[1],10, ceiling)
DBrange[2] <- round_any(DBrange[2],10, ceiling)
lrdb <- log(DBrange)
logbreak<-seq(lrdb[1],lrdb[2] , length.out=11)
rdbreak<- exp(logbreak)
	

DB1sp$col <- ifelse(is.na(DB1sp$z), "white",
			ifelse(DB1sp$z<=rdbreak[1],a[1],
			ifelse(DB1sp$z>rdbreak[1]&DB1sp$z<=rdbreak[2],a[2],
			ifelse(DB1sp$z>rdbreak[2]&DB1sp$z<=rdbreak[3],a[3],
			ifelse(DB1sp$z>rdbreak[3]&DB1sp$z<=rdbreak[4],a[4],
			ifelse(DB1sp$z>rdbreak[4]&DB1sp$z<=rdbreak[5],a[5],
			ifelse(DB1sp$z>rdbreak[5]&DB1sp$z<=rdbreak[6],a[6],
			ifelse(DB1sp$z>rdbreak[6]&DB1sp$z<=rdbreak[7],a[7],
			ifelse(DB1sp$z>rdbreak[7]&DB1sp$z<=rdbreak[8],a[8],
			ifelse(DB1sp$z>rdbreak[8]&DB1sp$z<=rdbreak[9],a[9],
			ifelse(DB1sp$z>rdbreak[9]&DB1sp$z<=rdbreak[10],a[10],
			ifelse(DB1sp$z>rdbreak[10]&DB1sp$z<=rdbreak[11],a[11],"black"
			))))))))))))

fig.yup<- 1/11
fig.lab<- seq(1,11)*fig.yup
#now make an interpolation for the figure

break.fig <- data.frame(x=rep(c(0,.5,1), each=11), y=rep(seq(0,1, length=11), times=3), z=rep(lab.break, times=3)	)

#create color range for soil moisture
vrange <- range(vDB1$Mineral.VWC)
#break up into 11
vl <-0.01
vu <- round_any(vrange[2],.1, ceiling)
lvbreaks <- seq(log(vl), log(vu), length=11)
vupper <- exp(lvbreaks)


vDB1$col <- ifelse(vDB1$Mineral.VWC<=vupper[1],a[11],
			ifelse(vDB1$Mineral.VWC>vupper[1]&vDB1$Mineral.VWC<=vupper[2],a[10],
			ifelse(vDB1$Mineral.VWC>vupper[2]&vDB1$Mineral.VWC<=vupper[3],a[9],
			ifelse(vDB1$Mineral.VWC>vupper[3]&vDB1$Mineral.VWC<=vupper[4],a[8],
			ifelse(vDB1$Mineral.VWC>vupper[4]&vDB1$Mineral.VWC<=vupper[5],a[7],
			ifelse(vDB1$Mineral.VWC>vupper[5]&vDB1$Mineral.VWC<=vupper[6],a[6],
			ifelse(vDB1$Mineral.VWC>vupper[6]&vDB1$Mineral.VWC<=vupper[7],a[5],
			ifelse(vDB1$Mineral.VWC>vupper[7]&vDB1$Mineral.VWC<=vupper[8],a[4],
			ifelse(vDB1$Mineral.VWC>vupper[8]&vDB1$Mineral.VWC<=vupper[9],a[3],
			ifelse(vDB1$Mineral.VWC>vupper[9]&vDB1$Mineral.VWC<=vupper[10],a[2],
			ifelse(vDB1$Mineral.VWC>vupper[10]&vDB1$Mineral.VWC<=vupper[11],a[1],"black")))))))))))

			
jpeg(paste0(plotDI, "\\DB1_out.jpg") , width=1500, height=700, units="px", quality=100)			
ab <- layout(matrix(seq(1,3), ncol=3), width=c(lcm(36), lcm(4), lcm(4)), height=c(lcm(15), lcm(15), lcm(15)))
layout.show(ab)
par(mai=c(0,0,0,0))		
plot(c(0,1), c(0,1), type="n", ylim=c(yl,yh), xlim=c(xl,xh), ylab=" ", xlab=" ", axes=FALSE, yaxs="i", xaxs="i")		
points(DB1sp$x,DB1sp$y,pch=19, col=DB1sp$col)
axis(2, seq(-.35,0, by=.05), las=2, cex.axis=2.5)
mtext("Depth (m)", side=2, line=7, cex=3)
axis(1, seq(0,2.5, by=.1), cex.axis=2.5)
mtext("Transect distance (m)", side=1, line=5, cex=3)

for(i in 1:dim(vDB1)[1]){
	polygon(c(vDB1$x[i],vDB1$x[i],vDB1$x[i]+.1,vDB1$x[i]+.1), c(-.015,0,0,-.015),
			col=vDB1$col[i],border=FALSE)

}


par(mai=c(0,.5,0,.5))
plot(c(0,1), c(0,1), type="n", ylim=c(0,1), xlim=c(0,1), ylab=" ", xlab=" ", axes=FALSE, yaxs="i", xaxs="i")

for(i in 1:length(a)){
	polygon(c(0,0,1,1), c(fig.yup*(i-1),fig.yup*i,fig.yup*i,fig.yup*(i-1)), col=a[i], border=FALSE)

}

axis(4, c(0,fig.lab), c(0,round(rdbreak,0)), las=2, cex.axis=2.5 )
mtext("Resistivity ", side=4, line=8, cex=3)

par(mai=c(0,1,0,0))
plot(c(0,1), c(0,1), type="n", ylim=c(0,1), xlim=c(0,1), ylab=" ", xlab=" ", axes=FALSE, yaxs="i", xaxs="i")

for(i in 1:length(a)){
	polygon(c(0,0,1,1), c(fig.yup*(i-1),fig.yup*i,fig.yup*i,fig.yup*(i-1)), col=a[i], border=FALSE)

}

axis(4, c(0,fig.lab), c(round(rev(vupper),3),0), las=2, cex.axis=2.5 )
mtext("Volumetric soil moisture ", side=4, line=9, cex=3)
dev.off()