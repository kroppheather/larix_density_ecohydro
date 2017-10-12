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

lab.break <-rdbreak/rdbreak[11]	

#now make an interpolation for the figure

break.fig <- data.frame(x=rep(c(0,.5,1), each=11), y=rep(seq(0,1, length=11), times=3), z=rep(lab.break, times=3)	)
legend.int <- interp(x=break.fig$x, y=break.fig$y, z=break.fig$y, xo=seq(0,1, length.out=100),
				yo=seq(0,1, length.out=100))

legend.int$z[,1]<-0				
legend.int$z[,100]<-1
legendDF <- data.frame(x=rep(legend.int$x, times=100), y=rep(legend.int$y, each=100), z=as.vector(legend.int$z))	


legendDF$col <-ifelse(is.na(legendDF$z), "white",
			ifelse(legendDF$z<=rdbreak[1]/rdbreak[11],a[1],
			ifelse(legendDF$z>rdbreak[1]/rdbreak[11]&legendDF$z<=rdbreak[2]/rdbreak[11],a[2],
			ifelse(legendDF$z>rdbreak[2]/rdbreak[11]&legendDF$z<=rdbreak[3]/rdbreak[11],a[3],
			ifelse(legendDF$z>rdbreak[3]/rdbreak[11]&legendDF$z<=rdbreak[4]/rdbreak[11],a[4],
			ifelse(legendDF$z>rdbreak[4]/rdbreak[11]&legendDF$z<=rdbreak[5]/rdbreak[11],a[5],
			ifelse(legendDF$z>rdbreak[5]/rdbreak[11]&legendDF$z<=rdbreak[6]/rdbreak[11],a[6],
			ifelse(legendDF$z>rdbreak[6]/rdbreak[11]&legendDF$z<=rdbreak[7]/rdbreak[11],a[7],
			ifelse(legendDF$z>rdbreak[7]/rdbreak[11]&legendDF$z<=rdbreak[8]/rdbreak[11],a[8],
			ifelse(legendDF$z>rdbreak[8]/rdbreak[11]&legendDF$z<=rdbreak[9]/rdbreak[11],a[9],
			ifelse(legendDF$z>rdbreak[9]/rdbreak[11]&legendDF$z<=rdbreak[10]/rdbreak[11],a[10],
			ifelse(legendDF$z>rdbreak[10]/rdbreak[11]&legendDF$z<=rdbreak[11]/rdbreak[11],a[11],"black"
			))))))))))))

ab <- layout(matrix(seq(1,2), ncol=2), width=c(lcm(35), lcm(5)), height=c(lcm(15), lcm(15)))
layout.show(ab)
par(mai=c(0,0,0,0))		
plot(c(0,1), c(0,1), type="n", ylim=c(yl,yh), xlim=c(xl,xh), ylab=" ", xlab=" ", axes=FALSE, yaxs="i", xaxs="i")		
points(DB1sp$x,DB1sp$y,pch=19, col=DB1sp$col)
axis(2, seq(-.35,0, by=.05), las=2, cex.axis=1.5)
mtext("Depth (m)", side=2, line=5, cex=2)
axis(3, seq(0,2.5, by=.5), cex.axis=1.5)
mtext("Transect distance (m)", side=3, line=3, cex=2)

par(mai=c(0,0,0,0))
plot(c(0,1), c(0,1), type="n", ylim=c(0,1), xlim=c(0,1), ylab=" ", xlab=" ", axes=FALSE, yaxs="i", xaxs="i")

points(legendDF$x,legendDF$y, pch=15,col=legendDF$col )