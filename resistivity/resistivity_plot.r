###########################################################################
###########################################################################
############## Created by Heather Kropp in October 2017      ##############
############## Creates plots of resitivity data              ##############
###########################################################################
###########################################################################

#read in resistivity file
rDB1 <- read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\resistivity\\DB1_block.csv")

#get range of resistivity
range(rDB1$resistivity)
#set limits for plotting

rbreak <- c(seq(40,120, by=20), seq(150,400, by=50), seq(500,1100, by=200), seq(2000,6000, by=1000))
rcol <- topo.colors(length(rbreak))
resC <- character(0)
for(i in 1:dim(rDB1)[1]){
	if(rDB1$resistivity[i]<=rbreak[1]){
	resC[i] <- rcol[1]
	}else{
	for(j in 2:length(rbreak)){
		if(rDB1$resistivity[i]<=rbreak[j]&rDB1$resistivity[i]>rbreak[j-1]){
			resC[i] <- rcol[j]
		}	
	}
	}
}


#set up dimensions to plot:
xl <- 0
xh <-2.3
yl <- -.4
yh <-.01

plot(c(0,1), c(0,1), type="n", xlim=c(xl,xh), ylim=c(yl,yh), xlab= "Transect (m)",
		ylab="Depth (m)", xaxs="i", yaxs="i")

for(i in 1:dim(rDB1)[1]){
	polygon(c(rDB1$X.coord1[i],rDB1$X.coord2[i],rDB1$X.coord3[i],rDB1$X.coord4[i]),
			c(rDB1$Z.coord1[i],rDB1$Z.coord2[i],rDB1$Z.coord3[i],rDB1$Z.coord4[i]),col=resC[i],
			border=FALSE)
}		