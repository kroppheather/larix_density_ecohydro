#check for correlation between thaw depth and other coefficients vs broken up thaw depth components (root frozen and temp) 
#with the other model coefficients.
library(R2OpenBUGS)


m1dir <- "c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\gc_model\\run34\\CODA_out"
m2dir <- "c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\gc_model\\run40\\CODA_out3"
m3dir <- "c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\gc_model\\run39\\CODA_out"
m4dir <- "c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\gc_model\\run42\\CODA_out3"

codaobj1 <- read.bugs(c(paste0(m1dir,"\\chain1\\CODAchain1.txt"),
						paste0(m1dir,"\\chain2\\CODAchain1.txt")
						,paste0(m1dir,"\\chain3\\CODAchain1.txt")
						))
codaobj2 <- read.bugs(c(paste0(m2dir,"\\chain1\\CODAchain1.txt"),
						paste0(m2dir,"\\chain2\\CODAchain1.txt")
						,paste0(m2dir,"\\chain3\\CODAchain1.txt")
						))
						
codaobj3 <- read.bugs(c(paste0(m3dir,"\\chain1\\CODAchain1.txt"),
						paste0(m3dir,"\\chain2\\CODAchain1.txt")
						,paste0(m3dir,"\\chain3\\CODAchain1.txt")
						))						
codaobj4 <- read.bugs(c(paste0(m4dir,"\\chain1\\CODAchain1.txt"),
						paste0(m4dir,"\\chain2\\CODAchain1.txt")
						,paste0(m4dir,"\\chain3\\CODAchain1.txt")
						))	
#get the parameters for m1 low density
p1m1<-c(90,92,94,96,98,100,102,104,106,108,110,112	)			 

m1parms1 <- rbind(codaobj1[[1]][,p1m1],codaobj1[[2]][,p1m1], codaobj1[[3]][,p1m1])

#gref mod1
plot(~m1parms1[,1]+m1parms1[,2]+m1parms1[,3]+m1parms1[,4])

#S mod1
plot(~m1parms1[,5]+m1parms1[,6]+m1parms1[,7]+m1parms1[,8])

#D mod1
plot(~m1parms1[,9]+m1parms1[,10]+m1parms1[,11]+m1parms1[,12])
cor(data.matrix(m1parms1[,9:12]))
cor(data.matrix(m1parms1[,1:4]))
#precip and thaw depth have high correlation between parameters in each iteration
#precip and air temperature have no correlation


p1m2<-c(90,92,94,96,98,100,102,104,106,108,110,112,114,116,118	)
p2m2<-p1m2+1

m2parms1 <- rbind(codaobj2[[1]][,p1m2],codaobj2[[2]][,p1m2], codaobj2[[3]][,p1m2])
m2parms2 <- rbind(codaobj2[[1]][,p2m2],codaobj2[[2]][,p2m2], codaobj2[[3]][,p2m2])
#gref mod2
plot(~m2parms1[,1]+m2parms1[,2]+m2parms1[,3]+m2parms1[,4]+m2parms1[,5])
plot(~m2parms2[,1]+m2parms2[,2]+m2parms2[,3]+m2parms2[,4]+m2parms2[,5])
cor(data.matrix(m2parms1[,1:5]))
#precip is not correlated with pfroze and has a slight correlation with T soil but Tsoil and pfroze are highly correlated


#get the parameters for m3 low density
p1m3<-c(90,92,94,96,98,100,102,104,106,108,110,112	)	

m3parms1 <- rbind(codaobj3[[1]][,p1m3],codaobj3[[2]][,p1m3], codaobj3[[3]][,p1m3])
plot(~m3parms1[,1]+m3parms1[,2]+m3parms1[,3]+m3parms1[,4])
#Tsoil is pretty correlated with precip in this model
cor(data.matrix(m3parms1[,1:4]))
#correlations are low and not a problem compared to above
p1m4<-c(90,92,94,96,98,100,102,104,106)	
m4parms1 <- rbind(codaobj4[[1]][,p1m4],codaobj4[[2]][,p1m4], codaobj4[[3]][,p1m4])
plot(~m4parms1[,1]+m4parms1[,2]+m4parms1[,3])
cor(m4parms1[,2],m4parms1[,3])
cor(data.matrix(m4parms1[,1:3]))