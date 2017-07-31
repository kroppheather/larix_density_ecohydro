#set working directory
setwd("c:\\Users\\hkropp\\Google Drive\\root_analysis")
#read in data
datF<-read.csv("model_comp.csv")

#fit the baseline model
fitb<-lm(datF[,2]~datF[,1])
summary(fitb)
plot(datF[,1],datF[,2], xlim=c(0,12.5), ylim=c(0,12.5), pch=19, xlab="observed Rbio", ylab="predicted Rbio") 
abline(0,1, lwd=2)
abline(fitb,lty=2, lwd=2)
#r2=0.50
#fit the first verison of stochastic Rtot
fitrs<-lm(datF[,3]~datF[,1])
summary(fitrs)
plot(datF[,1],datF[,3], xlim=c(0,12.5), ylim=c(0,12.5), pch=19, xlab="observed Rbio", ylab="predicted Rbio") 
abline(0,1, lwd=2)
abline(fitrs,lty=2, lwd=2)
#r2=0.48
#second version of stochastic Rtot
fitrs2<-lm(datF[,4]~datF[,1])
summary(fitrs2)
plot(datF[,1],datF[,4], xlim=c(0,12.5), ylim=c(0,12.5), pch=19, xlab="observed Rbio", ylab="predicted Rbio") 
abline(0,1, lwd=2)
abline(fitrs2,lty=2, lwd=2)