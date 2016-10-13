#examine potential function and realistic starting values
#look at just using the beta shape but scaling by total root biomass
log.beta<-function(x,alpha,beta){
			(log(x)*(alpha-1))+(log(1-x)*(beta-1))
		}
		
Root.shape1<-function(sh,Rtot){exp(sh)*Rtot}		
#make a plot on the back transformed scale
Xs<-seq(0,.99, by=0.01)

plot(Xs,Root.shape1(log.beta(Xs,16,17),10.46), type="l", lwd=2)
plot(Xs,exp(log.beta(Xs,1.67,1.43)), type="l", lwd=2, ylim=c(0,1), col="forestgreen")
points(Xs,exp(log.beta(Xs,1.43,1.36)), type="l", lwd=2,  col="coral4")
points(Xs,exp(log.beta(Xs,1.44,2.58)), type="l", lwd=2,  col="darkorchid4")
points(Xs,exp(log.beta(Xs,1.44,2.45)), type="l", lwd=2,  col="royalblue4")
points(Xs,exp(log.beta(Xs,1.26,4.67)), type="l", lwd=2,  col="forestgreen", lty=3)
points(Xs,exp(log.beta(Xs,1.39,4.88)), type="l", lwd=2,  col="darkorchid4", lty=3)
points(Xs,exp(log.beta(Xs,1.411,5.085)), type="l", lwd=2,  col="royalblue4", lty=3)

#just plot shape function

#try with the beta constant

log.betaC<-function(x,alpha,beta){
			(log(x)*(alpha-1))+(log(1-x)*(beta-1))-lbeta(alpha,beta)
		}
		
plot(Xs,exp(log.betaC(Xs,2,5)), type="l", lwd=2)
plot(Xs,exp(log.betaC(Xs,5,30)), type="l", lwd=2)