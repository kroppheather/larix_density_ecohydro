#examine potential function and realistic starting values
#look at just using the beta shape but scaling by total root biomass
log.beta<-function(x,alpha,beta){
			(log(x)*(alpha-1))+(log(1-x)*(beta-1))
		}
		
Root.shape1<-function(sh,Rtot){exp(sh)*Rtot}		
#make a plot on the back transformed scale
Xs<-seq(0,.99, by=0.01)

plot(Xs,Root.shape1(log.beta(Xs,16,17),10.46), type="l", lwd=2)

#try with the beta constant

log.betaC<-function(x,alpha,beta){
			(log(x)*(alpha-1))+(log(1-x)*(beta-1))-lbeta(alpha,beta)
		}
		
plot(Xs,exp(log.betaC(Xs,2,5)), type="l", lwd=2)
plot(Xs,exp(log.betaC(Xs,5,30)), type="l", lwd=2)