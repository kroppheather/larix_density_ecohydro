##############################
###model code for gc model ###
###across stand density grad##
##############################

model{
#Model likelihood
	for(i in 1:Nobs){
	gs[i]~dnorm(mu.gs[i],tau.gs)
	#gs.rep[i]~dnorm(mu.gs[i],tau.gs)
	#model for mean gs
	mu.gs[i]<-oren.mod[i]
	#*light[i]

	#light scaling function
	#light[i]<-1-exp(-l.slope[daySiteID[i]]*PAR[i])
	
	#oren model 1999 for mean gs
	oren.mod[i]<-gref[daySiteID[i]]*(1-S[daySiteID[i]]*log(D[i]))

	}
	for(i in 1:NdaySiteID){
	gref[i]<-a1[standID[i]]+a2[standID[i]]*ATemp[i]+a3[standID[i]]*PrecipTot[i]
	S[i]<-b1[standID[i]]+b2[standID[i]]*ATemp[i]+b3[standID[i]]*PrecipTot[i]
	}
	#priors
	for(i in 1:Nstand){
	a1[i]~dnorm(0,.001)
	a2[i]~dnorm(0,.001)
	a3[i]~dnorm(0,.001)
	b1[i]~dnorm(0,.001)
	b2[i]~dnorm(0,.001)
	b3[i]~dnorm(0,.001)
	}
	tau.gs<-pow(sig.gs,-2)
	sig.gs~dunif(0,1000)
	
	
	
}	