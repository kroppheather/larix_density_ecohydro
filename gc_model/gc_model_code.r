###########################################################################
###########################################################################
############## Created by Heather Kropp in October 2017      ##############
############## The model code for the analysis of the        ##############
############## of canopy stomatal conductance calculated     ##############
############## from sapflow. The model is based on a         ##############
############## phenomenological model that describes         ##############
############## stomatal responses to light and VPD           ##############
############## used in Kropp et. al. 2017, Oren et. al. 1999,##############
############## Jarvis 1976, and White et. al. 1999.          ##############
############## The parameters of this model are considered   ##############
############## to vary with environmental drivers.           ##############
###########################################################################
###########################################################################

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