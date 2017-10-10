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
############## to vary with environmental drivers:           ##############
############## average daily air temperature and antecedent  ##############
############## precipitation.                                ##############
###########################################################################
###########################################################################

model{
#Model likelihood
	for(i in 1:Nobs){
	#likelihood for each tree
	gs[i]~dnorm(mu.gs[i],tau.gs)
	#gs.rep[i]~dnorm(mu.gs[i],tau.gs)
	#model for mean gs
	mu.gs[i]<-oren.mod[i]
	#*light[i]

	#light scaling function
	light[i]<-1-exp(-l.slope[standDay[i]]*PAR[i])
	
	#oren model 1999 for mean gs
	oren.mod[i]<-gref[standDay[i]]*(1-S[standDay[i]]*log(D[i]))

	}
	for(i in 1:NstandDay){
		gref[i]<-a1[stand[i]]+a2[stand[i]]*airTcent[i]+a3[stand[i]]*PrecipTot[i]
		S[i]<-b1[stand[i]]+b2[stand[i]]*airTcent[stand[i]]+b3[stand[i]]*PrecipTot[i]
		#Log transform light function slope to avoid numerical traps
		#and allow for better mixing and faster convergence of the non-linear model
		slope.temp[i]<-d1[stand[i]]+d2[stand[i]]*ATemp[i]+d3[stand[i]]*PrecipTot[i]
		l.slope[i]<-exp(slope.temp[i])
	}
	#conduct covariate centering to help with mixing
	pastprcent[i]<-pastpr[i]-1
	airTcent[i]<-airT[i]-airTmean
	
	
	#calculate means for covariate centering
	airTmean<-mean(airT[])

	#Antecedent calculations based on Ogle et al 2015
	#calculate antecedent values for soil temp and soil water content

	for(m in 1:Nlag){
	#weights for precip
	deltapr[m]~dgamma(1,1)
	wpr[m]<-deltapr[m]/sumpr
	#calculate weighted precip for each day in the past
		for(i in 1:Ndays){
			pr.temp[i,m]<-wpr[m]*a.pr[i,m]
			}
	}


	#calculate sums of unweighted delta values

	sumpr<-sum(deltapr[])

	#final antecedent calculations for soil values
	for(i in 1:Ndays){
		pastpr[i]<-sum(pr.temp[i,])

	}
	
	#priors
	#define prior distributions for parameters
	#All parameters are given non-informative dist
	
	for(i in 1:Nparms){
		a[i]~dnorm(0,.0001)
		b[i]~dnorm(0,.0001)
		d[i]~dnorm(0,.0001)
		d.trans[i]<-exp(d[i])
	}
	tau.gs<-pow(sig.gs,-2)
	sig.gs~dunif(0,1000)
	
	
	
}	