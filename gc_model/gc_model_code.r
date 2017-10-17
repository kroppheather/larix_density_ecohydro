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
	mu.gs[i]<-oren.mod[i]*light[i]

	#light scaling function
	light[i]<-1-exp(-l.slope[standDay[i]]*PAR[i])
	
	#oren model 1999 for mean gs
	oren.mod[i]<-gref[standDay[i]]*(1-(S[standDay[i]]*log(D[i])))

	}
	for(i in 1:NstandDay){
		gref[i]<-a1[stand[i]]+a2[stand[i]]*airTcent[i]
		#+a3[stand[i]]*pastpr[Days[i],stand[i]]
		S[i]<-b1[stand[i]]+b2[stand[i]]*airTcent[i]
		#+b3[stand[i]]*pastpr[Days[i],stand[i]]
		#Log transform light function slope to avoid numerical traps
		#and allow for better mixing and faster convergence of the non-linear model
		slope.temp[i]<-d1[stand[i]]+d2[stand[i]]*airTcent[i]
		#+d3[stand[i]]*pastpr[Days[i],stand[i]]
		l.slope[i]<-exp(slope.temp[i])
	#conduct covariate centering to help with mixing

	airTcent[i]<-airT[i]-airTmean	
	#calculate sensitivity

	}


	#Antecedent calculations based on Ogle et al 2015
	#calculate antecedent values for soil temp and soil water content
	#for(j in 1:Nstand){
	#	for(m in 1:Nlag){
	#		#weights for precip
	#		deltapr[m,j]~dgamma(1,1)
	#		wpr[m,j]<-deltapr[m,j]/sumpr[j]
			#calculate weighted precip for each day in the past
	#		for(i in 1:Ndays){
	#			pr.temp[i,m,j]<-wpr[m,j]*a.pr[i,m]
	#		}
	#	}

	#}
	#calculate sums of unweighted delta values

	#sumpr[1]<-sum(deltapr[,1])
	#sumpr[2]<-sum(deltapr[,2])

	#final antecedent calculations for soil values
	#for(i in 1:Ndays){
	#	pastpr[i,1]<-sum(pr.temp[i,,1])
	#	pastpr[i,2]<-sum(pr.temp[i,,2])
	#}
	
	#priors
	#define prior distributions for parameters
	#All parameters are given non-informative dist
	

	for(i in 1:Nstand){
		a1[i]~dnorm(0,.001)
		b1[i]~dnorm(0,.001)
		d1[i]~dnorm(0,.0001)
		d.trans1[i]<-exp(d1[i])
		a2[i]~dnorm(0,.001)
		b2[i]~dnorm(0,.001)
		d2[i]~dnorm(0,.0001)
		d.trans2[i]<-exp(d2[i])
	#	a3[i]~dnorm(0,.001)
	#	b3[i]~dnorm(0,.001)
	#	d3[i]~dnorm(0,.0001)
	#	d.trans3[i]<-exp(d3[i])
	}
	tau.gs<-pow(sig.gs,-2)
	sig.gs~dunif(0,1000)
	
	
	
}	