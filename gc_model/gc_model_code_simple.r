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
#################################
#########Model likelihood########
#################################
	for(i in 1:Nobs){
	#likelihood for each tree
	gs[i]~dnorm(mu.gs[i],tau.gs[stand.obs[i]])
	rep.gs[i]~dnorm(mu.gs[i],tau.gs[stand.obs[i]])
	
	#gs.rep[i]~dnorm(mu.gs[i],tau.gs)
	#model for mean gs
	mu.gs[i]<-oren.mod[i]*light[i]
	
	#light scaling function
	light[i]<-1-exp(-l.slope[standDayTree[i]]*PAR[i])
	
	#oren model 1999 for mean gs
	oren.mod[i]<-gref[standDayTree[i]]*(1-(S[standDayTree[i]]*log(D[i])))

	}
#################################
#########parameter model ########
#################################	
	for(i in 1:NstandDayTree){
		gref[i]<-a1[stand[i]]+a2[stand[i]]*airTcent[i]+a3[stand[i]]*(pastpr[i]-5)+a4[stand[i]]*(thawD[i]-thawstart[stand[i]])+a5[stand[i]]*neighb[i]+epsA[stand[i],tree[i]]
		S[i]<-b1[stand[i]]+b2[stand[i]]*airTcent[i]+b3[stand[i]]*(pastpr[i]-5)+b4[stand[i]]*(thawD[i]-thawstart[stand[i]])+b5[stand[i]]*neighb[i]+epsB[stand[i],tree[i]]
		slope.temp[i] <-d1[stand[i]]+d2[stand[i]]*airTcent[i]+d3[stand[i]]*(pastpr[i]-5)+d4[stand[i]]*(thawD[i]-thawstart[stand[i]])+d5[stand[i]]*neighb[i]+epsD[stand[i],tree[i]]
		#Log transform light function slope to avoid numerical traps
		#and allow for better mixing and faster convergence of the non-linear model
		l.slope[i]<-exp(slope.temp[i])
	#conduct covariate centering to help with mixing

	airTcent[i]<-airT[i]-airTmean	

	#calculate sensitivity

	}
	
#################################
######random effects        #####
#################################
	#define random effects
	for(i in 1:Nstand){
		for(j in 1:Ntree){
		epsA[i,j] ~ dnorm(0, tau.epsA[i])
		epsB[i,j] ~ dnorm(0, tau.epsB[i])
		epsD[i,j] ~ dnorm(0, tau.epsB[i])
	
		#specify identifiable parameters
		epsA.star[i,j] <- epsA[i,j]-epsA.mean[i]
		epsB.star[i,j] <- epsB[i,j]-epsB.mean[i]
		epsD.star[i,j] <- epsD[i,j]-epsD.mean[i]
	}
	
	#calculate identifiable intercept
	
		a1.star[i] <- a1[i]+epsA.mean[i]
		b1.star[i] <- b1[i]+epsB.mean[i]
		d1.star[i] <- d1[i]+epsD.mean[i]	
	
}	
	#calculate mean for idetifiability
	epsA.mean[1] <- mean(epsA[1,])
	epsB.mean[1] <- mean(epsB[1,])
	epsD.mean[1] <- mean(epsD[1,])
	epsA.mean[2] <- mean(epsA[2,])
	epsB.mean[2] <- mean(epsB[2,])
	epsD.mean[2] <- mean(epsD[2,])	
	
	#priors for variance
	for(i in 1:Nstand){
	tau.epsA[i]<- pow(sig.epsA[i],-2)
	sig.epsA[i]~dunif(0,100)
	tau.epsB[i]<- pow(sig.epsB[i],-2)
	sig.epsB[i]~dunif(0,100)	
	tau.epsD[i]<- pow(sig.epsD[i],-2)
	sig.epsD[i]~dunif(0,100)
	}	
	
	
	
	

#################################
#########priors          ########
#################################	
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
		a3[i]~dnorm(0,.001)
		b3[i]~dnorm(0,.001)
		d3[i]~dnorm(0,.0001)
		d.trans3[i]<-exp(d3[i])
		a4[i]~dnorm(0,.001)
		b4[i]~dnorm(0,.001)
		d4[i]~dnorm(0,.0001)
		d.trans4[i]<-exp(d4[i])		
		a5[i]~dnorm(0,.001)
		b5[i]~dnorm(0,.001)
		d5[i]~dnorm(0,.0001)
		d.trans5[i]<-exp(d5[i])		
		#
		
		tau.gs[i]<-pow(sig.gs[i],-2)
		sig.gs[i]~dunif(0,1000)	
		
		
	}

	
	
	
}	