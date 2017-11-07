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
	gs[i]~dnorm(mu.gs[i],tau.gs[treeID[i]])
	rep.gs[i]~dnorm(mu.gs[i],tau.gs[treeID[i]])
	
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
	#met variables vary be stand, day
	#response varies by tree
		gref[i]<-a[1,tree[i]]+a[2,tree[i]]*airTcent[i]+a[3,tree[i]]*(pastpr[i]-5)+a[4,tree[i]]*(thawD[i]-thawstart[stand[i]])
		S[i]<-b[1,tree[i]]+b[2,tree[i]]*airTcent[i]+b[3,tree[i]]*(pastpr[i]-5)+b[4,tree[i]]*(thawD[i]-thawstart[stand[i]])
		slope.temp[i] <-d[1,tree[i]]+d[2,tree[i]]*airTcent[i]+d[3,tree[i]]*(pastpr[i]-5)+d[4,tree[i]]*(thawD[i]-thawstart[stand[i]])
		#Log transform light function slope to avoid numerical traps
		#and allow for better mixing and faster convergence of the non-linear model
		l.slope[i]<-exp(slope.temp[i])
	#conduct covariate centering to help with mixing

	airTcent[i]<-airT[i]-airTmean	

	#calculate sensitivity

	}
	
#################################
######hierarchical model    #####
#################################
	for(i in 1:Ntrees){
		for(j in 1:4){
		#hierarchical model for trees by stand with parameter expansion
		a[j,i] <- mu.a[j,standT[i]] + constA[j,standT[i]]*epsA[j,i]
		b[j,i]<-mu.b[j,standT[i]]  + constB[j,standT[i]]*epsB[j,i]
		d[j,i]<-mu.d[j,standT[i]]  + constD[j,standT[i]]*epsD[j,i]
		epsA[j,i] ~dnorm(0,tau.epsA[j,standT[i]])
		epsB[j,i] ~dnorm(0,tau.epsB[j,standT[i]])
		epsD[j,i] ~dnorm(0,tau.epsD[j,standT[i]])
		
		}
		#likelihood variance
		tau.gs[i]<-pow(sig.gs[i],-2)
		sig.gs[i]~dunif(0,1000)			
		
	}	
	
	#parameter expansion to promote better mixing 
	for(j in 1:4){
		for(i in 1:Nstand){	
		constA[j,i]~dnorm(0,1)
		constB[j,i]~dnorm(0,1)
		constD[j,i]~dnorm(0,1)

		#means
		mu.a[j,i] ~ dnorm(0,.001)
		mu.b[j,i] ~ dnorm(0,.001)
		mu.d[j,i] ~ dnorm(0,.001)
		#variance expansion
		tau.epsA[j,i] ~dgamma(.001,.001)
		tau.betaA[j,i] <- pow(sig.betaA[j,i], -2)
		sig.betaA[j,i] <- abs(constA[j,i]*sig.epsA[j,i])
		sig.epsA[j,i] <- 1/ sqrt(tau.epsA[j,i])
		
		tau.epsB[j,i] ~dgamma(.001,.001)
		tau.betaB[j,i] <- pow(sig.betaB[j,i], -2)
		sig.betaB[j,i] <- abs(constB[j,i]*sig.epsB[j,i])
		sig.epsB[j,i] <- 1/ sqrt(tau.epsB[j,i])
		
		tau.epsD[j,i] ~dgamma(.001,.001)
		tau.betaD[j,i] <- pow(sig.betaD[j,i], -2)
		sig.betaD[j,i] <- abs(constD[j,i]*sig.epsD[j,i])
		sig.epsD[j,i] <- 1/ sqrt(tau.epsD[j,i])
		}

		
	}

	
	
}	