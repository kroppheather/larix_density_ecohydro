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
		gref[i]<-a1[tree[i]]+a2[tree[i]]*airTcent[i]+a3[tree[i]]*(pastpr[i]-5)+a4[tree[i]]*(thawD[i]-thawstart[stand[i]])
		S[i]<-b1[tree[i]]+b2[tree[i]]*airTcent[i]+b3[tree[i]]*(pastpr[i]-5)+b4[tree[i]]*(thawD[i]-thawstart[stand[i]])
		slope.temp[i] <-d1[tree[i]]+d2[tree[i]]*airTcent[i]+d3[tree[i]]*(pastpr[i]-5)+d4[tree[i]]*(thawD[i]-thawstart[stand[i]])
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
		a1[i]~dnorm(mu.a1[standT[i]],tau.a1[standT[i]])
		b1[i]~dnorm(mu.b1[standT[i]],tau.b1[standT[i]])
		d1[i]~dnorm(mu.d1[standT[i]],tau.d1[standT[i]])
		d.trans1[i]<-exp(d1[i])
		a2[i]~dnorm(mu.a2[standT[i]],tau.a2[standT[i]])
		b2[i]~dnorm(mu.b2[standT[i]],tau.b2[standT[i]])
		d2[i]~dnorm(mu.d2[standT[i]],tau.d2[standT[i]])
		d.trans2[i]<-exp(d2[i])
		a3[i]~dnorm(mu.a3[standT[i]],tau.a3[standT[i]])
		b3[i]~dnorm(mu.b3[standT[i]],tau.b3[standT[i]])
		d3[i]~dnorm(mu.d3[standT[i]],tau.d3[standT[i]])
		d.trans3[i]<-exp(d3[i])
		a4[i]~dnorm(mu.a4[standT[i]],tau.a4[standT[i]])
		b4[i]~dnorm(mu.b4[standT[i]],tau.b4[standT[i]])
		d4[i]~dnorm(mu.d4[standT[i]],tau.d4[standT[i]])
		d.trans4[i]<-exp(d4[i])	
		
		#likelihood variance
		tau.gs[i]<-pow(sig.gs[i],-2)
		sig.gs[i]~dunif(0,1000)	
	
	}

#################################
#########hyper priors    ########
#################################	
	#define prior distributions for parameters
	#All parameters are given non-informative dist
	

	for(i in 1:Nstand){
		#mean hyper priors
		mu.a1[i]~dnorm(0,.001)
		mu.b1[i]~dnorm(0,.001)
		mu.d1[i]~dnorm(0,.0001)
		mu.d.trans1[i]<-exp(mu.d1[i])
		mu.a2[i]~dnorm(0,.001)
		mu.b2[i]~dnorm(0,.001)
		mu.d2[i]~dnorm(0,.0001)
		mu.d.trans2[i]<-exp(mu.d2[i])
		mu.a3[i]~dnorm(0,.001)
		mu.b3[i]~dnorm(0,.001)
		mu.d3[i]~dnorm(0,.0001)
		mu.d.trans3[i]<-exp(mu.d3[i])
		mu.a4[i]~dnorm(0,.001)
		mu.b4[i]~dnorm(0,.001)
		mu.d4[i]~dnorm(0,.0001)
		mu.d.trans4[i]<-exp(mu.d4[i])		
		#variance hyper priors
		tau.a1[i] <- pow(sig.a1[i],-2)
		tau.a2[i] <- pow(sig.a2[i],-2)
		tau.a3[i] <- pow(sig.a3[i],-2)
		tau.a4[i] <- pow(sig.a4[i],-2)
	
		tau.b1[i] <- pow(sig.b1[i],-2)
		tau.b2[i] <- pow(sig.b2[i],-2)
		tau.b3[i] <- pow(sig.b3[i],-2)
		tau.b4[i] <- pow(sig.b4[i],-2)

		
		tau.d1[i] <- pow(sig.d1[i],-2)
		tau.d2[i] <- pow(sig.d2[i],-2)
		tau.d3[i] <- pow(sig.d3[i],-2)
		tau.d4[i] <- pow(sig.d4[i],-2)
		
		sig.a1[i]~dunif(0,100)
		sig.b1[i]~dunif(0,100)
		sig.d1[i]~dunif(0,100)
		
		sig.a2[i]~dunif(0,100)
		sig.b2[i]~dunif(0,100)
		sig.d2[i]~dunif(0,100)
		
		sig.a3[i]~dunif(0,100)
		sig.b3[i]~dunif(0,100)
		sig.d3[i]~dunif(0,100)	
		
		sig.a4[i]~dunif(0,100)
		sig.b4[i]~dunif(0,100)
		sig.d4[i]~dunif(0,100)		

		
		
	}

	
	
	
}	