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
		a1[i]<- i.a1[standT[i]]+s.a1[standT[i]]*neighb[i]
		b1[i]<-i.b1[standT[i]]+s.b1[standT[i]]*neighb[i]
		d1[i]<-i.d1[standT[i]]+s.d1[standT[i]]*neighb[i]
		d.trans1[i]<-exp(d1[i])
		a2[i]<- i.a2[standT[i]]+s.a2[standT[i]]*neighb[i]
		b2[i]<- i.b2[standT[i]]+s.b2[standT[i]]*neighb[i]
		d2[i]<- i.d2[standT[i]]+s.d2[standT[i]]*neighb[i]
		d.trans2[i]<-exp(d2[i])
		a3[i]<-i.a3[standT[i]]+s.a3[standT[i]]*neighb[i]
		b3[i]<- i.b3[standT[i]]+s.b3[standT[i]]*neighb[i]
		d3[i]<- i.d3[standT[i]]+s.d3[standT[i]]*neighb[i]
		d.trans3[i]<-exp(d3[i])
		a4[i]<-i.a4[standT[i]]+s.a4[standT[i]]*neighb[i]
		b4[i]<-i.b4[standT[i]]+s.b4[standT[i]]*neighb[i]
		d4[i]<-i.d4[standT[i]]+s.d4[standT[i]]*neighb[i]
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
		
		i.a1[i]~dnorm(0,.001)
		i.b1[i]~dnorm(0,.001)
		i.d1[i]~dnorm(0,.0001)
		i.a2[i]~dnorm(0,.001)
		i.b2[i]~dnorm(0,.001)
		i.d2[i]~dnorm(0,.0001)
		i.a3[i]~dnorm(0,.001)
		i.b3[i]~dnorm(0,.001)
		i.d3[i]~dnorm(0,.0001)

		i.a4[i]~dnorm(0,.001)
		i.b4[i]~dnorm(0,.001)
		i.d4[i]~dnorm(0,.0001)
	
		s.a1[i] ~dnorm(0,.001)
		s.a2[i]~dnorm(0,.001)
		s.a3[i]~dnorm(0,.001)
		s.a4[i] ~dnorm(0,.001)
	
		s.b1[i]~dnorm(0,.001)
		s.b2[i] ~dnorm(0,.001)
		s.b3[i] ~dnorm(0,.001)
		s.b4[i]~dnorm(0,.001)

		
		s.d1[i] ~dnorm(0,.001)
		s.d2[i] ~dnorm(0,.001)
		s.d3[i] ~dnorm(0,.001)
		s.d4[i] ~dnorm(0,.001)
	
		
	}

	
	
	
}	