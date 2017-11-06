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
		a1[i]~dnorm(0,.001)
		b1[i]~dnorm(0,.001)
		d1[i]~dnorm(0,.001)
		d.trans1[i]<-exp(d1[i])
		a2[i]~dnorm(0,.001)
		b2[i]~dnorm(0,.001)
		d2[i]~dnorm(0,.001)
		d.trans2[i]<-exp(d2[i])
		a3[i]~dnorm(0,.001)
		b3[i]~dnorm(0,.001)
		d3[i]~dnorm(0,.001)
		d.trans3[i]<-exp(d3[i])
		a4[i]~dnorm(0,.001)
		b4[i]~dnorm(0,.001)
		d4[i]~dnorm(0,.001)
		d.trans4[i]<-exp(d4[i])	
		
		#likelihood variance
		tau.gs[i]<-pow(sig.gs[i],-2)
		sig.gs[i]~dunif(0,1000)	
	
	}

	
	
}	