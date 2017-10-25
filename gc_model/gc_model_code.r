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
	gs[i]~dnorm(mu.gs[i],tau.gs)
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
		gref[i]<-a1[stand[i]]+a2[stand[i]]*airTcent[i]+a3[stand[i]]*(pastpr[i]-5)+a4[stand[i]]*(thawD[i]-thawstart[stand[i]])+epsA[tree[i]]
		S[i]<-b1[stand[i]]+b2[stand[i]]*airTcent[i]+b3[stand[i]]*(pastpr[i]-5)+b4[stand[i]]*(thawD[i]-thawstart[stand[i]])+epsB[tree[i]]
		slope.temp[i] <-d1[stand[i]]+d2[stand[i]]*airTcent[i]+d3[stand[i]]*(pastpr[i]-5)+d4[stand[i]]*(thawD[i]-thawstart[stand[i]])+epsD[tree[i]]
		#Log transform light function slope to avoid numerical traps
		#and allow for better mixing and faster convergence of the non-linear model
		l.slope[i]<-exp(slope.temp[i])
	#conduct covariate centering to help with mixing

	airTcent[i]<-airT[i]-airTmean	

	#calculate sensitivity

	}
#################################
######spatial random effects#####
#################################
	#define random effects
	epsA[1:Ntree] ~ dmnorm(mu.epsA[1:Ntree],OmegaA[1:Ntree,1:Ntree])
	epsB[1:Ntree] ~ dmnorm(mu.epsB[1:Ntree],OmegaB[1:Ntree,1:Ntree])
	epsD[1:Ntree] ~ dmnorm(mu.epsD[1:Ntree],OmegaD[1:Ntree,1:Ntree])
	
	for(i in 1:Ntree){
		#specify means
		mu.epsA[i] <- 0
		mu.epsB[i] <- 0
		mu.epsD[i] <- 0
		#specify identifiable parameters
		epsA.star[i] <- epsA[i]-epsA.mean
		epsB.star[i] <- epsB[i]-epsB.mean
		epsD.star[i] <- epsD[i]-epsD.mean
	}
	#calculate mean for idetifiability
	epsA.mean <- mean(epsA[])
	epsB.mean <- mean(epsB[])
	epsD.mean <- mean(epsD[])
	#calculate identifiable intercept
	for(i in 1:Nstand){
		a1.star[i] <- a1[i]+epsA.mean
		b1.star[i] <- b1[i]+epsB.mean
		d1.star[i] <- d1[i]+epsD.mean
		
	}
	
	#spatial covariance model for tree random effect
	OmegaA[1:Ntree,1:Ntree] <- inverse(SigmaA[1:Ntree,1:Ntree])
	OmegaB[1:Ntree,1:Ntree] <- inverse(SigmaB[1:Ntree,1:Ntree])
	OmegaD[1:Ntree,1:Ntree] <- inverse(SigmaD[1:Ntree,1:Ntree])
	#standard deviation spatial covariance
	for(m in 1:Ntree){
		for(j in 1:Ntree){
			SigmaA[m,j] <- (1/tauA)*exp(phiA*DistA[m,j])
			SigmaB[m,j] <- (1/tauB)*exp(phiB*DistB[m,j])			
			SigmaD[m,j] <- (1/tauD)*exp(phiD*DistD[m,j])
		}
	}
	#priors for spatial covariance
	#folded t for standard deviation
	tauA <- pow(sigA,-2)
	sigA <- abs(t.A)
	t.A ~ dt(0,p.A, 2)
	p.A <- 1/(v.A*v.A)
	v.A ~ dunif(0,100)
	
	tauB <- pow(sigB,-2)
	sigB <- abs(t.B)
	t.B <- dt(0,p.B, 2)
	p.B <- 1/(v.B*v.B)
	v.B ~ dunif(0,100)
	
	tauD <- pow(sigD,-2)
	sigD <- abs(t.D)
	t.D <- dt(0,p.D, 2)
	p.D <- 1/(v.D*v.D)
	v.D ~ dunif(0,100)	
	
	#prior for autocorrelation
	phiA <-  log(rhoA)
	rhoA ~ dbeta(alphaA,betaA)
	alphaA ~ dunif(0,100)
	betaA v dunif(0,100)
	
	phiB <-  log(rhoB)
	rhoB ~ dbeta(alphaB,betaB)
	alphaB ~ dunif(0,100)
	betaB ~ dunif(0,100)
	
	phiA <-  log(rhoA)
	rhoA ~ dbeta(alphaA,betaA)
	alphaB ~ dunif(0,100)
	betaB ~ dunif(0,100)
	
	
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
	}
	tau.gs<-pow(sig.gs,-2)
	sig.gs~dunif(0,1000)
	
	
	
}	