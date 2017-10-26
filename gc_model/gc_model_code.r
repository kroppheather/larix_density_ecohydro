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
		gref[i]<-a1[stand[i]]+a2[stand[i]]*airTcent[i]+a3[stand[i]]*(pastpr[i]-5)+a4[stand[i]]*(thawD[i]-thawstart[stand[i]])+epsA[stand[i],Tree[i]]
		S[i]<-b1[stand[i]]+b2[stand[i]]*airTcent[i]+b3[stand[i]]*(pastpr[i]-5)+b4[stand[i]]*(thawD[i]-thawstart[stand[i]])+epsB[stand[i],Tree[i]]
		slope.temp[i] <-d1[stand[i]]+d2[stand[i]]*airTcent[i]+d3[stand[i]]*(pastpr[i]-5)+d4[stand[i]]*(thawD[i]-thawstart[stand[i]])+epsD[stand[i],Tree[i]]
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
	for(i in 1:Nstand){
	epsA[i,1:Ntree] ~ dmnorm(mu.epsA[i,1:Ntree],OmegaA[i,1:Ntree,1:Ntree])
	epsB[i,1:Ntree] ~ dmnorm(mu.epsB[i,1:Ntree],OmegaB[i,1:Ntree,1:Ntree])
	epsD[i,1:Ntree] ~ dmnorm(mu.epsD[i,1:Ntree],OmegaD[i,1:Ntree,1:Ntree])
	
	for(j in 1:Ntree){
		#specify means
		mu.epsA[i,j] <- 0
		mu.epsB[i,j] <- 0
		mu.epsD[i,j] <- 0
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
	
	
	#spatial covariance model for tree random effect
	OmegaA[1,1:Ntree,1:Ntree] <- inverse(SigmaA[1,1:Ntree,1:Ntree])
	OmegaB[1,1:Ntree,1:Ntree] <- inverse(SigmaB[1,1:Ntree,1:Ntree])
	OmegaD[1,1:Ntree,1:Ntree] <- inverse(SigmaD[1,1:Ntree,1:Ntree])
	OmegaA[2,1:Ntree,1:Ntree] <- inverse(SigmaA[2,1:Ntree,1:Ntree])
	OmegaB[2,1:Ntree,1:Ntree] <- inverse(SigmaB[2,1:Ntree,1:Ntree])
	OmegaD[2,1:Ntree,1:Ntree] <- inverse(SigmaD[2,1:Ntree,1:Ntree])
	#standard deviation spatial covariance
	for(i in 1:Nstand){
		for(m in 1:Ntree){
			for(j in 1:Ntree){
				SigmaA[i,m,j] <- (1/tauA[i])*exp(phiA[i]*DistA[i,m,j])
				SigmaB[i,m,j] <- (1/tauB[i])*exp(phiB[i]*DistB[i,m,j])			
				SigmaD[i,m,j] <- (1/tauD[i])*exp(phiD[i]*DistD[i,m,j])
				DistA[i,m,j]=sqrt(pow(xCA[i,j]-xCA[i,m],2)+ pow(yCA[i,m] - yCA[i,j], 2))
				DistB[i,m,j]=sqrt(pow(xCB[i,j]-xCB[i,m],2)+ pow(yCB[i,m] - yCB[i,j], 2))
				DistD[i,m,j]=sqrt(pow(xCD[i,j]-xCD[i,m],2)+ pow(yCD[i,m] - yCD[i,j], 2))
				
			}
		}
	}	
	#priors for spatial covariance
	#folded t for standard deviation
	for(i in 1:Nstand){
		tauA[i] <- pow(sigA[i],-2)
		sigA[i] <- abs(t.A[i])
		t.A[i] ~ dt(0,p.A[i], 2)
		p.A[i] <- 1/(v.A[i]*v.A[i])
		v.A[i] ~ dunif(0,100)
		
		tauB[i] <- pow(sigB[i],-2)
		sigB[i] <- abs(t.B[i])
		t.B[i] ~ dt(0,p.B[i], 2)
		p.B[i] <- 1/(v.B[i]*v.B[i])
		v.B[i] ~ dunif(0,100)
		
		tauD[i] <- pow(sigD[i],-2)
		sigD[i] <- abs(t.D[i])
		t.D[i] ~ dt(0,p.D[i], 2)
		p.D[i] <- 1/(v.D[i]*v.D[i])
		v.D[i] ~ dunif(0,100)	
		
		#prior for autocorrelation
		phiA[i] <-  log(rhoA[i])
		rhoA[i] ~ dbeta(alphaA[i],betaA[i])
		alphaA[i] ~ dunif(0,100)
		betaA[i] ~ dunif(0,100)
		
		phiB[i] <-  log(rhoB[i])
		rhoB[i] ~ dbeta(alphaB[i],betaB[i])
		alphaB[i] ~ dunif(0,100)
		betaB[i] ~ dunif(0,100)
		
		phiD[i] <-  log(rhoD[i])
		rhoD[i] ~ dbeta(alphaD[i],betaD[i])
		alphaD[i] ~ dunif(0,100)
		betaD[i] ~ dunif(0,100)
	
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
		
		
		
		tau.gs[i]<-pow(sig.gs[i],-2)
		sig.gs[i]~dunif(0,1000)	
		
		
	}

	
	
	
}	