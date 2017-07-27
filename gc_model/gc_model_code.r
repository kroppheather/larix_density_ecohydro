##############################
###model code for gc model ###
###across stand density grad##
##############################

Model{
#Model likelihood
	for(i in 1:Nobs){
	gs[i]~dnorm(mu.gs[i],tau.gs)
	gs.rep[i]~dnorm(mu.gs[i],tau.gs)
	#model for mean gs
	mu.gs[i]<-oren.mod[i]*light[i]

	#light scaling function
	light[i]<-1-exp(-l.slope[daySiteID[i]]*PAR[i])
	
	#oren model 1999 for mean gs
	oren.mod[i]<-gref[daySiteID[i]]*(1-S[daySiteID[i]]*log(D[i]))

	}
	
}	