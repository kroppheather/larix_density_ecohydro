###########################################################################
###########################################################################
############## Created by Heather Kropp in October 2017      ##############
############## This script is to be run for all analyses     ##############
############## associated with patterns in vwc between sites ##############
###########################################################################
###########################################################################

#################################################################
####designate if plotting should be done                  #######
#################################################################
#set to 1 if plotting, zero otherwise
plotcheck <- 1

#directory to plot
pdir <- "c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\soil_plot"

#################################################################
####read in datafiles                                     #######
#################################################################
datGV <- read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\individual_data\\grav_to_vwc.csv")


#################################################################
####compare samples to procheck vwc                       #######
#################################################################
#see how a gravimetric derived versus procheck
#measurements compare for all soil types

soillessF <- lm(datGV$vwc.samp~datGV$vwc.soilless)	
mineralF <- lm(datGV$vwc.samp~datGV$vwc.mineral)	
customF <- lm(datGV$vwc.samp~datGV$vwc.custom)

#plot of all soil types
if(plotcheck==1){
jpeg(file=paste0(pdir, "\\vwc_comp_all",".jpg"), width=1000, height=700, units="px")
par(mfrow=c(1,2))
plot(datGV$vwc.mineral, datGV$vwc.samp, pch=19, 
		col="tomato3", xlab="procheck derived vwc", ylab="gravimetric derived vwc",
			xlim=c(0,1), ylim=c(0,1))
points(datGV$vwc.soilless, datGV$vwc.samp, pch=19, 
		col="royalblue3",)
abline(soillessF, col="royalblue3")	
abline(mineralF, col="tomato3")
abline(0,1, col="black", lty=2)
legend(0,1, c("mineral", "soilless"), pch=19, col=c("tomato3", "royalblue3"), bty="n")		
text(0.55,.95, paste("y=", round(mineralF$coefficients[1],3),"+",
						round(mineralF$coefficients[2],3),"*mineral R2=",
						round(summary(mineralF)$r.squared,2)))							
text(0.55,.85, paste("y=", round(soillessF$coefficients[1],3),"+",
						round(soillessF$coefficients[2],3),"*soilless R2=",
						round(summary(soillessF)$r.squared,2)))	
						
plot(datGV$vwc.custom, datGV$vwc.samp, pch=19, 
		col="tomato3", xlab="procheck derived vwc", ylab="gravimetric derived vwc",
			 ylim=c(0,1))			
abline(customF, col="tomato3")
legend(0,1, c("custom"), pch=19, col=c("tomato3"), bty="n")			 
		 
text(45,.95, paste("y=", round(customF$coefficients[1],3),"+",
						round(customF$coefficients[2],3),"*custom R2=",
						round(summary(customF)$r.squared,2)))
dev.off()						
}		

#break up soil types	
soillessFm <- lm(datGV$vwc.samp[datGV$type=="moss"]~datGV$vwc.soilless[datGV$type=="moss"])	
mineralFm <- lm(datGV$vwc.samp[datGV$type=="moss"]~datGV$vwc.mineral[datGV$type=="moss"])	
customFo <- lm(datGV$vwc.samp[datGV$type=="moss"]~datGV$vwc.custom[datGV$type=="moss"])
soillessFo <- lm(datGV$vwc.samp[datGV$type=="organic"]~datGV$vwc.soilless[datGV$type=="organic"])	
mineralFo <- lm(datGV$vwc.samp[datGV$type=="organic"]~datGV$vwc.mineral[datGV$type=="organic"])	
customFo <- lm(datGV$vwc.samp[datGV$type=="organic"]~datGV$vwc.custom[datGV$type=="organic"])

#plot all soil types
if(plotcheck==1){
jpeg(file=paste0(pdir, "\\vwc_comp_type",".jpg"), width=1500, height=700, units="px")
par(mfrow=c(1,4))
#moss
plot(datGV$vwc.mineral[datGV$type=="moss"], datGV$vwc.samp[datGV$type=="moss"], pch=19, 
		col="tomato3", xlab="procheck derived vwc", ylab="gravimetric derived vwc",
			xlim=c(0,1), ylim=c(0,1), main="moss")
points(datGV$vwc.soilless[datGV$type=="moss"], datGV$vwc.samp[datGV$type=="moss"], pch=19, 
		col="royalblue3",)
abline(soillessFm, col="royalblue3")	
abline(mineralFm, col="tomato3")
abline(0,1, col="black", lty=2)
legend(0,1, c("mineral", "soilless"), pch=19, col=c("tomato3", "royalblue3"), bty="n")		
text(0.55,.95, paste("y=", round(mineralFm$coefficients[1],3),"+",
						round(mineralFm$coefficients[2],3),"*mineral R2=",
						round(summary(mineralFm)$r.squared,2)))							
text(0.55,.85, paste("y=", round(soillessFm$coefficients[1],3),"+",
						round(soillessFm$coefficients[2],3),"*soilless R2=",
						round(summary(soillessFm)$r.squared,2)))	
						
#organic
plot(datGV$vwc.mineral[datGV$type=="organic"], datGV$vwc.samp[datGV$type=="organic"], pch=19, 
		col="tomato3", xlab="procheck derived vwc", ylab="gravimetric derived vwc",
			xlim=c(0,1), ylim=c(0,1), main="organic")
points(datGV$vwc.soilless[datGV$type=="organic"], datGV$vwc.samp[datGV$type=="organic"], pch=19, 
		col="royalblue3",)
abline(soillessFo, col="royalblue3")	
abline(mineralFo, col="tomato3")
abline(0,1, col="black", lty=2)
legend(0,1, c("mineral", "soilless"), pch=19, col=c("tomato3", "royalblue3"), bty="n")		
text(0.55,.95, paste("y=", round(mineralFo$coefficients[1],3),"+",
						round(mineralFo$coefficients[2],3),"*mineral R2=",
						round(summary(mineralFo)$r.squared,2)))							
text(0.55,.85, paste("y=", round(soillessFo$coefficients[1],3),"+",
						round(soillessFo$coefficients[2],3),"*soilless R2=",
						round(summary(soillessFo)$r.squared,2)))							
						
						
						
#moss
plot(datGV$vwc.custom[datGV$type=="moss"], datGV$vwc.samp[datGV$type=="moss"], pch=19, 
		col="tomato3", xlab="procheck derived vwc", ylab="gravimetric derived vwc",
			 ylim=c(0,1), main="moss")			
abline(customFm, col="tomato3")
legend(0,1, c("custom"), pch=19, col=c("tomato3"), bty="n")			 
		 
text(25,.95, paste("y=", round(customFm$coefficients[1],3),"+",
						round(customFm$coefficients[2],3),"*custom R2=",
						round(summary(customFm)$r.squared,2)))
#organic
plot(datGV$vwc.custom[datGV$type=="organic"], datGV$vwc.samp[datGV$type=="organic"], pch=19, 
		col="tomato3", xlab="procheck derived vwc", ylab="gravimetric derived vwc",
			 ylim=c(0,1), main="organic")			
abline(customFo, col="tomato3")
legend(0,1, c("custom"), pch=19, col=c("tomato3"), bty="n")			 
		 
text(45,.95, paste("y=", round(customFo$coefficients[1],3),"+",
						round(customFo$coefficients[2],3),"*custom R2=",
						round(summary(customFo)$r.squared,2)))						
						
dev.off()							
}