###########################################################################
###########################################################################
############## Created by Heather Kropp in October 2017      ##############
############## This script creates figures for all time      ##############
############## series data.                                  ##############
###########################################################################
###########################################################################
############## Input files:                                  ##############
############## from sapflux calc:                            ##############
############## Transpiration: El.L,El.L17,El.H,El.H17        ##############
############## stomatal conductance:gc.L, gc.L17, gc.H, gc.H17#############
############## tree info: datTreeL, datTreeL17, datTreeH,     #############
##############            datTreeH17                          #############
##############  from thaw depth: TDall                        #############
###########################################################################


#set the plotting directory
plotDI <- "c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\time_plot"

#################################################################
####read in sapflow data                                  #######
#################################################################
source("c:\\Users\\hkropp\\Documents\\GitHub\\larch_density_ecohydro\\sapflux_process.r")
#libraries loaded from source
#plyr, lubridate,caTools


#################################################################
####read in thaw depth data                               #######
#################################################################

source("c:\\Users\\hkropp\\Documents\\GitHub\\larch_density_ecohydro\\thaw_depth_process.r")

#################################################################
####read in datafiles                                     #######
#################################################################

#read in precip data
datAirP <- read.csv("c:\\Users\\hkropp\\Google Drive\\viperSensor\\airport\\airport.csv")

#read in continuous soil data
datSW <- read.csv("c:\\Users\\hkropp\\Google Drive\\viperSensor\\soil\\vwc.GS3.csv")

#canopy rh and temperature
datRH <- read.csv("c:\\Users\\hkropp\\Google Drive\\viperSensor\\met\\RH.VP4.csv")
datTC <- read.csv("c:\\Users\\hkropp\\Google Drive\\viperSensor\\met\\TempC.VP4.csv")

#PAR
datPAR <- read.csv("c:\\Users\\hkropp\\Google Drive\\viperSensor\\met\\PAR.QSOS PAR.csv")