#set up tower coordinates 
towerCoord <- data.frame(site=c("hd","ld"), lat=c(-2226781.44511,-2229282.50998), 
						long=c(-747023.651188,-745712.80962))



#read in coordinate data
datTM <- read.csv("c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\individual_data\\tree_map.csv")

#fix azimuth
#declination for cherskii should be 7 degrees west
#but the declination on the compass was set to 11E

datTM$Azimuthf <- 
				ifelse(datTM$Azimuth.from.reference<18,360-(18-datTM$Azimuth.from.reference),
									datTM$Azimuth.from.reference-18)

									
#calculate coordinates for each tree that is from the tower
datTM$rad <- datTM$Azimuthf*(pi/180)

datTM$deltalat <- datTM$Distance.from.reference..m.*cos(datTM$rad) 
datTM$deltalon <- datTM$Distance.from.reference..m.*sin(datTM$rad) 



datTMp1 <- datTM[datTM$reference=="tower",]
datTMp2 <- datTM[datTM$reference!="tower",]

datTMp1$lat <- ifelse(datTMp1$stand=="hd", datTMp1$deltalat+towerCoord$lat[1],datTMp1$deltalat+towerCoord$lat[2])
datTMp1$long <- ifelse(datTMp1$stand=="hd", datTMp1$deltalon+towerCoord$long[1],datTMp1$deltalon+towerCoord$long[2])


t11refhd <- datTMp1[which(datTMp1$treeID==11&datTMp1$stand=="hd"),]

datTMp2$lat <- datTMp2$deltalat+t11refhd$lat
datTMp2$long <- datTMp2$deltalon+t11refhd$lon

datptsA <- rbind(datTMp1,datTMp2)

#reformat for a output gis and R table

datptsOUT <- data.frame(year=datptsA$year,site=datptsA$stand, treeID =datptsA$treeID, lat=datptsA$lat,long=datptsA$long)

datptsOUT2 <- datptsOUT[order(datptsOUT$year,datptsOUT$site, datptsOUT$treeID),]


write.table(datptsOUT2,"c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\individual_data\\tree_coord_out.csv", sep=",", row.names=FALSE)
write.table(datptsOUT2,"c:\\Users\\hkropp\\Google Drive\\Viper_Ecohydro\\individual_data\\tree_coord_outGIS4.txt", sep="\t", row.names=FALSE)

