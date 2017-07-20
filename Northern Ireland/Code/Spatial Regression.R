#Load Packages
library(spdep)
library(rgdal)
library(maptools)

#Load Data
setwd(.../Northern Ireland)
SpatDat <- readOGR("Shapefiles","SOA2011_Linked")
summary(SpatDat)

#Create Spatial Weights
continuity.nb <- poly2nb(SpatDat, queen = TRUE)
plot(SpatDat, border = "grey")
plot(continuity.nb, coordinates(SpatDat), add = TRUE, col = "blue")
continuity.listw <- nb2listw(continuity.nb)
summary(continuity.listw)

#Autocorrelation Analysis
moran.test(SpatDat$IS_PropDie,continuity.listw, randomisation=FALSE, alternative="two.sided")
moran.plot(SpatDat$IS_PropDie, continuity.listw)
LMIresult <- localmoran(SpatDat$IS_PropDie, continuity.listw)
LMImap <- spCbind(SpatDat, as.data.frame(LMIresult))
spplot(LMImap, "Z.Ii")

#Spatial log-normal Regression Models

#Spatial Lag Model
mod7SLM <- lagsarlm(PropDiesLN ~ MeanAgeLN + SelfEmpLN + Level4LN + OneCarLN +
                    CarDrivLN + +Over30LN + PopDLN + MeanResLN + RentSocLN + FlatsLN + NetDistLN, 
                    data = SpatDat, continuity.listw)
summary(mod7SLM)

#Spatial Error Model
mod7SEM <- errorsarlm(PropDiesLN ~ MeanAgeLN + SelfEmpLN + Level4LN + OneCarLN +
                        CarDrivLN + Over30LN + PopDLN + MeanResLN + RentSocLN + FlatsLN + NetDistLN, 
                      data = SpatDat, continuity.listw)
summary(mod7SEM)

#Spatial Durbin Model
mod7SDM <- lagsarlm(PropDiesLN ~ MeanAgeLN + SelfEmpLN + Level4LN + OneCarLN +
                      CarDrivLN + Over30LN + PopDLN + MeanResLN + RentSocLN + FlatsLN + NetDistLN, 
                    data = SpatDat, continuity.listw, type = "mixed")
summary(mod7SDM)

#Spatial Durbin Error Model
mod7SDEM <- errorsarlm(PropDiesLN ~ MeanAgeLN + SelfEmpLN + Level4LN + OneCarLN +
                         CarDrivLN + Over30LN + PopDLN + MeanResLN + RentSocLN + FlatsLN + NetDistLN, 
                      data = SpatDat, continuity.listw, etype = "emixed")
summary(mod7SDEM)

#Simultaneous Autoregressive Model
mod7SAR <- spautolm(PropDiesLN ~ MeanAgeLN + SelfEmpLN + Level4LN + OneCarLN +
                      CarDrivLN + Over30LN + PopDLN + MeanResLN + RentSocLN + FlatsLN + NetDistLN, 
                    data = SpatDat, listw = continuity.listw, family = "SAR", method = "eigen")
summary(mod7SAR)
mod7resSAR <- MCMCsamp(mod7SAR, mcmc = 5000, burnin = 500, listw = continuity.listw)
summary(mod7resSAR)

#Conditional Autoregressive Model
mod7CAR <- spautolm(PropDiesLN ~ MeanAgeLN + SelfEmpLN + Level4LN + OneCarLN +
                      CarDrivLN + Over30LN + PopDLN + MeanResLN + RentSocLN + FlatsLN + NetDistLN, 
                    data = SpatDat, listw = continuity.listw, family = "CAR", method = "eigen")
summary(mod7CAR)
mod7resCAR <- MCMCsamp(mod7CAR, mcmc = 5000, burnin = 500, listw = continuity.listw)
summary(mod7resCAR)
