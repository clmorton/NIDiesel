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
mod3SLM <- lagsarlm(log(IS_PropDie) ~ IS_MeanAge + IS_SelfEmp + IS_Level4 + IS_OneCar +
                    IS_CarDriv + IS_PopDens + IS_MeanRes + IS_RentSoc + IS_Flats, 
                    data = SpatDat, continuity.listw)
summary(mod3SLM)

#Spatial Error Model
mod3SEM <- errorsarlm(log(IS_PropDie) ~ IS_MeanAge + IS_SelfEmp + IS_Level4 + IS_OneCar +
                        IS_CarDriv + IS_PopDens + IS_MeanRes + IS_RentSoc + IS_Flats, 
                      data = SpatDat, continuity.listw)
summary(mod3SEM)

#Spatial Durbin Model
mod3SDM <- lagsarlm(log(IS_PropDie) ~ IS_MeanAge + IS_SelfEmp + IS_Level4 + IS_OneCar +
                      IS_CarDriv + IS_PopDens + IS_MeanRes + IS_RentSoc + IS_Flats, 
                    data = SpatDat, continuity.listw, type = "mixed")
summary(mod3SDM)

#Spatial Durbin Error Model
mod3SDEM <- errorsarlm(log(IS_PropDie) ~ IS_MeanAge + IS_SelfEmp + IS_Level4 + IS_OneCar +
                        IS_CarDriv + IS_PopDens + IS_MeanRes + IS_RentSoc + IS_Flats, 
                      data = SpatDat, continuity.listw, etype = "emixed")
summary(mod3SDEM)

#Simultaneous Autoregressive Model
mod3SAR <- spautolm(log(IS_PropDie) ~ IS_MeanAge + IS_SelfEmp + IS_Level4 + IS_OneCar +
                      IS_CarDriv + IS_PopDens + IS_MeanRes + IS_RentSoc + IS_Flats, 
                    data = SpatDat, listw = continuity.listw, family = "SAR", method = "eigen")
summary(mod3SAR)
mod3resSAR <- MCMCsamp(mod3SAR, mcmc = 5000, burnin = 500, listw = continuity.listw)
summary(mod3resSAR)

#Conditional Autoregressive Model
mod3CAR <- spautolm(log(IS_PropDie) ~ IS_MeanAge + IS_SelfEmp + IS_Level4 + IS_OneCar +
                      IS_CarDriv + IS_PopDens + IS_MeanRes + IS_RentSoc + IS_Flats, 
                    data = SpatDat, listw = continuity.listw, family = "CAR", method = "eigen")
summary(mod3CAR)
mod3resCAR <- MCMCsamp(mod3CAR, mcmc = 5000, burnin = 500, listw = continuity.listw)
summary(mod3resCAR)
