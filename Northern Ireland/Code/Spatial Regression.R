#Load Packages
library(spdep)
library(rgdal)
library(maptools)
library(CARBayes)

#Load Data
setwd(.../Northern Ireland)
SpatDat <- readOGR("Shapefiles","SOA2011_Linked")
summary(SpatDat)

#Create Spatial Weights
continuity.nb <- poly2nb(SpatDat, queen = TRUE)
plot(SpatDat, border = "grey")
plot(continuity.nb, coordinates(SpatDat), add = TRUE, col = "blue")
continuity.listw <- nb2listw(continuity.nb)
W <- nb2mat(continuity.nb, style="B")
summary(continuity.listw)

#Autocorrelation Analysis
moran.test(SpatDat$IS_PropDie,continuity.listw, randomisation=FALSE, alternative="two.sided")
moran.plot(SpatDat$IS_PropDie, continuity.listw)
LMIresult <- localmoran(SpatDat$IS_PropDie, continuity.listw)
LMImap <- spCbind(SpatDat, as.data.frame(LMIresult))
spplot(LMImap, "Z.Ii")

#Spatial log-log Regression Models

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
W <- as(continuity.listw, "CsparseMatrix")
trMatc <- trW(W, type = "mult")
summary(impacts(mod7SDM, tr=trMatc, R=200), zstats=TRUE, short = TRUE)

#Spatial Durbin Error Model
mod7SDEM <- errorsarlm(PropDiesLN ~ MeanAgeLN + SelfEmpLN + Level4LN + OneCarLN +
                         CarDrivLN + Over30LN + PopDLN + MeanResLN + RentSocLN + FlatsLN + NetDistLN, 
                      data = SpatDat, continuity.listw, etype = "emixed")
summary(mod7SDEM)
summary(impacts(mod7SDEM))

#Simultaneous Autoregressive Model
mod7SAR <- spautolm(PropDiesLN ~ MeanAgeLN + SelfEmpLN + Level4LN + OneCarLN +
                      CarDrivLN + Over30LN + PopDLN + MeanResLN + RentSocLN + FlatsLN + NetDistLN, 
                    data = SpatDat, listw = continuity.listw, family = "SAR", method = "eigen")
summary(mod7SAR)
mod7resSAR <- MCMCsamp(mod7SAR, mcmc = 5000, burnin = 500, listw = continuity.listw)
summary(mod7resSAR)

#Conditional Autoregressive Model

form <- PropDiesLN ~ MeanAgeLN + SelfEmpLN + Level4LN + OneCarLN +
  CarDrivLN + Over30LN + PopDLN + MeanResLN + RentSocLN + FlatsLN + NetDistLN

mod7CAR <- S.CARleroux(formula=form, data=SpatDat,
                             family="gaussian", W=W, burnin=20000, n.sample=120000, thin=10)

print(mod7CAR)
