#Load Data
setwd(...\Northern Ireland)
attach(Integrated_Spreadsheet)

#Load Packages
library(psych)
library(car)
library(ggplot2)

#Histograms of Proportion Diesel

hist(PropDiesel, breaks = 25, xlab = "Mean Percentage Diesel Cars", main = NULL, font.lab = 2, col = "gray80")
hist(log(PropDiesel), breaks = 25, xlab = "Mean Percentage Diesel Cars", main = NULL, font.lab = 2, col = "gray80")

#Descriptive Statistics

describe(Integrated_Spreadsheet)

#Bivariate Analaysis with Socioeconomics 

pairs.panels(Integrated_Spreadsheet[c(6:17)], method = "spearman", hist.col = "gray80", ellipses = "FALSE", breaks = 25)

#Bivariate Analaysis with Travel 

pairs.panels(Integrated_Spreadsheet[c(6,18:29)], method = "spearman", hist.col = "gray80", ellipses = "FALSE", breaks = 25)

#Bivariate Analaysis with Household

pairs.panels(Integrated_Spreadsheet[c(6,30:39)], method = "spearman", hist.col = "gray80", ellipses = "FALSE", breaks = 25)

#Scatterplots between Percentage Diesel and Nearness to Border

ggplot(Integrated_Spreadsheet, aes(x=DistCross, y=PropDiesel)) +
  geom_point(shape=1) + 
  xlab("Euclidean Distance to Closest Border Crossing (km)") +
  ylab("Diesel Cars (%)")
  geom_smooth(method=lm,
              se=FALSE)
  
ggplot(Integrated_Spreadsheet, aes(x=NetDist, y=PropDiesel)) +
    geom_point(shape=1) + 
    xlab("Network Distance to Closest Fuel Station in the Republic (km)") +
    ylab("Diesel Cars (%)")
  geom_smooth(method=lm,
              se=FALSE)

ggplot(Integrated_Spreadsheet, aes(x=NetTime, y=PropDiesel)) +
    geom_point(shape=1) + 
    xlab("Network Time to to Closest Fuel Station in the Republic (minutes)") +
    ylab("Diesel Cars (%)")
  geom_smooth(method=lm,
              se=FALSE)

# Benchmark OLS log-log Regression Models

mod1 <- lm(PropDiesLN ~ MeanAgeLN + SelfEmpLN + Level4LN)
summary(mod1)
par(mfrow=c(2,2))
plot(mod1, main = "Socioeconomic Model")
vif(mod1)
anova(mod1)

mod2 <- lm(PropDiesLN ~ OneCarLN + CarDrivLN + PopDLN + MeanResLN + RentSocLN + FlatsLN)
summary(mod2)
par(mfrow=c(2,2))
plot(mod2, main = "Travel and Household Model")
vif(mod2)
anova(mod2)

mod3 <- lm(PropDiesLN ~ MeanAgeLN + SelfEmpLN + Level4LN + OneCarLN + CarDrivLN + PopDLN + MeanResLN + RentSocLN + FlatsLN)
summary(mod3)
par(mfrow=c(2,2))
plot(mod3, main = "Integrated Model")
vif(mod3)
anova(mod3)

mod4 <- lm(PropDiesLN ~ MeanAgeLN + SelfEmpLN + Level4LN + OneCarLN + CarDrivLN + PopDLN + MeanResLN + RentSocLN + FlatsLN + DistCrossLN)
summary(mod4)
par(mfrow=c(2,2))
plot(mod4, main = "Integrated Model with Proximity to Border")
vif(mod4)
anova(mod4)

mod5 <- lm(PropDiesLN ~ MeanAgeLN + SelfEmpLN + Level4LN + OneCarLN + CarDrivLN + PopDLN + MeanResLN + RentSocLN + FlatsLN + FiveKBuff + TenKBuff + FiftKBuff + TwentKBuff)
summary(mod5)
par(mfrow=c(2,2))
plot(mod5, main = "Integrated Model with Buffer to Border")
vif(mod5)
anova(mod5)

mod6 <- lm(PropDiesLN ~ MeanAgeLN + SelfEmpLN + Level4LN + OneCarLN + CarDrivLN + PopDLN + MeanResLN + RentSocLN + FlatsLN + NetDistLN)
summary(mod6)
par(mfrow=c(2,2))
plot(mod6, main = "Integrated Model with Network Distance to Border")
vif(mod6)
anova(mod6)

mod7 <- lm(PropDiesLN ~ MeanAgeLN + SelfEmpLN + Level4LN + OneCarLN + CarDrivLN + PopDLN + MeanResLN + RentSocLN + FlatsLN + NetTimeLN)
summary(mod7)
par(mfrow=c(2,2))
plot(mod7, main = "Integrated Model with Network Time to Border")
vif(mod7)
anova(mod7)