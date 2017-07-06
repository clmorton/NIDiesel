#Load Data
setwd(.../Northern Ireland)
attach(Integrated_Spreadsheet)

#Load Packages
library(psych)
library(car)

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

# Benchmark OLS log-normal Regression Models

mod1 <- lm(log(PropDiesel) ~ MeanAge + SelfEmployed + Level4)
summary(mod1)
par(mfrow=c(2,2))
plot(mod1, main = "Socioeconomic Model")
vif(mod1)
anova(mod1)

mod2 <- lm(log(PropDiesel) ~ OneCar + CarDrive + PopDens + MeanResidents + RentSocial + Flats)
summary(mod2)
par(mfrow=c(2,2))
plot(mod2, main = "Travel and Household Model")
vif(mod2)
anova(mod2)

mod3 <- lm(log(PropDiesel) ~ MeanAge + SelfEmployed + Level4 + OneCar + CarDrive + PopDens + MeanResidents + RentSocial + Flats)
summary(mod3)
par(mfrow=c(2,2))
plot(mod3, main = "Integrated Model")
vif(mod3)
anova(mod3)
