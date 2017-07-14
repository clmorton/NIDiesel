#Load Data
setwd(...\Northern Ireland)
attach(Integrated_Spreadsheet)

#Load Packages
library(psych)
library(car)
library(ggplot2)
library(Hmisc)
library(corrplot)

#Histograms of Proportion Diesel

hist(PropDiesel, breaks = 25, xlab = "Mean Percentage Diesel Cars", main = NULL, font.lab = 2, col = "gray80")
hist(log(PropDiesel), breaks = 25, xlab = "Mean Percentage Diesel Cars", main = NULL, font.lab = 2, col = "gray80")

#Descriptive Statistics

describe(Integrated_Spreadsheet)

#Bivariate Analaysis with Socioeconomics 

pairs.panels(Integrated_Spreadsheet[c(6:17)], method = "spearman", hist.col = "gray80", ellipses = "FALSE", breaks = 25)
cor1 <- rcorr(as.matrix(Integrated_Spreadsheet[c(6:17)]))
cor1
cor1$r
cor1$P
corrplot(cor1$r, type="upper", order="original", 
         p.mat = cor1$P, sig.level = 0.01, insig = "blank", tl.srt = 45, tl.col = "black")

#Bivariate Analaysis with Travel 

pairs.panels(Integrated_Spreadsheet[c(6,18:32)], method = "spearman", hist.col = "gray80", ellipses = "FALSE", breaks = 25)
cor2 <- rcorr(as.matrix(Integrated_Spreadsheet[c(6,18:32)]))
cor2
cor2$r
cor2$P
corrplot(cor2$r, type="upper", order="original", 
         p.mat = cor2$P, sig.level = 0.01, insig = "blank", tl.srt = 45, tl.col = "black")

#Bivariate Analaysis with Household

pairs.panels(Integrated_Spreadsheet[c(6,33:42)], method = "spearman", hist.col = "gray80", ellipses = "FALSE", breaks = 25)
cor3 <- rcorr(as.matrix(Integrated_Spreadsheet[c(6,33:42)]))
cor3
cor3$r
cor3$P
corrplot(cor3$r, type="upper", order="original", 
         p.mat = cor3$P, sig.level = 0.01, insig = "blank", tl.srt = 45, tl.col = "black")

#Scatterplots between Percentage Diesel and Nearness to Border

ggplot(Integrated_Spreadsheet, aes(x=DistCross, y=PropDiesel)) +
  geom_point(shape=1) + 
  xlab("Euclidean Distance to Closest Border Crossing (km)") +
  ylab("Diesel Cars (%)") +
  theme(axis.title.y=element_text(color = "black", face="bold")) +
  theme(axis.title.x=element_text(color = "black", face="bold")) +
  geom_smooth(method=lm,
              se=FALSE)
  
ggplot(Integrated_Spreadsheet, aes(x=NetDist, y=PropDiesel)) +
    geom_point(shape=1) + 
    xlab("Network Distance to Closest Fuel Station in the Republic (km)") +
    ylab("Diesel Cars (%)") +
    theme(axis.title.y=element_text(color = "black", face="bold")) +
    theme(axis.title.x=element_text(color = "black", face="bold")) +
  geom_smooth(method=lm,
              se=FALSE)

ggplot(Integrated_Spreadsheet, aes(x=NetTime, y=PropDiesel)) +
    geom_point(shape=1) + 
    xlab("Network Time to to Closest Fuel Station in the Republic (minutes)") +
    ylab("Diesel Cars (%)") +
    theme(axis.title.y=element_text(color = "black", face="bold")) +
    theme(axis.title.x=element_text(color = "black", face="bold")) +
  geom_smooth(method=lm,
              se=FALSE)

#Boxplots of Buffer Categories against Percentage Diesel and Kruskal Wallis Test

  Integrated_Spreadsheet$BuffCat <- factor(Integrated_Spreadsheet$BuffCat, levels=c("Five", "Ten", "Fifteen", "Twenty", "Rest of NI"))
  ggplot(Integrated_Spreadsheet, aes(x=Integrated_Spreadsheet$BuffCat, y=PropDiesel)) + geom_boxplot() theme(axis.text.x=element_text(angle=30, vjust=0.8, hjust=1, face="bold"))
  ggbox <- ggplot(Integrated_Spreadsheet, aes(x=Integrated_Spreadsheet$BuffCat, y=PropDiesel)) + geom_boxplot(outlier.shape = NA)
  ggbox <- ggbox + theme(axis.text.x=element_text(color = "black", angle=30, vjust=0.8, hjust=1))
  ggbox <- ggbox + theme(axis.title.x=element_text(color = "black", face="bold"))
  ggbox <- ggbox + theme(axis.title.y=element_text(color = "black", face="bold"))
  ggbox <- ggbox + ylab("Percentage Diesel Cars") + xlab("Super Output Areas with Set Buffers to the Border with the Republic of Ireland")
  ggbox
  
  kruskal.test(PropDiesel ~ BuffCat, data = Integrated_Spreadsheet)
  
# Benchmark OLS log-log Regression Models

mod1 <- lm(PropDiesLN ~ MeanAgeLN + SelfEmpLN + Level4LN)
summary(mod1)
par(mfrow=c(2,2))
plot(mod1, main = "Socioeconomic Model")
vif(mod1)
anova(mod1)

mod2 <- lm(PropDiesLN ~ OneCarLN + CarDrivLN + Over30LN + PopDLN + MeanResLN + RentSocLN + FlatsLN)
summary(mod2)
par(mfrow=c(2,2))
plot(mod2, main = "Travel and Household Model")
vif(mod2)
anova(mod2)

mod3 <- lm(PropDiesLN ~ MeanAgeLN + SelfEmpLN + Level4LN + OneCarLN + CarDrivLN + Over30LN + PopDLN + MeanResLN + RentSocLN + FlatsLN)
summary(mod3)
par(mfrow=c(2,2))
plot(mod3, main = "Integrated Model")
vif(mod3)
anova(mod3)

mod4 <- lm(PropDiesLN ~ MeanAgeLN + SelfEmpLN + Level4LN + OneCarLN + CarDrivLN + Over30LN + PopDLN + MeanResLN + RentSocLN + FlatsLN + DistCrossLN)
summary(mod4)
par(mfrow=c(2,2))
plot(mod4, main = "Integrated Model with Proximity to Border")
vif(mod4)
anova(mod4)

mod5 <- lm(PropDiesLN ~ MeanAgeLN + SelfEmpLN + Level4LN + OneCarLN + CarDrivLN + Over30LN + PopDLN + MeanResLN + RentSocLN + FlatsLN + FiveKBuff + TenKBuff + FiftKBuff + TwentKBuff)
summary(mod5)
par(mfrow=c(2,2))
plot(mod5, main = "Integrated Model with Buffer to Border")
vif(mod5)
anova(mod5)

mod6 <- lm(PropDiesLN ~ MeanAgeLN + SelfEmpLN + Level4LN + OneCarLN + CarDrivLN + Over30LN + PopDLN + MeanResLN + RentSocLN + FlatsLN + NetDistLN)
summary(mod6)
par(mfrow=c(2,2))
plot(mod6, main = "Integrated Model with Network Distance to Border")
vif(mod6)
anova(mod6)

mod7 <- lm(PropDiesLN ~ MeanAgeLN + SelfEmpLN + Level4LN + OneCarLN + CarDrivLN + Over30LN + PopDLN + MeanResLN + RentSocLN + FlatsLN + NetTimeLN)
summary(mod7)
par(mfrow=c(2,2))
plot(mod7, main = "Integrated Model with Network Time to Border")
vif(mod7)
anova(mod7)