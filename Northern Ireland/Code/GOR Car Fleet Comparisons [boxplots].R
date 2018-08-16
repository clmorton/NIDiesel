#Load data and packages

attach(GOR_Comparison)
library(ggplot2)

# Boxplots of GORs by percentage of car fleet fuelled by diesel

ggplot(GOR_Comparison, aes(x=Region, y=PropDiesel)) + geom_boxplot() theme(axis.text.x=element_text(angle=30, vjust=0.8, hjust=1, face="bold", size=15))
ggbox <- ggplot(GOR_Comparison, aes(x=Region, y=PropDiesel)) + geom_boxplot(outlier.shape = NA)
ggbox <- ggbox + theme(axis.text.x=element_text(color = "black", angle=30, vjust=0.8, hjust=1, size=15))
ggbox <- ggbox + theme(axis.text.y=element_text(color = "black", size=15))
ggbox <- ggbox + theme(axis.title.x=element_text(color = "black", face="bold", size=15))
ggbox <- ggbox + theme(axis.title.y=element_text(color = "black", face="bold", size=15))
ggbox <- ggbox + ylab("Percentage Diesel Cars") + xlab("Government Office Regions")
ggbox

# Boxplots of GORs by percentage of car fleet company cars

ggplot(GOR_Comparison, aes(x=Region, y=PropCompanyCar)) + geom_boxplot() theme(axis.text.x=element_text(angle=30, vjust=0.8, hjust=1, face="bold", size=15))
ggbox <- ggplot(GOR_Comparison, aes(x=Region, y=PropCompanyCar)) + geom_boxplot(outlier.shape = NA)
ggbox <- ggbox + theme(axis.text.x=element_text(color = "black", angle=30, vjust=0.8, hjust=1, size=15))
ggbox <- ggbox + theme(axis.text.y=element_text(color = "black", size=15))
ggbox <- ggbox + theme(axis.title.x=element_text(color = "black", face="bold", size=15))
ggbox <- ggbox + theme(axis.title.y=element_text(color = "black", face="bold", size=15))
ggbox <- ggbox + ylab("Percentage Company Cars") + xlab("Government Office Regions")
ggbox <- ggbox + scale_y_continuous(limits = quantile(GOR_Comparison$PropCompanyCar, c(0.15, 0.85)))
ggbox

# Boxplots of GORs by mean age of the car fleet

ggplot(GOR_Comparison, aes(x=Region, y=Cars..Average.Age)) + geom_boxplot() theme(axis.text.x=element_text(angle=30, vjust=0.8, hjust=1, face="bold", size=15))
ggbox <- ggplot(GOR_Comparison, aes(x=Region, y=Cars..Average.Age)) + geom_boxplot(outlier.shape = NA)
ggbox <- ggbox + theme(axis.text.x=element_text(color = "black", angle=30, vjust=0.8, hjust=1, size=15))
ggbox <- ggbox + theme(axis.text.y=element_text(color = "black", size=15))
ggbox <- ggbox + theme(axis.title.x=element_text(color = "black", face="bold", size=15))
ggbox <- ggbox + theme(axis.title.y=element_text(color = "black", face="bold", size=15))
ggbox <- ggbox + ylab("Mean Age of the Car Stock (years)") + xlab("Government Office Regions")
ggbox <- ggbox + scale_y_continuous(limits = quantile(GOR_Comparison$Cars..Average.Age, c(0.1, 0.9)))
ggbox

# Boxplots of GORs by mean mass of the car fleet

ggplot(GOR_Comparison, aes(x=Region, y=Cars..Av.Mass)) + geom_boxplot() theme(axis.text.x=element_text(angle=30, vjust=0.8, hjust=1, face="bold" ,size=15))
ggbox <- ggplot(GOR_Comparison, aes(x=Region, y=Cars..Av.Mass)) + geom_boxplot(outlier.shape = NA)
ggbox <- ggbox + theme(axis.text.x=element_text(color = "black", angle=30, vjust=0.8, hjust=1, size=15))
ggbox <- ggbox + theme(axis.text.y=element_text(color = "black", size=15))
ggbox <- ggbox + theme(axis.title.x=element_text(color = "black", face="bold", size=15))
ggbox <- ggbox + theme(axis.title.y=element_text(color = "black", face="bold", size=15))
ggbox <- ggbox + ylab("Mean Mass of the Car Stock (kilograms)") + xlab("Government Office Regions")
ggbox <- ggbox + scale_y_continuous(limits = quantile(GOR_Comparison$Cars..Av.Mass, c(0.1, 0.9)))
ggbox

# Boxplots of GORs by mean engine size of the car fleet

ggplot(GOR_Comparison, aes(x=Region, y=Cars..Av.Engine.Size)) + geom_boxplot() theme(axis.text.x=element_text(angle=30, vjust=0.8, hjust=1, face="bold", size=15))
ggbox <- ggplot(GOR_Comparison, aes(x=Region, y=Cars..Av.Engine.Size)) + geom_boxplot(outlier.shape = NA)
ggbox <- ggbox + theme(axis.text.x=element_text(color = "black", angle=30, vjust=0.8, hjust=1, size=15))
ggbox <- ggbox + theme(axis.text.y=element_text(color = "black", size=15))
ggbox <- ggbox + theme(axis.title.x=element_text(color = "black", face="bold", size=15))
ggbox <- ggbox + theme(axis.title.y=element_text(color = "black", face="bold", size=15))
ggbox <- ggbox + ylab("Mean Engine Size of the Car Stock (cubic centimeters)") + xlab("Government Office Regions")
ggbox <- ggbox + scale_y_continuous(limits = quantile(GOR_Comparison$Cars..Av.Engine.Size, c(0.1, 0.9)))
ggbox

