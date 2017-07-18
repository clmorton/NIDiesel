#Attach the dataset to R
attach(Diesel_Share)

#Load packages necessary for the work
library(ggplot2)
library(tidyr)

#Transform the wide data in the attached dataframe into long data
long <- gather(Diesel_Share, Year, Diesel, c(2:28))
View(long)

#Create the line chart               
p1 <- ggplot() + geom_line(aes(y = Diesel, x = Year, group = Country, colour = Country), size=1.5,
                           data = long, stat="identity") +
  ylab("Percentage of New Car Sales Diesel Fuelled") +
  xlab("Year") +
  theme(axis.title.y=element_text(color = "black", face="bold")) +
  theme(axis.title.x=element_text(color = "black", face="bold")) +
  theme(legend.position="right", legend.direction="vertical", legend.title = element_blank())
p1

