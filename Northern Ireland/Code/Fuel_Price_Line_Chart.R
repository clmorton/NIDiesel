attach(Fuel_Price_Long)
library(ggplot2)

p1 <- ggplot() + geom_line(aes(y = Price, x = Year, colour = Nation), size=1.5,
                           data = Fuel_Price_Long, stat="identity") +
  ylab("Price of Diesel (pence per litre)") +
  xlab("Year") +
  theme(axis.title.y=element_text(color = "black", face="bold")) +
  theme(axis.title.x=element_text(color = "black", face="bold")) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank())
p1