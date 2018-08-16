attach(Diesel_Share_UK)
library(ggplot2)

p1 <- ggplot() + geom_line(aes(y = Share, x = Year, colour = Fuel), size=1.5,
                           data = Diesel_Share_UK, stat="identity") +
  ylab("New Car Registrations by Fuel Type (percent)") +
  xlab("Year") +
  theme(axis.title.y=element_text(color = "black", face="bold")) +
  theme(axis.title.x=element_text(color = "black", face="bold")) +
  theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank())
p1