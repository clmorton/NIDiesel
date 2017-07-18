#a script to read in the binned travel to work distances
#for northern Ireland SOAs.  
#Each SOA has its own excel spreadsheet
#Ian 130717


install.packages("readxl")
library(readxl)
library(readr)

help("readxl")
xls_example <- read_excel("SOA/LC7701NI_95AA01S1.xlsx")





#-----read in data ---------------------------
#when I wrote this script I'd already 
#downloaded the data and upzipped it.  

#this block of code will read in all the files in the directory called "SOA"

filenames <- list.files("SOA", pattern="*.xlsx", full.names=TRUE)
#filenames
#(basename(filenames))

SOAdata <- data.frame(allworkpop = integer(0), 
                      home = integer(0),
                      under10 = integer(0) ,
                      ten30 = integer(0), 
                      over30 = integer(0),
                      other = integer(0))
str(SOAdata)



for (i in 1:length(filenames)){
#for (i in 1:5){
    
  assign(basename(filenames[i]),read_excel(filenames[i])) 
  #gets cells in the row
  #All usual residents aged 16 to 74 (excluding students) in employment and currently working
  #cols are all home, less10 ten30 over30 other
  #assign(basename(filenames[i]),read_excel(filenames[i],range = "B8:G8", col_names=FALSE))  
  assign(basename(filenames[i]),read_excel(filenames[i],range = "B8:G8", col_names=c("allworkpop", "home", "under10", "ten30", "over30", "other")))  
  
  #SOAdata <- rbind(SOAdata, basename(filenames[i]))
  SOAdata <- rbind(SOAdata, get(basename(filenames[i])))
  
}

#don't tell the r police that I used a loop instead of apply


#SOAdata <- rbind(SOAdata, get(basename(filenames[i])))
SOAdata2 <- SOAdata

#binds the SOA identifier onto the dataframe
SOAdata2 <- cbind(SOAdata, (basename(filenames)))

#shortens the identifier so it is just the SOA cde
#so it should join to other SOA data
substr(basename(filenames[i]), 10, 17)


names(SOAdata2)

#instead of fix, you could be a fancy pants and do it programatically
fix(SOAdata2) # change to basename
SOAdata2$SOAname <- substr(SOAdata2$basename, 10, 17)

#output the file
write_csv(SOAdata2, "SOAbyCommuteDist.csv")






