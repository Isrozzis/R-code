 ## how to make a line graph for each site on a single day for each hour for a single pollutant 

 ## read in the data
hourly_O3 <- read.csv("C:/Users/Trevor/Documents/Air/hourly_ozone.csv")

 ##Clean up the negative values. They should not exist
hourly_O3$value[hourly_O3$value < 0] <- NA

 ## Create and clean up a data frame for a single day
df <- hourly_O3[hourly_O3$date == "2008-08-01", ]
df <- df[,-(1:4)]
df <- df[,-(2:4)]
df <- df[,-4]

 ## use the dcast function to restructure the data
library(reshape2)
c <- dcast(df, site ~ hour)

 ##restructure and clean up the data some more
tc <- t(c)
tc <- data.frame(tc)
colnames(tc) <- as.character(tc[1,])
tc <- tc[-1,]
tc$hour <- c(1:23)

 ## melt the data so we can graph is much easier with ggplot
melt <- melt(tc, id = "hour")

 ## graph the data with ggplot
library(ggplot2)
meltp <- ggplot(melt, aes(x = hour, y = value, colour = variable))
meltp + geom_line(size = .7)
