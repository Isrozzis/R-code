 ## trying to map the path of the mobile app data 

mobile <- read.csv("C:/Users/Trevor/Documents/Air/New folder/UH_Mobile_Lab_DISCOVER_r1_20140228.csv")

 ## Convert the date column to the date object in R

new_mobile <- cbind(mobile$start_60sec_CST, mobile$GPS_Lat_60s_avg, mobile$GPS_Lon_60s_avg)
new_mobile <- data.frame(new_mobile)
library(readr)
library(dplyr)
library(lubridate)

class(new_mobile$X1)

## new_mobile$X1 <- as.Date(new_mobile$X1, origin = "1970-01-01")
## head(new_mobile$X1)

mobile$start_60sec_CST <- as.character(mobile$start_60sec_CST)
mobile$date <- strptime(mobile$start_60sec_CST,"%m/%d/%y %H:%M")
head(mobile$date)

class(mobile$start_60sec_CST)
head(mobile$start_60sec_CST)

day1 <- new_mobile[1:826,]
day1 <- data.frame(day1)

library(ggplot2)
map <- ggplot(day1, aes(x = X2, y = X3, colour = "red"))
map + geom_point()

library(ggmap)
loc <- get_googlemap("houston")

h_map <- ggmap(loc, extent = "satelite", zoom = 14)
h_map + geom_point(data = o3, aes(x = longitude, y = latitude), size = 4) + geom_point(data=mobile, aes(x = GPS_Lon_60s_avg, y = GPS_Lat_60s_avg ), colour = "red")


 ## read in the file that has the locations of the ozone monitors

asdf <- read.csv("C:/Users/Trevor/Documents/Air/houston_poll_noNOx_PMcode88101.csv")
o3 <- asdf[asdf$poll_code == 44201, ]
