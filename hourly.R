 ## applying the monolingual nonweighted method to the hourly data 

hourly <- read.csv("C:/Users/Trevor/Documents/Air/1houravg_allsites_0209_7p.csv", header=FALSE)

 ## The intial data set had no colnames. Let's add some to make this easier to understand

names <- c("poll_code", "region", "site", "year", "month", "day", "hour", "value")
colnames(hourly) <- names

 ## The date isn't exactly in a very useable format for us. 

hourly$date <- c()
hourly$date <- paste(hourly$year, hourly$mont, hourly$day, sep="/")
hourly$date <- as.Date(hourly$date)

 ## NA values in value are stored as 1.00e+09. We want to change these to NA so statistical tools actually work

hourly[hourly >= 100000] <- NA

 ## This file is large and takes awhile to clean. I'll create a new file for future use

write.csv(hourly, "C:/Users/Trevor/Documents/Air/cleaned_hourly.csv")

 ## I had to restart the R console since it was eating up way too much RAM. Might as well read in the cleaned version and not do all of that again

cleaned <- read.csv("C:/Users/Trevor/Documents/Air/cleaned_hourly.csv")

 ## This is a large data set, so I want to extract just what I am going to use

hourly_ozone <- cleaned[cleaned$poll_code == 44201,]
write.csv(hourly_ozone, "C:/Users/Trevor/Documents/Air/hourly_ozone.csv")
hourly_O3 <- read.csv("C:/Users/Trevor/Documents/Air/hourly_ozone.csv")

 ## Now I want to make some sort of graphic that shows the hourly variance of the monitors

library(ggplot2)
box <- ggplot(hourly_O3, aes(factor(hour), value))
box + stat_smooth(aes(group = 1)) + geom_jitter() + geom_boxplot() 

 ## Apparently there are negative values for Ozone. This is clearly a mistake. For now I'm going to put them to NA. 

hourly_O3$value[hourly_O3$value < 0] <- NA

date <- ggplot(hourly_O3, aes(factor(date), value))

 ## Now to do it on a by day basis since that is what we want to look at

hourly_O3$date <- as.Date(hourly_O3$date)
jan_1 <- hourly_O3[hourly_O3$date == "2002-01-01",]

 ## Make some boxplots and see what happens

jan1 <- ggplot(jan_1, aes(factor(site), value))
jan1 + geom_jitter() + geom_boxplot()

jan1_1 <- ggplot(jan_1, aes(factor(hour), value))
jan1_1 + geom_jitter() + geom_boxplot()

 ## Neither of these are exactly what we want. I'm going to subset by hour now

jan_1_1 <- jan_1[jan_1$hour == 1, ]
graph <- ggplot(jan_1_1, aes(factor(site), value))
graph + geom_point()

 ## maybe try another day that doesn't have as many missing values

aug_1_1 <- hourly_O3[hourly_O3$date == "2002-07-01" & hourly_O3$hour == 13, ]
aug <- ggplot(aug_1_1, aes(factor(site), value))
aug  + geom_point()

 ## and another day

july_1_15_2008 <- hourly_O3[hourly_O3$date == "2008-07-01" & hourly_O3$hour == 15, ]
jul <- ggplot(july_1_15_2008, aes(factor(site), value))
jul + geom_point()

 ## maybe another way to look at this

july_1_hour_2008 <- hourly_O3[hourly_O3$date == "2008-07-01" & hourly_O3$site == 8,] ##just a randomly chosen site
maybe <- ggplot(july_1_hour_2008, aes(factor(hour), value))
maybe + geom_point()

melt_test_data <- hourly_O3[hourly_O3$date>="2008-08-01" & hourly_O3$date<="2008-08-08", ]

 ## I need to restructure the data to effectively graph it

melt_test_data <- melt_test_data[melt_test_data$date == "2008-08-01", ]
melt_test_data <- melt_test_data[,-(1:4)]
melt_test_data <- melt_test_data[,-(2:4)]
melt_test_data <- melt_test_data[,-4]
casted <- dcast(melt_test_data, site ~ hour)
str(casted)
melted <- melt(casted, id = "site")

castplot <- ggplot(melted, aes(x = factor(site), y = value, colour = variable))
castplot + geom_line()


site_1 <- melt_test_data[melt_test_data$site==1,]
site_1015 <- melt_test_data[melt_test_data$site==1015,]

y <- merge(site_1, site_1015, "hour")
yx <- cbind(y$hour, y$value.x, y$value.y)
yx <- data.frame(yx)
colnames(yx) <- c("hour", "site1", "site1015")

yp <- ggplot(yx, aes(x = hour))
yp + geom_line(aes(y = site1), colour = "red") + geom_line(aes(y = site1015), colour = "blue")

tcast <- t(casted)
tcast <- data.frame(tcast)
colnames(tcast) <- as.character(tcast[1,])
tcast <- tcast[-1,]
head(tcast)
tcast$hour <- c(1:23)


meltround2 <- melt(tcast, id = "hour")
meltround2[meltround2=="site"] <- NA

meltplot2 <- ggplot(meltround2, aes(x = hour, y = value, colour = variable))
meltplot2 + geom_line()
