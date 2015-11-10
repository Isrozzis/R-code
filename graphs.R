#basic graphs for paper

library(ggplot2)

boxplot <- ggplot(air, aes(y=val0_O3, x=race)) + geom_boxplot()
boxplot

CO2 <- air[, 7:15]
CO2 <- CO2[, -c(2,4,6,8)]
str(CO2)

CO2$value <- range(CO2$val0_CO)

COplot <- ggplot(CO2, aes(y=value, x=factor(val0_CO))) + geom_boxplot()
COplot


boxplot(CO2)
boxplot(CO2[,-6])

CO2$five_day_CO <- air$five_day_CO


a <- poll[poll$poll_code == 42101 & poll$site == 403,]
summary(a)
summary(a$longitude)

b <- poll[poll$poll_code == 42101,]


boxplot1 <- ggplot(b, aes(factor(site), value))
boxplot1 + geom_boxplot()

c <- subset(poll, poll_code==42401)

boxplot_c <- ggplot(c, aes(factor(site), value))
boxplot_c + stat_boxplot(geom ='errorbar') + geom_boxplot()

d <- poll[poll$poll_code == 44201,]

boxplot_d <- ggplot(d, aes(factor(site), value))
boxplot_d + stat_boxplot(geom ='errorbar') + geom_boxplot() + ggtitle("Boxplots of all sites for Ozone") + xlab("Sites") + ylab("O3 levels")

e <- d[d$site == 96,]
summary(e)

asdf <- ggplot(e, aes(factor(hour), value))
asdf + stat_boxplot(geom ='errorbar') + geom_boxplot() + ylab("O3 levels")  + xlab("Hours") + ggtitle("Site 96 Hourly Peak Boxplots")

f <- ggplot(d, aes(factor(hour), value))
f + stat_boxplot(geom ='errorbar') + geom_boxplot() + xlab("Hours") + ylab("O3 levels") + ggtitle("Boxplots of Peak values by hour for all sites for Ozone")

str(d$site)
g <- ggplot(a, aes(factor(hour), value))
g + stat_boxplot(geom ='errorbar') + geom_boxplot()

h <- poll[poll$poll_code == 44201 & poll$site == 619,]

i <- ggplot(h, aes(factor(hour), value))
i + stat_boxplot(geom ='errorbar') + geom_boxplot() + xlab("Hours") + ggtitle("Site 619 Hourly Peak Boxplots") + ylab("O3 levels")

gg <- d[d$site==403,]

gg1 <- ggplot(gg, aes(factor(hour), value))
gg1 + stat_boxplot(geom = "errorbar") + geom_boxplot()



time_series <- 