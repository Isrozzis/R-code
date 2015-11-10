 ## Oh man there was a package someone already made to do this

install.packages("DAMdisc")
library(DAMisc)

mi <- glm(event ~ three_day_PM + five_day_NO2 + five_day_SO2 + five_day_O3 + five_day_NO2 * five_day_O3 + five_day_SO2 * five_day_NO2, family=binomial, data=air)

int <- intEff(mi, c("five_day_O3", "five_day_NO2"), air)

summary(int)
plot(int$phat, int$int_eff)
ag <- aggregate(int$linear, list(int$phat), mean)
lines(ag[,1], ag[,2], lty=2, col="red", lwd=2)

plot(int$phat, int$zstat)
mean(int$zstat)

# a z score of 1.96 accounts for 5% significance level abs(mean(int$zscore)) 
# is > than 1.96, so we can conclude that the interaction effect is significant

int_SO2 <- intEff(mi, c("five_day_SO2", "five_day_NO2"), air)
plot(int_SO2$phat, int_SO2$int_eff)
ag <- aggregate(int_SO2$linear, list(int_SO2$phat), mean)
lines(ag[,1], ag[,2], lty=2, col="red", lwd=2)

plot(int_SO2$phat, int_SO2$zstat)
abs(mean(int_SO2$zstat))

# the z score we get here is 2.418, so it is significant. 