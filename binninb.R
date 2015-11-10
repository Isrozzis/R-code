#binning


n <- 1
q <- 1
upper_bin <- c()
lower_bin <- c()
for(i in 1:length(air$five_day_SO2)){
  if(air$five_day_SO2[i] >= 7.4330 ){
    upper_bin[n] <- air$five_day_SO2[i]
    n <- n + 1
  }
  else{
    lower_bin[q] <- air$five_day_SO2[i]
    q <- q + 1
  }
}

summary(upper_bin)
summary(lower_bin)

summary(air$SO2_NO2)



m <- 1
r <- 1
upper_bin1 <- c()
lower_bin1 <- c()
for(i in 1:length(air$O3_NO2)){
  if(air$O3_NO2[i] >= 1448 ){
    upper_bin1[m] <- air$O3_NO2[i]
    m <- m + 1
  }
  else{
    lower_bin1[r] <- air$O3_NO2[i]
    r <- r + 1
  }
}

summary(upper_bin1)
summary(lower_bin1)


m1 <- glm(event ~ PM_center + O3_center + NO2_center + SO2_center + NO2_O3_center + SO2_NO2_center, family=binomial, data=air)
summary(m1)
m <- data.frame(air$event, air$PM_center, air$O3_center, air$NO2_center, air$SO2_center, air$NO2_O3_center, air$SO2_NO2_center)
cor(m)
m2 <- glm(event ~ PM_center + O3_center + NO2_center + NO2_O3_center + SO2_NO2_center, family=binomial, data=air)
summary(m2)
m3 <- glm(event ~ PM_center + NO2_center + NO2_O3_center + SO2_NO2_center, family=binomial, data=air)
summary(m3)
cor(m)

m4 <- glm(event ~ PM_center + O3_center + NO2_center + SO2_center + CO_center + NO2_O3_center + SO2_NO2_center, family=binomial, data=air)
summary(m4)
pm4 <- predict(m4, type="response")
summary(pm4)

roc.plot(air$event, pm4, binormal=TRUE, plot="both")

m5 <- glm(event ~ PM_center + NO2_center + SO2_center + CO_center + O3_center, family=binomial, data=air)
summary(m5)
m6 <- glm(event ~ three_day_PM + five_day_NO2 + five_day_SO2 + five_day_CO + five_day_O3, family=binomial, data=air)
summary(m6)


lrtest(m93)
library(lmtest)
lrtest(m2)
lrtest(m2, m93)

#the centered model is regretfully different than the non centered model

#at this point I should start my write up to send to Giulia. I think I have enough information to present to the statistician. 
#hopefully I didn't make any terrible mistakes. 
