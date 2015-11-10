results <- data.frame()
results <- rbind(c(1.019, 2.796, 2.358), c(.017,.479,1.267),c(1.052,.922,1.533),c(3.314,3.863,3.965),c(2.928,3.298,3.526),c(2.384,.833,2.8230))
colnames(results) <- c("Event day","1 day before event", "2 days before event")
rownames(results) <- c("CO","SO2","NO","NO2","NOx","PM")

ORtable <- data.frame()
ORtable <- rbind(exp(c(0.00001547, 0.00004201, 0.00003608)), exp(c(0.00001564, 0.0004423, 0.001188)), exp(c(0.0001605, 0.0001432, 0.0002398)), exp(c(0.0021792, 0.0025337, 0.0026198)), exp(c(0.001245, 0.0014011, 0.001492)), exp(c(0.004258, 0.001529, 0.001828)))
rownames(ORtable) <- c("CO","SO2","NO","NO2","O3","PM")
colnames(ORtable) <- c("Event day","1 day before event", "2 days before event")

maybe <- glm(event ~ val1_CO + val2_CO + val0_NO2 + val1_NO2 + val2_NO2 + val0_O3 + val1_O3 + val2_O3 + val0_PM + val2_PM, family=binomial, data=air)
summary(maybe)

maybe1 <- maybe <- glm(event ~ val1_CO + val2_CO + val0_NO2 + val1_NO2 + val2_NO2 + val0_O3 + val1_O3 + val2_O3, family=binomial, data=air)
summary(maybe1)

