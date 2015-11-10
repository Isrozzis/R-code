 ## compilation of all other regressions I've done

co <- glm(event ~ CO_center, family=binomial, data=air)
summary(co)

o3 <- glm(event ~ O3_center, family=binomial, data=air)
summary(o3)

pm <- glm(event ~ PM_center, family=binomial, data=air)
summary(pm)

no <- glm(event ~ NO_center, family=binomial, data=air)
summary(no)

no2 <- glm(event ~ NO2_center, family=binomial, data=air)
summary(no2)

so2 <- glm(event ~ SO2_center, family=binomial, data=air)
summary(so2)

 ## co and so2 don't make the cut


 ## multi pollutant models

library(lmtest)
library(car)
for_paper <- glm(event ~ three_day_PM + five_day_NO2 + five_day_O3 + five_day_SO2 + five_day_O3 * five_day_NO2 + five_day_SO2 * five_day_NO2, family = binomial, data = air)
summary(for_paper)
lrtest(for_paper)

for_center <- glm(event ~ PM_center + O3_center + NO2_center + SO2_center + NO2_center * SO2_center + NO2_center * O3_center, family=binomial, data = air)
summary(for_center)
lrtest(for_center)
vif(for_center)

for_center1 <- glm(event ~ PM_center + O3_center + NO2_center + O3_center * NO2_center, family = binomial, data=air)
summary(for_center1)
vif(for_center1)

for_center2 <- glm(event ~ PM_center + O3_center + NO2_center + CO_center + O3_center * NO2_center, family = binomial, data=air)
summary(for_center2)
vif(for_center)

for_center3 <- glm(event ~ PM_center + NO2_center + SO2_center+ NO2_center * SO2_center, family=binomial, data = air)
summary(for_center3)

for_center4 <- glm(event ~ PM_center + O3_center + NO2_center + NO_center + O3_center * NO2_center, family = binomial, data=air)
summary(for_center4)
vif(for_center4)

for_center5 <- glm(event ~ PM_center + NO2_center, family = binomial, data= air)
summary(for_center5)


## OR and CI table

PMOR <- exp(pm$coefficients[2] * 16)
O3OR <- exp(o3$coefficients[2] * 34)
NO2OR <- exp(no2$coefficients[2] * 26.5)
NOOr <- exp(no$coefficients[2] * 54.79)

ORtable <- rbind(PMOR, O3OR, NO2OR, NOOr)
ORtable


cpm <- confint.default(pm) * 16
co3 <- confint.default(o3 * 34)
cno2 <- confint.default(no2 * 26.5)
cno <- confint.default(no * 54.79)

exp(cpm[2,] * 16)

ctable <- data.frame(cpm[2,], co3[2,], cno2[2,], cno[2,])
results <- cbind(ORtable, g)

g <- t(ctable)
g[1,] <- exp(g[1,] * 16)
g[2,] <- exp(g[2,] * 34)
g[3,] <- exp(g[3,] * 26.5)
g[4,] <- exp(g[4,] * 54.79)
rownames(results) <- c("PM", "O3", "NO2", "NO")
colnames(results) <- c("OR", "2.5%", "97.5%")

PMOR1 <- exp(0.0106825 * 16)
O3OR1 <- exp(0.0021984 * 34)
NO2OR1 <- exp(0.0059232 * 26.5)
SO2OR1 <- exp(-0.0026843 * 9.653)
ORtable1 <- rbind(PMOR1, O3OR1, NO2OR1, SO2OR1)

confint <- confint.default(for_center, 2:5)
confint[1,] <- exp(confint[1,] * 16)
confint[2,] <- exp(confint[2,] * 34)
confint[3,] <- exp(confint[3,] * 26.5)
confint[4,] <- exp(confint[4,] * 9.653)
confint[5,] <- exp(confint[5,] * 1244.4)

confint
 asdf <- cbind(ORtable1, confint)
rownames(asdf) <- c("PM", "O3", "NO2", "SO2")
colnames(asdf) <- c("OR", "2.5%", "97.5%")
## interaction effect
library(DAMisc)

i11 <- glm(event ~ O3_center + NO2_center + O3_center * NO2_center, family=binomial, data=air)
int <- intEff(i11, c("O3_center", "NO2_center"), air)
plot(int$phat, int$int_eff, xlab="Predicted Value", ylab="Coeffecient value", main="O3*NO2 Interaction effect coeffecient for predicted value")
ag <- aggregate(int$linear, list(int$phat), mean)
lines(ag[,1], ag[,2], lty=2, col="red", lwd=2)

plot(int$phat, int$zstat, xlab="Predicted Value", ylab="z stat", main="Significance of O3*No2 Interaction effect")
mean(int$zstat)

i22 <- glm(event ~ O3_center + CO_center + O3_center * CO_center, family=binomial, data=air)
int2 <- intEff(i22, c("O3_center", "CO_center"), air)
plot(int2$phat, int2$int_eff, xlab="Predicted value", ylab="Coefficient value", main="O3*CO2 Interaction effect coefficient for predicted value")
ag <- aggregate(int2$linear, list(int2$phat), mean)
lines(ag[,1], ag[,2], lty=2, col="red", lwd=2)
plot(int2$phat, int2$zstat)
mean(int2$zstat)

i33 <- glm(event ~ SO2_center + NO2_center + SO2_center * NO2_center, family=binomial, data = air)
int3 <- intEff(i33, c("SO2_center", "NO2_center"), air)
plot(int3$phat, int3$int_eff, xlab="Predicted value", ylab="Coefficient value", main="SO2*NO2 Interaction effect coefficient for predicted value")
ag <- aggregate(int3$linear, list(int3$phat), mean)
lines(ag[,1], ag[,2], lty=2, col="red", lwd=2)
plot(int3$phat, int3$zstat, xlab="Predicted Value", ylab="z stat", main="Significance of SO2*No2 Interaction effect")
mean(int3$zstat)
