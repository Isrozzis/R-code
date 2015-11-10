n1 <- glm(event ~ five_day_CO + five_day_NO + five_day_NO2 + five_day_SO2 + five_day_O3 + five_day_PM, family=binomial, data=air)
summary(n1)

n2 <- glm(event ~ five_day_CO + five_day_NO2 + five_day_SO2 + five_day_O3 + five_day_PM + SO2_CO + SO2_NO2, family=binomial, data=air)
summary(n2)

n3 <- glm(event ~ five_day_NO2 + five_day_SO2 + five_day_O3 + five_day_PM + SO2_NO2, family=binomial, data=air)
summary(n3)

O3_SO2 <- air$five_day_O3 * air$five_day_SO2 
n4 <- glm(event ~ five_day_O3 + five_day_SO2, family=binomial, data=air)
summary(n4)
n5 <- glm(event ~ five_day_O3 + five_day_SO2 + O3_SO2, family=binomial, data=air)
summary(n5)

O3_CO <- air$five_day_CO * air$five_day_O3
n6 <- glm(event ~ five_day_O3 + five_day_CO, family=binomial, data=air)
summary(n6)
n7 <- glm(event ~ five_day_O3 + five_day_CO + O3_CO, family=binomial, data=air)
summary(n7)
lrtest(n7, n6)

n8 <- glm(event ~ three_day_PM + five_day_NO2 + five_day_O3 + five_day_SO2 + SO2_NO2 + O3_NO2, family=binomial, data=air)
summary(n8)

n9 <- glm(event ~ five_day_NO2 + five_day_SO2 + five_day_O3 + five_day_PM + SO2_NO2 + O3_CO, family=binomial, data=air)
summary(n9)

n10 <- glm(event ~ five_day_CO + five_day_NO2 + five_day_SO2 + five_day_O3 + five_day_PM + SO2_NO2 + O3_CO, family=binomial, data=air)
summary(n10)

n11 <- glm(event ~ three_day_PM + five_day_NO2 + five_day_O3 + five_day_SO2 + SO2_NO2 + O3_NO2 + O3_CO, family=binomial, data=air)
summary(n11)

n12 <- glm(event ~ three_day_PM + five_day_CO + five_day_NO2 + five_day_O3 + five_day_SO2 + SO2_NO2 + O3_NO2 + O3_CO, family=binomial, data=air)
summary(n12)

n13 <- glm(event ~ three_day_PM + five_day_NO2 + five_day_O3 + SO2_NO2 + O3_NO2, family=binomial, data=air)
summary(n13)

CO_NO2 <- air$five_day_CO * air$five_day_NO2
n14 <- glm(event ~ five_day_CO + five_day_NO2, family=binomial, data=air)
summary(n14)
n15 <- glm(event ~ five_day_CO + five_day_NO2 + CO_NO2, family=binomial, data=air)
summary(n15)

n16 <- glm(event ~ three_day_PM + five_day_NO2 + five_day_O3 + O3_NO2 + SO2_NO2 + gender, family=binomial, data=air)
summary(n16)
air$female <- air$gender

n17 <- glm(event ~ three_day_PM + five_day_NO2 + five_day_O3 + O3_NO2 + SO2_NO2 + female, family=binomial, data=air)
summary(n17)

air$race <- as.factor(air$race)
class(air$race)

n18 <- glm(event ~ three_day_PM + five_day_NO2 + five_day_O3 + O3_NO2 + SO2_NO2 + race, family=binomial, data=air)
summary(n18)

air$age <- as.factor(air$age)
n19 <- glm(event ~ three_day_PM + five_day_NO2 + five_day_O3 + O3_NO2 + SO2_NO2 + age, family=binomial, data=air)
summary(n19)

air$insured <- as.factor(air$insured)
n20 <- glm(event ~ three_day_PM + five_day_NO2 + five_day_O3 + O3_NO2 + SO2_NO2 + insured, family=binomial, data=air)
summary(n20)

#stratifying the data didn't seem to do anything. This is sort of surprising. Does this mean that cross-class classiciation worked?
#i'm not really sure. Maybe it did, maybe it didn't. It could also mean that subsetting the data further makes it much harder to work with

a <- summary(n13)
exp(a$coefficients)

n21 <- glm(event ~ three_day_PM + five_day_NO2 + five_day_O3 + SO2_NO2 + O3_NO2 + severity, family=binomial, data=air)
summary(n21)

air$severity <- as.factor(air$severity)

#bins
#CO: 1,500 420
#SO2: 20 2.6
#NO: 114 12.5
#NO2: 46 19.5
#O3: 70 36
#PM: 32 16

a <- summary(n13)
b <- exp(.0105667 * 16)
c <- exp(.0244255 * 26.5)
d <- exp(.0110827 * 34)
e <- exp(-.0003099 * 337.97)
f <- exp(-.0003550 * 1643)

b
c
d
e
f

a <- rbind(b,c,d,e,f)
rownames(a) <- c("PM", "NO2", "O3", "SO2 & NO2", "O3 & No2")
a
