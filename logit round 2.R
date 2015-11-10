#working on trying to find interactions between variables
#I suspect that there is some sort of interaction between SO2 and other variables in the 5 day averages model

m1 <- glm(event ~ five_day_SO2 + five_day_O3, family=binomial, data=air)
summary(m1)

air$SO2_O3 <- air$five_day_SO2 * air$five_day_O3

m2 <- glm(event ~ five_day_SO2 + five_day_O3 + SO2_O3, family=binomial, data=air)
summary(m2)

lrtest(m2, m1)
lrtest(m1)
lrtest(m2)


m3 <- glm(event ~ five_day_SO2 + five_day_CO, family=binomial, data=air)
summary(m3)

air$SO2_CO <- air$five_day_SO2 * air$five_day_CO

m4 <- glm(event ~ five_day_SO2 + five_day_CO + SO2_CO, family=binomial, data=air)
summary(m4)

lrtest(m4)
lrtest(m3)
lrtest(m4,m3)

#ah hell ya I actually found something. There appears to be an interaction between SO2 and CO
#I'm not really sure what it means, but there is something going on. 

m5 <- glm(event ~ five_day_CO + SO2_CO, data=air, family=binomial)
summary(m5)
lrtest(m5)
lrtest(m5, m4)

#the model isn't significantly different than the model with all 3 of the variables, but I think I might be on to something. 

m6 <- glm(event ~ five_day_SO2 + five_day_NO, data=air, family=binomial)
summary(m6)
lrtest(m6)

air$SO2_NO <- air$five_day_SO2 * air$five_day_NO

m7 <- glm(event ~ five_day_SO2 + five_day_NO + SO2_NO, data=air, family=binomial)
summary(m7)
lrtest(m7)
lrtest(m6, m7)

#there doesn't seem to be any sort of interaction here. 

m8 <- glm(event ~ five_day_SO2 + five_day_NO2, family=binomial, data=air)
summary(m8)
lrtest(m8)

air$SO2_NO2 <- air$five_day_NO2 * air$five_day_SO2
m9 <- glm(event ~ five_day_SO2 + five_day_NO2 + SO2_NO2, family=binomial, data=air)
summary(m9)
lrtest(m9)

#there seems to be an interaction here. 

m10 <- glm(event ~ five_day_NO2 + SO2_NO2, data=air, family=binomial)
summary(m10)
lrtest(m10)
lrtest(m10, m9)

#it is almost significantly different than the previous model at 5%. I might actually be on to something. 

m11 <- glm(event ~ five_day_CO + five_day_NO2 + SO2_CO + SO2_NO2, family=binomial, data=air)
summary(m11)

#the combination of bits of previous models does not seem to yield anything particularly useful
#fkasfjasl;kfjasdlfjasd;flkjf
#oh well, time to keep going

m12 <- glm(event ~ five_day_SO2 + five_day_PM, family=binomial, data=air)
summary(m12)
lrtest(m12)

air$SO2_PM <- air$five_day_SO2 * air$five_day_PM

m13 <- glm(event ~ five_day_SO2 + five_day_PM + SO2_PM, family=binomial, data=air)
summary(m13)
#Pr(<|z|) = .09886 for interaction coeff. There is no interaction here, which makes sense. 

m14 <- glm(event ~ five_day_PM + five_day_O3, family=binomial, data=air)
summary(m14)

air$PM_O3 <- air$five_day_PM * air$five_day_O3

m15 <- glm(event ~ five_day_PM + five_day_O3 + PM_O3, family=binomial, data=air)
summary(m15)

lrtest(m15)
lrtest(m14)
lrtest(m14, m15)

#there does not seem to be any sort of interaction here. 

m16 <- glm(event ~ five_day_PM + five_day_NO2, family=binomial, data=air)
summary(m16)
lrtest(m16)

air$PM_NO2 <- air$five_day_PM * air$five_day_NO2

m17 <- glm(event ~ five_day_PM + five_day_NO2 + PM_NO2, family=binomial, data=air)
summary(m17)
lrtest(m17)
lrtest(m16,m17)

m18 <- glm(event ~ val2_CO + five_day_PM + five_day_O3 + SO2_CO, family=binomial, data=air)
summary(m18)

m19 <- glm(event ~ val2_CO + five_day_PM + five_day_O3, family=binomial, data=air)
summary(m19)
m20 <- glm(event ~ val2_CO, family=binomial, data=air)
summary(m20)
m21 <- glm(event ~ three_day_CO, family=binomial, data=air)
summary(m21)
m22 <- glm(event ~ five_day_CO, family=binomial, data=air)
summary(m22)
m23 <- glm(event ~ five_day_PM + val0_CO, family=binomial, data=air)
summary(m23)
m24 <- glm(event ~ five_day_PM + val1_CO, family=binomial, data=air)
summary(m24)
m25 <- glm(event ~ five_day_PM + val2_CO, family=binomial, data=air)
summary(m25)
m26 <- glm(event ~ five_day_PM + val3_CO, family=binomial, data=air)
summary(m26)
m27 <- glm(event ~ five_day_PM + val4_CO, family=binomial, data=air)
summary(m27)
m28 <- glm(event ~ five_day_PM + five_day_CO, family=binomial, data=air)
summary(m28)
m29 <- glm(event ~ five_day_PM + three_day_CO, family=binomial, data=air)
summary(m29)
#CO doesn't seem to be significant?

air$PM_CO <- air$five_day_CO * air$five_day_PM
m30 <- glm(event ~ five_day_PM + five_day_CO + PM_CO, family=binomial, data=air)
summary(m30)
#doesn't seem to be any sort of interaction here. 

m31 <- glm(event ~ five_day_O3, family=binomial, data=air)
summary(m31)
m32 <- glm(event ~ three_day_O3, family=binomial, data=air)
summary(m32)
m33 <- glm(event ~ five_day_PM + five_day_O3, family=binomial, data=air)
summary(m33)
m34 <- glm(event ~ five_day_PM + five_day_NO2, family=binomial, data=air)
summary(m34)
m35 <- glm(event ~ five_day_PM + five_day_SO2, family=binomial, data=air)
summary(m35)
predict.prob(m34)

m36 <- glm(event ~ five_day_PM + five_day_NO2 + SO2_NO2, family=binomial, data=air)
summary(m36)
predict.prob(m36)

m37 <- glm(event ~ five_day_PM + five_day_NO2 + five_day_SO2 + SO2_NO2, family=binomial, data=air)
summary(m37)
predict.prob(m37)

lrtest(m35)
lrtest(m36)
lrtest(m37)
lrtest(m36, m35)
lrtest(m37, m36)

m38 <- glm(event ~ five_day_PM + five_day_NO2 + five_day_O3 + five_day_SO2 + SO2_NO2, family=binomial, data=air)
summary(m38)

#ozone section
m39 <- glm(event ~ val0_O3, family=binomial, data=air)
summary(m39)
#2%
m40 <- glm(event ~ val1_O3, family=binomial, data=air)
summary(m40)
#7%
m41 <- glm(event ~ val2_O3, family=binomial, data=air)
summary(m41)
m42 <- glm(event ~ val3_O3, family=binomial, data=air)
summary(m42)
m43 <- glm(event ~ val4_O3, family=binomial, data=air)
summary(m43)
#6%

m44 <- glm(event ~ five_day_O3, family=binomial, data=air)
summary(m44)
#1.65%
m45 <- glm(event ~ three_day_O3, family=binomial, data=air)
summary(m45)
#2.22%

m46 <- glm(event ~ five_day_O3 + five_day_PM, family=binomial, data=air)
summary(m46)
air$O3_PM <- air$five_day_O3 * air$five_day_PM
summary(air$O3_PM)
m47 <- glm(event ~ five_day_O3 + five_day_PM + O3_PM, family=binomial, data=air)
summary(m47)
#nothing is significant now. There doesn't seem to be the sort of interaction that we want to test for. 
#perhaps trying individual values or three day values of O3 will work. 

m48 <- glm(event ~ three_day_O3 + five_day_PM, family=binomial, data=air)
summary(m48)
#roughly the same as five day model
m49 <- glm(event ~ val0_O3 + five_day_PM, family=binomial, data=air)
summary(m49)
#15% significance, but not good enough
m50 <- glm(event ~ val1_O3 + five_day_PM, family=binomial, data=air)
summary(m50)
m51 <- glm(event ~ val2_O3 + five_day_PM, family=binomial, data=air)
summary(m51)
m52 <- glm(event ~ val3_O3 + five_day_PM, family=binomial, data=air)
summary(m52)
m53 <- glm(event ~ val4_O3 + five_day_PM, family=binomial, data=air)
summary(m53)


#testing particle matter
m54 <- glm(event ~ five_day_PM, family=binomial, data=air)
summary(m54)
m55 <- glm(event ~ three_day_PM, family=binomial, data=air)
summary(m55)
#three day is more significant, perhaps use that. 
m56 <- glm(event ~ val0_PM, family=binomial, data=air)
summary(m56)
m57 <- glm(event ~ val1_PM, family=binomial, data=air)
summary(m57)
m58 <- glm(event ~ val2_PM, family=binomial, data=air)
summary(m58)
m59 <- glm(event ~ val3_PM, family=binomial, data=air)
summary(m59)
#60%, not at all useful. Sort of surprising too. 
m60 <- glm(event ~ val4_PM, family=binomial, data=air)
summary(m60)

#use three day, or 0 and 2 day. 

#testing previous model with 3 day average of PM instead of 5 day. 
m61 <- glm(event ~ three_day_PM + five_day_NO2 + five_day_O3 + five_day_SO2 + SO2_NO2, family=binomial, data=air)
summary(m61)
#time to refine this model a bit
m62 <- glm(event ~ three_day_PM + five_day_NO2 + val0_O3 + five_day_SO2 + SO2_NO2, family=binomial, data=air)
summary(m62)
lrtest(m61, m62)
m63 <- glm(event ~ three_day_PM + five_day_NO2 + val0_O3 + five_day_SO2 + SO2_NO2 + val2_CO, family=binomial, data=air)
summary(m63)


#testing CO
m64 <- glm(event ~ val0_CO, family=binomial, data=air)
summary(m64)
m66 <- glm(event ~ val1_CO, family=binomial, data=air)
summary(m66)
m67 <- glm(event ~ val2_CO, family=binomial, data=air)
summary(m67)
m68 <- glm(event ~ val3_CO, family=binomial, data=air)
summary(m68)
m69 <- glm(event ~ val4_CO, family=binomial, data=air)
summary(m69)
m70 <- glm(event ~ three_day_CO, family=binomial, data=air)
summary(m70)
m71 <- glm(event ~ five_day_CO, family=binomial, data=air)
summary(m71)

m72 <- glm(event ~ three_day_PM + five_day_NO2 + val0_O3 + five_day_SO2 + SO2_NO2 + five_day_CO, family=binomial, data=air)
summary(m72)

m73 <- glm(event ~ three_day_PM + five_day_NO2 + val0_O3 + five_day_SO2 + SO2_NO2 + two_four_CO, family=binomial, data=air)
summary(m73)

m74 <- glm(event ~ three_day_PM + five_day_NO2 + val0_O3 + five_day_SO2 + SO2_NO2 + val2_CO, family=binomial, data=air)
summary(m74)
#for CO using the average of days 2-4 did make a better model, but it was still not very good. 

m75 <- glm(event ~ three_day_PM + two_four_NO2 + val0_O3 + five_day_SO2 + SO2_NO2, family=binomial, data=air)
summary(m75)
lrtest(m75)
lrtest(m75, m72)

m76 <- glm(event ~ three_day_PM + one_two_fourNO2 + val0_O3 + one_two_fourSO2 + SO2_NO2_4, family=binomial, data=air)
summary(m76)

m77 <- glm(event ~ three_day_PM + one_two_fourNO2 + val0_O3 + five_day_SO2 + SO2_NO2, family=binomial, data=air)
summary(m77)
lrtest(m77, m74)

m78 <- glm(event ~ three_day_PM + one_two_fourNO2 + five_day_SO2 + SO2_NO2, family=binomial, data=air)
summary(m78)

m79 <- glm(event ~ three_day_PM + one_two_fourNO2 + five_day_SO2 + SO2_NO2 + three_day_O3, family=binomial, data=air)
summary(m79)

#time to check something about the difference between five_day_NO2 and one_two_fourNO2

m80 <- glm(event ~ three_day_PM + one_two_fourNO2 + five_day_SO2 + SO2_NO2, family=binomial, data=air)
summary(m80)
lrtest(m80)
m81 <- glm(event ~ three_day_PM + five_day_NO2 + five_day_SO2 + SO2_NO2, family=binomial, data=air)
summary(m81)
lrtest(m81)
lrtest(m81,m80)

# I got a result and I don't know what it means

m82 <- glm(event ~ five_day_O3, family=binomial, data=air)
summary(m82)
m83 <- glm(event ~ three_day_O3, family=binomial, data=air)
summary(m83)
m83 <- glm(event ~ val0_O3, family=binomial, data=air)
summary(m83)
#five_day_o3 works best

m84 <- glm(event ~ five_day_O3 + five_day_NO2, family=binomial, data=air)
summary(m84)

air$O3_NO2 <- air$five_day_O3 * air$five_day_NO2
m4444 <- glm(event ~ five_day_O3 + five_day_NO2, family=binomial, data=air)
summary(m4444)
m85 <- glm(event ~ five_day_O3 + five_day_NO2 + O3_NO2, family=binomial, data=air)
summary(m85)
lrtest(m85)
lrtest(m85, m4444)

#yay a result

m86 <- glm(event ~ three_day_PM + five_day_NO2 + five_day_SO2 + five_day_O3 + SO2_NO2 + O3_NO2, family=binomial, data=air)
summary(m86)
lrtest(m86)
predict.prob(m86)

#testing for interactions between CO and other things. I think it actually has some sort of significance
#the trick is how do I do this? There is evidence that only days 2-4 of CO matter for this study. 
#It would be not exactly right to form interaction varialbes by multiplying CO by the 5 day values of other variables though. 
#Perhaps just multiplying by the last 3 days of other variables and checking? I'm unsure of what this would mean.

#i've already tested NO2_CO and PM_CO, so i'm not going to do those again. 

air$CO_NO <- air$five_day_CO * air$five_day_NO

m87 <- glm(event ~ five_day_CO + five_day_NO, family=binomial, data=air)
summary(m87)
m88 <- glm(event ~ five_day_CO + five_day_NO + CO_NO, family=binomial, data=air)
summary(m88)
#nope

air$CO_SO2 <- air$five_day_CO * air$five_day_SO2

m89 <- glm(event ~ five_day_CO + five_day_SO2, family=binomial, data=air)
summary(m89)
m90 <- glm(event ~ five_day_CO + five_day_SO2 + CO_SO2, family=binomial, data=air)
summary(m90)
lrtest(m89)
lrtest(m90)
lrtest(m90, m89)
#there seems to be an interaction here. Perhaps SO2 has been mostly a confounder in this study and I've been misinterpretting 
#its significance

m91 <- glm(event ~ three_day_PM + five_day_NO2 + five_day_SO2 + five_day_O3 + five_day_CO + SO2_NO2 + O3_NO2 + CO_SO2, family=binomial, data=air)
summary(m91)
m92 <- glm(event ~ three_day_PM + five_day_NO2 + five_day_O3 + five_day_CO + SO2_NO2 + O3_NO2 + CO_SO2, family=binomial, data=air)
summary(m92)
#neither of these models really accounts for what is going on

m93 <- glm(event ~ three_day_PM + five_day_NO2 + five_day_O3 + SO2_NO2 + O3_NO2, family=binomial, data=air)
summary(m93)
library(lrtest(m93)
p93 <- predict(m93, type="response")
#yes yes yes yes yes

for_paper <- glm(event ~ three_day_PM + five_day_NO2 + five_day_O3 + five_day_SO2 + five_day_O3 * five_day_NO2 + five_day_SO2 * five_day_NO2, family = binomial, data = air)
summary(for_paper)
lrtest(for_paper)

for_center <- glm(event ~ PM_center + O3_center + NO2_center + SO2_center + NO2_center * SO2_center + NO2_center * O3_center, family=binomial, data = air)
summary(for_center)
lrtest(for_center)
vif(for_center)

for_center1 <- glm(event ~ PM_center + O3_center + PM_center * O3_center, family=binomial, data=air)
summary(for_center1)

cent <- intEff(for_center1, c("PM_center", "O3_center"), air)

summary(cent)
plot(cent$phat, cent$int_eff)
ag <- aggregate(cent$linear, list(cent$phat), mean)
lines(ag[,1], ag[,2], lty=2, col="red", lwd=2)

plot(cent$phat, cent$zstat)
abs(mean(cent$zstat))

c1 <- glm(event ~ O3_center + NO2_center + NO2_center * O3_center, family=binomial, data=air)
summary(c1)
i1 <- intEff(c1, c("O3_center", "NO2_center"), air)

plot(i1$phat, i1$int_eff)
ag <- aggregate(i1$linear, list(i1$phat), mean)
lines(ag[,1], ag[,2], lty=2, col="red", lwd=2)

plot(i1$phat, i1$zstat)
abs(mean(i1$zstat))

c2 <- glm(event ~ SO2_center + NO2_center + SO2_center * NO2_center, family=binomial, data=air)
summary(c2)
i2 <- intEff(c2, c("SO2_center", "NO2_center"), air)

plot(i2$phat, i2$int_eff)
ag <- aggregate(i2$linear, list(i2$phat), mean)
lines(ag[,1], ag[,2], lty=2, col="red", lwd=2)

plot(i2$phat, i2$zstat)
abs(mean(i2$zstat))

c3 <- glm(event ~ PM_center + O3_center + NO2_center + O3_center * NO2_center, family=binomial, data=air)
summary(c3)

i3 <- intEff(c3, c("NO2_center", "O3_center"), air)
plot(i3$phat, i3$int_eff)
ag <- aggregate(i3$linear, list(i3$phat), mean)
lines(ag[,1], ag[,2], lty=2, col="red", lwd=2)
plot(i3$phat, i3$zstat)


asdf
lrtest(for_paper, m93)

m96 <- glm(event ~ three_day_PM + five_day_NO2 + five_day_O3 + five_day_CO + SO2_NO2 + O3_NO2 + CO_SO2, family=binomial, data=air) 
summary(m96)

lrtest(m93, m86)
#the chisq test only shows significance of 12%, but i'm very happy with this model. Now to muck about with CO more. 

m94 <- glm(event ~ two_four_CO, family=binomial, data=air)
summary(m94)
m95 <- glm(event ~ five_day_CO, family=binomial, data=air)
summary(m95)
#of the two, the second is better, but not great. 



