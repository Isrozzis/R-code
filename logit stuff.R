#Start of air pollution work

air <- read.csv("C:/Users/Trevor/Documents/Air/001subjects_poll_merged_fixed.csv")

#understanding how the data I'm working with is coded is impotant. Each pollutant is recorded at the peak time during that day
#and the hour at which it was recorded is also listed. There is a lag time of 3 days, meaning that val0 is the day the event occured
#val1 would be 1 day before, and so on. Hour is the hour of the day that value for the corresponding pollutant was recorded. 

airSO2 <- glm(event ~ val0_SO2 + val1_SO2 + val2_SO2, family=binomial, data=air)
#summary yields z values of -.245, .270, .980 respectively

airCO <- glm(event ~ val0_CO + val1_CO + val2_CO, family=binomial, data=air)
#summary yields z values of -.207, 2.005 (pr(>|z|) = .0449), 1.435 respectively

airNO <- glm(event ~ val0_NO + val1_NO + val2_NO, family=binomial, data=air)
#summary yields z values of .722, .283, 1.329 repsectively 

airNO2 <- glm(event ~ val0_NO2 + val1_NO2 + val2_NO2, family=binomial, data=air)
#summary yields z values of 1.337, 1.208, 2.058 (pr(>|z|) = .0396) respectively

airO3 <- glm(event ~ val0_O3 + val1_O3 + val2_O3, family=binomial, data=air)
#summary yields z values of 1.024, .454, 1.750 respectively

airPM <- glm(event ~ val0_PM + val1_PM + val2_PM, family=binomial, data=air)
#summary yields z vales of 2.227 (pr(>|z|) = .02597), -.614, 2.677 (pr(>|z|) = .00743)

