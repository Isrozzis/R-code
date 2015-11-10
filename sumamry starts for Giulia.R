#make boxplots of houston_poll thing

poll <- read.csv("C:/Users/Trevor/Documents/Air/houston_poll_noNOx_PMcode88101.csv")

boxplot1 <- ggplot(poll, aes(factor(hour), value))
boxplot1 + geom_jitter() + geom_boxplot()

boxplot2 <- ggplot(poll, aes(factor(site), value))
boxplot2 + geom_jitter() + geom_boxplot()


boxplot3 <- ggplot(poll, aes(factor(poll_code), value)) 
boxplot3 + geom_jitter() + geom_boxplot()

for (i in 1:nrow(poll)){
  if (poll$poll_code[i] == 42101){
    poll$poll_code[i] <- "CO"
  }
  if (poll$poll_code[i] == 42401){
    poll$poll_code[i] <- "SO2"
  }
  if (poll$poll_code[i] == 42601){
    poll$poll_code[i] <- "NO"
  }
  if (poll$poll_code[i] == 42602){
    poll$poll_code[i] <- "NO2"
  }
  if (poll$poll_code[i] == 44201){
    poll$poll_code[i] <- "O3"
  }
  if (poll$poll_code[i] == 88101){
    poll$poll_code[i] <- "PM"
  }
}

CO_poll <- data.frame()
CO_poll <- poll[1:19033,]

SO2_poll <- data.frame()
SO2_poll <- poll[19034:43063,]

NO_poll <- data.frame()
NO_poll <- poll[43064:105234,]

NO2_poll <- data.frame()
NO2_poll <- poll[105234:174082,]

O3_poll <- data.frame()
O3_poll <- poll[174082:306696,]

PM_poll <- data.frame()
PM_poll <- poll[306696:326528,]


COplot <- ggplot(CO_poll, aes(factor(hour), value)) 
COplot + geom_jitter() + geom_violin()

summary(CO_poll$value)
no_COvalue <- ggplot(no_CO, aes(factor(poll_code), value)) 
no_COvalue + geom_jitter(size = .01) + stat_boxplot(geom ='errorbar') + geom_boxplot(notch=FALSE, outlier.shape=NA, fill="red", alpha=1)

no_CO <- rbind(SO2_poll, NO_poll, NO2_poll, O3_poll, PM_poll)
summary(no_CO)

COvalue <- ggplot(CO_poll, aes(factor(0), value), xlab="") 
COvalue + stat_boxplot(geom ='errorbar') + geom_boxplot(notch=FALSE, outlier.shape=NA, fill="red", alpha=0.1)

COvalue

poll$date <- as.POSIXct(poll$epoch)
str(poll$date)

epochplot <- ggplot(CO_poll, aes(epoch, value)) 
epochplot + geom_line() + stat_smooth(aes(group = 1)) 

summary(SO2_poll$value)
summary(NO_poll$value)
summary(NO2_poll$value)
summary(O3_poll$value)
summary(PM_poll$value)


summary(female_data$five_day_PM)
summary(male_data$five_day_PM)

summary(female_data$five_day_O3)
summary(male_data$five_day_O3)

summary(female_data$five_day_NO2)
summary(male_data$five_day_NO2)

summary(female_data$five_day_NO)
summary(male_data$five_day_NO)

summary(female_data$five_day_SO2)
summary(male_data$five_day_SO2)


 ## CO

summary(female_data$five_day_CO)
summary(male_data$five_day_CO)
summary(white_data$five_day_CO)
summary(black_data$five_day_CO)
summary(hispanic_data$five_day_CO)
summary(other_data$five_day_CO)
summary(white_data$five_day_CO)
summary(insured_data$five_day_CO)
summary(uninsured_data$five_day_CO)
summary(one_4_data$five_day_CO)
summary(five_14_data$five_day_CO)
summary(fifteen_18_data$five_day_CO)
summary(mild_i_data$five_day_CO)
summary(mild_p_data$five_day_CO)
summary(moderate_data$five_day_CO)
summary(severe_data$five_day_CO)

 ## PM

summary(female_data$five_day_PM)
summary(male_data$five_day_PM)
summary(white_data$five_day_PM)
summary(black_data$five_day_PM)
summary(hispanic_data$five_day_PM)
summary(other_data$five_day_PM)
summary(white_data$five_day_PM)
summary(insured_data$five_day_PM)
summary(uninsured_data$five_day_PM)
summary(one_4_data$five_day_PM)
summary(five_14_data$five_day_PM)
summary(fifteen_18_data$five_day_PM)
summary(mild_i_data$five_day_PM)
summary(mild_p_data$five_day_PM)
summary(moderate_data$five_day_PM)
summary(severe_data$five_day_PM)

 ## Male/Female t test
t.test(female_data$five_day_CO, male_data$five_day_CO)
t.test(female_data$five_day_O3, male_data$five_day_O3)
t.test(female_data$five_day_NO, male_data$five_day_NO)
t.test(female_data$five_day_NO2, male_data$five_day_NO2)
t.test(female_data$five_day_SO2, male_data$five_day_SO2)
t.test(female_data$five_day_PM, male_data$five_day_PM)

wilcox.test(female_data$five_day_CO, male_data$five_day_CO)
wilcox.test(female_data$five_day_O3, male_data$five_day_O3)
wilcox.test(female_data$five_day_NO, male_data$five_day_NO)
wilcox.test(female_data$five_day_NO2, male_data$five_day_NO2)
wilcox.test(female_data$five_day_SO2, male_data$five_day_SO2)
wilcox.test(female_data$five_day_PM, male_data$five_day_PM)

air$race <- as.factor(air$race)
race_aov_CO <- aov(five_day_CO ~ race, data=air)
summary(race_aov_CO)
TukeyHSD(race_aov_CO)

race_aov_O3 <- aov(five_day_O3 ~ race, data=air)
summary(race_aov_O3)
TukeyHSD(race_aov_O3)

race_aov_PM <- aov(five_day_PM ~ race, data=air)
summary(race_aov_PM)
TukeyHSD(race_aov_PM)

race_aov_NO <- aov(five_day_NO ~ race, data=air)
summary(race_aov_NO)
TukeyHSD(race_aov_NO)

race_aov_NO2 <- aov(five_day_NO2 ~ race, data=air)
summary(race_aov_NO2)
TukeyHSD(race_aov_NO2)

race_aov_SO2 <- aov(five_day_SO2 ~ race, data=air)
summary(race_aov_SO2)
TukeyHSD(race_aov_SO2)


t.test(insured_data$five_day_CO, uninsured_data$five_day_CO)
t.test(insured_data$five_day_O3, uninsured_data$five_day_O3)
t.test(insured_data$five_day_NO, uninsured_data$five_day_NO)
t.test(insured_data$five_day_NO2, uninsured_data$five_day_NO2)
t.test(insured_data$five_day_SO2, uninsured_data$five_day_SO2)
t.test(insured_data$five_day_PM, uninsured_data$five_day_PM)

air$age <- as.factor(air$age)
age_aov_CO <- aov(five_day_CO ~ age, data=air)
summary(age_aov_CO)
TukeyHSD(age_aov_CO)

age_aov_O3 <- aov(five_day_O3 ~ age, data=air)
summary(age_aov_O3)
TukeyHSD(age_aov_O3)

age_aov_PM <- aov(five_day_PM ~ age, data=air)
summary(age_aov_PM)
TukeyHSD(age_aov_PM)

age_aov_NO <- aov(five_day_NO ~ age, data=air)
summary(age_aov_NO)
TukeyHSD(age_aov_NO)

age_aov_NO2 <- aov(five_day_NO2 ~ age, data=air)
summary(age_aov_NO2)
TukeyHSD(age_aov_NO2)

age_aov_SO2 <- aov(five_day_SO2 ~ age, data=air)
summary(age_aov_SO2)
TukeyHSD(age_aov_SO2)

plot(age_aov_SO2)

