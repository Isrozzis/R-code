#testing the centered model

lrtest(m2, m93)
#the centered model is unfortunately different from the non centered model. I don't know which model should be interpretted though
#I want to use the non centered model since it is easier to interpret and I've already made the bins for it, but it is not as good of a model
#It has several very high VIFs that indicate that there is multicollinearity going on. 

library(car)
vif(m2)
vif(m93)
vif(for_paper)

#we can see from the VIF that our non centered model has lots of multicollinearity. Research I've done has been inconclusive about how to 
#interpret the higher VIFs. One source says you can safely ignore then because of how the model is constructed, but another source
#says that the presence of high VIFs indicates poor model construction. 

summary(m2)

#regrettably using the centered data has O3 coming up short on the significance scale. This is disappointing because it was previously
#quite significant. Though perhaps that was because of multicollinearity. I'm unsure as to where to progress from here. The same interactions
#and rules should exist in this model as well, but it seems that I was a bit far off. 

summary(m93)
#while this model is quite desirable for a lot of reasons I'm not sure if it is better than the centered model. Though the interaction 
#variables are a bit strange

uncenter <- m93$coefficients
exp(uncenter)

center <- m2$coefficients
exp(center)

((exp(uncenter))/(exp(uncenter)-exp(center))) * 100

#finding OR for going from normal to high day

PMOR <- exp(.0106441494 * 16)
O3OR <- exp(.0022220361 * 34)
NO2OR <- exp(.0054105087 * 26.5)
SO2_NO2OR <- exp(-.0010089467 * 337.97)
NO2_O3OR <- exp(-.0003054 * 1244.4)
SO2_NO2OR
NO2_O3OR

ORtable <- rbind(PMOR, O3OR, NO2OR, SO2_NO2OR, NO2_O3OR)
ORtable
#these results don't really make sense. I'm not sure how to interpet them qq

cbind(ORtable, confint.default(m2, 2:6))

confint <- confint.default(m2, 2:6)
confint[1,] <- exp(confint[1,] * 16)
confint[2,] <- exp(confint[2,] * 34)
confint[3,] <- exp(confint[3,] * 26.5)
confint[4,] <- exp(confint[4,] * 337.97)
confint[5,] <- exp(confint[5,] * 1244.4)

table <- cbind(ORtable, confint)
table
