#cleaning the data

data <- read.csv("C:/Users/Trevor/Documents/Air/003subjects_poll_merged_5dlag.csv")

row.has.na <- apply(data, 1, function(x){any(is.na(x))})

sum(row.has.na)

air <- data[!row.has.na,]

#the data has now contains no rows that had NA values. I can do likelihood ratio tests now. 

#the code below intializes new variables to store the 5 day values in, and then assigns the proper values to them. 
air$five_day_CO <- c()
air$five_day_SO2 <- c()
air$five_day_NO <- c()
air$five_day_NO2 <- c()
air$five_day_Nox <- c()
air$five_day_O3 <- c()
air$five_day_PM <- c()

for(i in 1:length(air$val0_CO)){
  air$five_day_CO[i] <- (air$val0_CO[i] + air$val1_CO[i] + air$val2_CO[i] + air$val3_CO[i] + air$val4_CO[i])/5
  air$five_day_SO2[i] <- (air$val0_SO2[i] + air$val1_SO2[i] + air$val2_SO2[i] + air$val3_SO2[i] + air$val4_SO2[i])/5
  air$five_day_NO[i] <- (air$val0_NO[i] + air$val1_NO[i] + air$val2_NO[i] + air$val3_NO[i] + air$val4_NO[i])/5
  air$five_day_NO2[i] <- (air$val0_NO2[i] + air$val1_NO2[i] + air$val2_NO2[i] + air$val3_NO2[i] + air$val4_NO2[i])/5
  air$five_day_NOx[i] <- (air$val0_NOx[i] + air$val1_NOx[i] + air$val2_Nox[i] + air$val3_Nox[i] + air$val4_NOx[i])/5
  air$five_day_O3[i] <- (air$val0_O3[i] + air$val1_O3[i] + air$val2_O3[i] + air$val3_O3[i] + air$val4_O3[i])/5
  air$five_day_PM[i] <- (air$val0_PM[i] + air$val1_PM[i] + air$val2_PM[i] + air$val3_PM[i] + air$val4_PM[i])/5
}

air$three_day_CO <- c()
air$three_day_SO2 <- c()
air$three_day_NO <- c()
air$three_day_NO2 <- c()
air$three_day_Nox <- c()
air$three_day_O3 <- c()
air$three_day_PM <- c()

for(i in 1:length(air$val0_CO)){
  air$three_day_CO[i] <- (air$val0_CO[i] + air$val1_CO[i] + air$val2_CO[i])/3
  air$three_day_SO2[i] <- (air$val0_SO2[i] + air$val1_SO2[i] + air$val2_SO2[i])/3
  air$three_day_NO[i] <- (air$val0_NO[i] + air$val1_NO[i] + air$val2_NO[i])/3
  air$three_day_NO2[i] <- (air$val0_NO2[i] + air$val1_NO2[i] + air$val2_NO2[i])/3
  air$three_day_NOx[i] <- (air$val0_NOx[i] + air$val1_NOx[i] + air$val2_Nox[i] )/3
  air$three_day_O3[i] <- (air$val0_O3[i] + air$val1_O3[i] + air$val2_O3[i])/3
  air$three_day_PM[i] <- (air$val0_PM[i] + air$val1_PM[i] + air$val2_PM[i])/3
}

for(i in 1:length(air$val0_CO)){
  air$two_four_CO[i] <- (air$val2_CO[i] + air$val3_CO[i] + air$val4_CO[i])/3
  air$two_four_NO2[i] <- (air$val2_NO2[i] + air$val3_NO2[i] + air$val4_NO2[i])/3
}

for(i in 1:length(air$event)){
  air$one_two_fourNO2[i] <- (air$val0_NO2[i] + air$val2_NO2[i] + air$val3_NO2[i] + air$val4_NO2[i])/4
  air$one_two_fourSO2[i] <- (air$val0_SO2[i] + air$val2_SO2[i] + air$val3_NO2[i] + air$val4_NO2[i])/4
}

air$SO2_NO2_4 <- air$one_two_fourNO2 * air$one_two_fourSO2
air$SO2_O3 <- air$five_day_SO2 * air$five_day_O3
air$SO2_CO <- air$five_day_SO2 * air$five_day_CO
air$SO2_NO <- air$five_day_SO2 * air$five_day_NO
air$SO2_NO2 <- air$five_day_NO2 * air$five_day_SO2
air$SO2_PM <- air$five_day_SO2 * air$five_day_PM
air$PM_O3 <- air$five_day_PM * air$five_day_O3
air$PM_NO2 <- air$five_day_PM * air$five_day_NO2
air$PM_CO <- air$five_day_CO * air$five_day_PM
air$O3_PM <- air$five_day_O3 * air$five_day_PM
air$O3_NO2 <- air$five_day_O3 * air$five_day_NO2


air$gender <- c()

summary(air$female)

#using the match function so I don't have to wait for 30 minutes for it to iterate
for (i in 1:length(air$event)){
  if(is.na(match(air$UNQID[i],strata$Females))){
    air$Female_1[i] <- 0
  }
  else{
    air$Female_1[i] <- 1
  }
}

air$race <- c()

for (i in 1:length(air$event)){
  if(!is.na(match(air$UNQID[i], strata$Black))){
    air$race[i] <- 1
  }
  if (!is.na(match(air$UNQID[i], strata$White))){
    air$race[i] <- 0
  }
  if (!is.na(match(air$UNQID[i], strata$Hispanic))){
    air$race[i] <- 2
  }
  if (!is.na(match(air$UNQID[i], strata$Other_ethnic))){
    air$race[i] <- 3
  }
}

air$age <- c()
for (i in 1:length(air$event)){
  if(!is.na(match(air$UNQID[i], strata$Age1_4))){
    air$age[i] <- 0
  }
  if (!is.na(match(air$UNQID[i], strata$Age5_14))){
    air$age[i] <- 1
  }
  if (!is.na(match(air$UNQID[i], strata$Age15_18))){
    air$age[i] <- 2
  }
}

air$insured <- c()

for (i in 1:length(air$event)){
  if(is.na(match(air$UNQID[i],strata$Insured))){
    air$insured[i] <- 0
  }
  else{
    air$insured[i] <- 1
  }
}

air$severity <- c()
for (i in 1:length(air$event)){
  if(!is.na(match(air$UNQID[i], strata$Mild_Int))){
    air$severity[i] <- 1
  }
  if (!is.na(match(air$UNQID[i], strata$Mild_Pers))){
    air$severity[i] <- 0
  }
  if (!is.na(match(air$UNQID[i], strata$Moderate))){
    air$severity[i] <- 2
  }
  if (!is.na(match(air$UNQID[i], strata$Severe))){
    air$severity[i] <- 3
  }
}

air$NO_center <- air$five_day_NO - mean(air$five_day_NO)
air$CO_center <- air$five_day_CO - mean(air$five_day_O3)
air$O3_center <- air$five_day_O3 - mean(air$five_day_O3)
air$NO2_center <- air$five_day_NO2 - mean(air$five_day_NO2)
air$SO2_center <- air$five_day_SO2 - mean(air$five_day_SO2)
air$PM_center <- air$three_day_PM - mean(air$three_day_PM)
air$NO2_O3_center <- air$NO2_center * air$O3_center
air$SO2_NO2_center <- air$SO2_center * air$NO2_center

female_data <- data.frame()
male_data <- data.frame()

female_data <- subset(air, Female_1==1)
male_data <- subset(air, Female_1 == 0)

white_data <- data.frame()
black_data <- data.frame()
hispanic_data <- data.frame()
other_data <- data.frame()

white_data <- subset(air, race==0)
black_data <- subset(air, race==1)
hispanic_data <- subset(air, race==2)
other_data <- subset(air, race==3)
insured_data <- subset(air, insured==0)
uninsured_data <- subset(air, insured==1)

one_4_data <- data.frame()
five_14_data <- data.frame()
fifteen_18_data <- data.frame()

one_4_data <- subset(air, age == 0)
five_14_data <- subset(air, age == 1)
fifteen_18_data <- subset(air, age == 2)

mild_p_data <- data.frame()
mild_i_data <- data.frame()
moderate_data <- data.frame()
severe_data <- data.frame()

mild_i_data <- subset(air, severity==0)
mild_p_data <- subset(air, severity==1)
moderate_data <- subset(air, severity == 2)
severe_data <- subset(air, severity == 3)

