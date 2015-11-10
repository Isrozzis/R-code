#preparing the data for KNN analysis and then doing it. 

air <- read.csv("C:/Users/Trevor/Documents/Air/003subjects_poll_merged_5dlag.csv")

#now I want to use only the complete cases, and generate 5 day averages

row.has.na <- apply(data, 1, function(x){any(is.na(x))})
sum(row.has.na)
air <- data[!row.has.na,]

#the data has now contains no rows that had NA values. 

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

#now I want to create a new data frame to do KNN on that contains event and 5 day averages. Later I may make one that has 
#0-4 day values, but for now a simple model is sufficient

air_new <- as.data.frame(air[,c(2,66:71)])

#uses the user defined function normalize to normalize the data for KNN

air_new <- sapply(air_new[,2:7], normalize)
air_new <- as.data.frame(air_new)
air_new$event <- air[,2]


air_train <- air_new[1:7352,]
air_train_target <- air_new[1:7352, 7]
air_test <- air_new[7353:length(air$event),]
air_test_target <- air_new[7352:length(air$event),7]
air_test_target <- air_test_target[1:7352]


library(class)
knn <- knn(air_train, air_test, air_train_target, 35)
knn1 <- knn.cv(air_train, air_train_target, 35)

table(air_test_target, knn)
# 86% accuracy
# much higher accuracy than logistic regression models, but still not great 



