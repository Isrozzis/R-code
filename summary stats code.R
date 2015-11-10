#Summary stats for Giulia

#Goal is to make stats that explain all of the data
#Yearly citywide averages for each pollutant, yearly trends for each pollutant, monthly trends for each pollutant

#Cleaning the data

#read in the files
file <- read.csv("C:/Users/Trevor/Documents/Air/data/1houravg_allsites_0209_7P.csv", header=FALSE) #02-09 data, don't use PM from here
file1 <- read.csv("C:/Users/Trevor/Documents/Air/data/1houravg_allsites_1012_6P.csv", header=FALSE) #10-12 data, no PM
file2 <- read.csv("C:/Users/Trevor/Documents/Air/data/newPM25_88502.csv", header=FALSE) #07-2012 PM
file3 <- read.csv("C:/Users/Trevor/Documents/Air/data/newPM25_88101.csv", header=FALSE) #02-06 PM

#files need to be properly merged and cleaned
#We need to first trim the file down to PM, NO, NO2, and O3. pollcodes = 88502, 88101 (PM), 42601, 42602, 44201 respectively

#file cleaning
file[file == 42101] <- NA
file[file == 88502] <- NA
file[file == 42401] <- NA
file[file == 42603] <- NA
file[file == 88101] <- NA
row.has.na <- apply(file, 1, function(x){any(is.na(x))})
file <- file[!row.has.na,]

#file1 cleaning
file1[file1 == 42101] <- NA
file1[file1 == 42401] <- NA
file1[file1 == 42603] <- NA
row.has.na1 <- apply(file1, 1, function(x){any(is.na(x))})
file1 <- file1[!row.has.na1,]

#file and file1 need to be consolidated into one dataframe
file5 <- rbind(file, file1)
rm(file, file1)

#file2 covers years 07-2012, file3 covers 02-06. They need to be consolidated into one data frame and then cleaned
file4 <- rbind(file3, file2)
rm(file2, file3)
cols.to.remove <- c("V9", "V10", "V11", "V12")
file4 <- file4[, ! names(file4) %in% cols.to.remove]

#file4 contains the PM values for the years 2002-2012. It needs to be added onto file5
air <- rbind(file5, file4)
rm(file4, file5)
attach(air)

#remove NA values. In this file NA values are recorded as 1.00E+9
air[air >= 10000000] <- NA

#summary stats show that there are values below 0 recorded as well. I'm going to assume that these are NA values
air[air < 0] <- NA

#add col names
colnames <- c("pollcode", "region", "site", "year", "month", "day", "hour", "value")
colnames(air) <- colnames

#The data is now properly cleaned, and labeled. Now it just needs to be seperated out for each pollutant
unique(pollcode)
NOdata <- subset(air, pollcode==42601)
NO2data <- subset(air, pollcode==42602)
O3data <- subset(air, pollcode==44201)
PMdata <- subset(air, pollcode==88502 | pollcode==88101)

#check to make sure that things subsetted correctly 
sum(nrow(NOdata ), nrow(NO2data), nrow(O3data), nrow(PMdata)) == nrow(air)

#
a <- 1
count <- 1
average <- c()

while(day==a){
  for(i in 0:23){
    if (value[i] == TRUE){
      count <- count + 1
    }
    if (count >= 18){
      running_average <- running_average + value[i]
    }
    average
  }
  
}

for(i in 2002:2013){
  for(c in 1:12){
    if(month[c]== (1 | 3 | 5 | 7 | 8 | 12)){
      
    }
    if(month[c]== ())
  }
}