

b <- 1
c <- 1
pollen <- c()
mold <- c()
a <- grepl("Mold",basename(file_list),ignore.case = TRUE)
summary(a)


for (i in 1:length(file_list)){
  if(a[i] == TRUE){
    mold[b] <- file_list[i]
    b <- b + 1
  }
  else{
    pollen[c] <- file_list[i]
    c <- c + 1
  }
}

test_list <- list.files(path = "C:/Users/Trevor/Documents/Air/test test", pattern="*.csv", full.names = TRUE)
test_list
library(plyr)
test_list1 <- ldply(test_list, read.csv, sep = ",", header = TRUE, quote = "\"")
test_list1

write.csv(test_list1, file = "two_combine.csv", row.names=FALSE)

for(file in pollen){
  
}


pollen1 <- read.csv("~/Air/pollen_1.csv")

#Remove all rows where DATE is NA
count <- c()
n <- 1
for (i in 1:length(pollen1$Ash)){
  if (is.na(pollen1$DATE[i] == TRUE)){
    count[n] <- i
    n <- n + 1
  }
}
pollen1 <- pollen1[-count,]

#Remove columns with only NA values
pollen1 <- pollen1[ , ! apply( pollen1 , 2 , function(x) all(is.na(x)) ) ]

#transfer the data from RAGWEED to Ragweed. There was one month where the handle RAGWEED was used instead of Ragweed
for (i in 1:nrow(pollen1)){
  if (is.na(pollen1$RAGWEED[i]) == FALSE ){
    pollen1$Ragweed[i] <- pollen1$RAGWEED[i]
  }
}

#delete columns that have unspecified variables, along with the now redundant RAGWEED column
pollen1 <- pollen1[, -c(62:88)]
pollen1 <- pollen1[, -17]
str(pollen1)

#cleanup some of the characters that don't matter
pollen1[pollen1=="WEEKEND"] <- NA
pollen1[pollen1=="Weekend"] <- NA
pollen1[pollen1=="W"] <- NA
pollen1[pollen1=="RAIN"] <- NA
pollen1[pollen1=="Rain"] <- NA
pollen1[pollen1=="NC"] <- NA
pollen1[pollen1=="HOLIDAY"] <- NA
pollen1[pollen1=="Holiday"] <- NA
pollen1[pollen1=="ELECTRICAL"] <- NA
pollen1[pollen1=="Mechanical Failure"] <- NA
pollen1[pollen1=="HEAVY"] <- NA
pollen1[pollen1=="MEDIUM"] <- NA
pollen1[pollen1=="LOW"] <- NA
pollen1[pollen1=="L"] <- NA
pollen1[pollen1=="Low"] <- NA
pollen1[pollen1=="Medium"] <- NA
pollen1[pollen1=="low"] <- NA

write.csv(pollen1, file="filtered_pollen2.csv", row.names=FALSE)
