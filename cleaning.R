## Attempt #1 to clean the pollen.csv data

pollen <- read.csv("C:/Users/Trevor/Documents/Air/pollen.csv")
summary(pollen)

pollen[, -(12:17)]
pollen <- pollen[, -(12:17)]

new_pollen <- c()

file_list <- list.files(path = "C:/Users/Trevor/Documents/Air/Output", pattern="*.csv", full.names = TRUE)
pollen <- file_list[seq(1, length(file_list), 2)]

library(plyr)
# for each name file we "read" into a data.frame and the
# ldply function will make a rbind between these data.frames
data <- ldply(pollen, read.csv, sep = ",", header = TRUE, quote = "\"")
#data <- lapply(file_list, function(x) {read.csv(file=x,header=T, quote = "\"")} )
Reduce(function(x,y) {merge(x,y)}, data)
print("Start filtering ...\n")
#remove NA columns
filtered <- Filter(function(x)!all(is.na(x)), data)
write.csv(filtered, file = "pollen1.csv", row.names=FALSE)


#maybe this works
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


##
library(plyr)
mold_list <- ldply(mold, read.csv, sep = ",", header = TRUE, quote = "\"")

write.csv(mold_list, file = "mold.csv", row.names=FALSE)

pollen_list <- ldply(pollen, read.csv, sep = ",", header = TRUE, quote = "\"")

write.csv(pollen_list, file = "pollen_1.csv", row.names=FALSE)

pollen_1 <- read.csv("C:/Users/Trevor/Documents/Air/pollen_1.csv")
mold_1 <- read.csv("C:/Users/Trevor/Documents/Air/mold.csv")

wolololo <- merge(pollen_1, mold_1, by="DATE")

write.csv(wolololo, file = "wololo.csv", row.names=FALSE)
