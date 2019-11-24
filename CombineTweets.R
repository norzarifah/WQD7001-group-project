################################################## MERGE CSV FILES

## path to folder that holds multiple .csv files
## create list of all .csv files in folder

fullpath<- "D:/Personal/UM/Master of Data Science/4th_Semester_2019/WQD7001/Group Assignment/MergeCSV/"
file_list <- list.files(path=fullpath, pattern="*.csv") # create list of all .csv files in folder

# read in each .csv file in file_list and rbind them into a data frame called data 
rawdata <- 
  do.call("rbind", 
          lapply(file_list, 
                 function(x) 
                   read.csv(paste(fullpath, x, sep=''), 
                            stringsAsFactors = FALSE)))

View(rawdata)
head(rawdata)
colnames(rawdata)
tail(rawdata)
dim(rawdata)
str(rawdata)
nrow(rawdata) # 11,376 records were created in the raw csv file.

################################################## CHECK FOR DUPLICATE ROWS IN DATA FRAME

#gives you a data frame with a list of ids and the number of times they occurred.
n_occur <- data.frame(table(rawdata$id))
View(n_occur)
nrow(n_occur) # Count how many records are duplicates or repeated, ie 5767 records are duplicated.
write.csv(n_occur,"writecsv/frequency.csv", row.names = TRUE)

#returns the records with more than one occurrence.  
repdata <- rawdata[rawdata$id %in% n_occur$Var1[n_occur$Freq > 1],]
View(repdata) 
nrow(repdata)
write.csv(repdata,"writecsv/RepeatRecords.csv", row.names = TRUE)

################################################## REMOVE DUPLICATE ROWS IN DATA FRAME
library(tidyverse)

noDup <- rawdata %>% distinct()
nrow(noDup) # 5900 records are unique.
write.csv(noDup,"writecsv/NoDuplicate.csv", row.names = TRUE)

