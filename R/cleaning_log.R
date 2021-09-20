# script for cleaning cleaning_log
rm(list = ls())
library(readxl)
raw_data <- read_excel(path = "../October_data.xlsx", sheet = "Raw Data")
clean_data <- read_excel(path = "../October_data.xlsx", sheet = "Clean Data")
cleaning_log <- data.frame()
z<-0
for (i in 1:ncol(clean_data)) {
  for (j in 1:nrow(clean_data)) {
    if(!is.na(raw_data[j,i]) && is.na(clean_data[j,i])){
      z <-z+1
      cleaning_log[z,1]<-clean_data[j,"uuid"]
      cleaning_log[z,2]<- names(clean_data)[i]
      cleaning_log[z,3]<- raw_data[j,i]
      cleaning_log[z,4]<- ""
      cleaning_log[z,5]<- clean_data[j,"q_municipality"]
      } else
    if(!is.na(raw_data[j,i]) && !is.na(clean_data[j,i])){
    if(raw_data[j,i] != clean_data[j,i]){
      z <-z+1
      cleaning_log[z,1]<-clean_data[j,"uuid"]
      cleaning_log[z,2]<- names(clean_data)[i]
      cleaning_log[z,3]<- raw_data[j,i]
      cleaning_log[z,4]<- clean_data[j,i]
      cleaning_log[z,5]<- clean_data[j,"q_municipality"]
    }
  }
}}

names(cleaning_log) <- c("uuid", "question.names", "old.value", "new.value", "City")
write.csv(x = cleaning_log, file = "../cleaning_log.csv")
