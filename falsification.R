## check falsificaion
rm(list=ls())

# library
library(tidyverse)
library(cluster)
library(openxlsx)


# source
source("./R/data_falsification.R")

# load
data <- read.xlsx("./data/reach_lby_dataset_joint_market_monitoring_initiative_jmmi_May_2021.xlsx", sheet = "Data")
tool <- read.xlsx("./data/raw/04_JMMI_Tool (Apr2020).xlsx")

# check
data$q_orgname <- as.character(data$q_orgname)

enum.false <- calculateEnumeratorSimilarity(raw.data, tool, "q_orgname", "q_district")
write.xlsx(enum.false, "./output/silhouette_analysis_22042021")

false <- calculateDifferences(data, tool) %>% filter(number.different.columns < 5)
write.xlsx(false, "./output/similar_surveys_25052021")

# NAs
check_blanks<- function(data){
  data <- mutate(data, NAcols=
                   apply(data, 1,function(x) {length(which(is.na(x)))})/ncol(data)*100)
}

count_na <- function(x) sum(is.na(x))    



test <- check_blanks(data)

test <- df1 %>% 
  mutate(count_na2 = apply(., 1, function(x) sum(is.na(x))))
