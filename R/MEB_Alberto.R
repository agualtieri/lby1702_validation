# rm(list=ls(all=TRUE))

if (!require("pacman")) install.packages("pacman")

pacman::p_load(rio,plyr,tidyverse,cleaninginspectoR,tibble,magrittr,reshape2)

#data <- readRDS("data.rds")


meb.composition <- import("./data/MEB_composition.xlsx", sheet="hh5" ) %>% extract(c("item","quantity"))

### you can ignore these lines ###
#data <- within(data, q_municipality[q_district == "Tripoli"] <- "Tripoli")

#data$q_district <-  NULL

#colnames(data)[1]  <-  "city"



### stop ignore ### 

data.all.2 <- melt(data, id = "city")

medians.all <- ddply(data.all.2, .(data.all.2$city,variable), summarise, med = median(value,na.rm=TRUE))


cities.meb <-  data.frame(matrix(ncol = 2, nrow = 0))

colnames(cities.meb) <-  c("city","meb")

libya.item.medians <- ddply(medians.all, .(variable), summarise, med = median(med,na.rm=TRUE))

i=0

for (city in unique(medians.all$`data.all.2$city`)) {
  
  i =i+1
  
  item.medians <- medians.all %>% filter(`data.all.2$city`==city) %>% select(variable,med)
  
  colnames(item.medians)[1]  <-  "item"
  
  colnames(item.medians)[2]  <-  "unit.price"
  
  meb.items <- merge(item.medians,meb.composition,by = "item")
  
  meb.items <- meb.items %>% mutate(price = unit.price * quantity)
  
  
  meb.cost <- sum(meb.items$price,na.rm = F)
  
  
  new.observation <- c(city,meb.cost)
  
  cities.meb[i,] <- new.observation
  
}

cities.meb$meb <- as.numeric(cities.meb$meb)

cities.meb <-cities.meb[order(cities.meb$meb,decreasing = T),]

View(cities.meb)

export(cities.meb,"MEB.xlsx")
