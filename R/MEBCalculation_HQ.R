

if (!require("pacman")) install.packages("pacman")

pacman::p_load(rio,plyr,tidyverse,cleaninginspectoR,tibble,magrittr) 

subset_var <-readRDS("./Feb_subset.rds")
subset_var <- subset_var[-c(1:2)]

data <- read.xlsx("./data/reach_lby_dataset_joint_market_monitoring_initiative_jmmi_August_2021.xlsx", sheet = "Clean Data")
data <- data %>% extract(subset_var)


# ###### MEB Calculation ######

meb.composition <- import("./data/MEB_composition.xlsx", sheet="hh5" ) %>% extract(c("item","quantity"))

data <- within(data, q_municipality[(q_district == "Tripoli" & q_municipality!= "Qasr Ben Ghashir" )] <- "Tripoli")
#data$cooking_fuel_price_per_11kg <- apply(data[,c("q_fuel_public_price_per_11kg","q_fuel_private_price_per_11kg")],1,median,na.rm=T)
data$q_district <-  NULL

colnames(data)[1]  <-  "city"

# colnames(data)

data.all.2 <- melt(data,id = c("city"))

# data.all.2$variable <- with(data.all.2,reorder(data.all.2$variable,value,function(x) - median(x, na.rm=TRUE)))


medians.all <- ddply(data.all.2, .(data.all.2$city,variable), summarise, med = median(value,na.rm=TRUE))


# unique(medians.all$`data.all.2$city`)

cities.meb <-  data.frame(matrix(ncol = 2, nrow = 0))
colnames(cities.meb) <-  c("city","meb")

libya.item.medians <- ddply(medians.all, .(variable), summarise, med = median(med,na.rm=TRUE))

i=0
for (city in unique(medians.all$`data.all.2$city`)) {
 
     i =i+1
    
    item.medians <- medians.all %>% filter(`data.all.2$city`==city) %>% select(variable,med)
    
    colnames(item.medians)[1]  <-  "item"
    
    colnames(item.medians)[2]  <-  "unit.price"
    
    item.medians$unit.price[item.medians$item=="cooking_fuel_price_per_11kg"]<- median(c(item.medians$unit.price[item.medians$item=="q_fuel_public_price_per_11kg"], item.medians$unit.price[item.medians$item=="q_fuel_private_price_per_11kg"]), na.rm = T)
    
    meb.items <- merge(item.medians,meb.composition,by = "item")
    
    meb.items <- meb.items %>% mutate(price = unit.price * quantity)
  
    # print(city)
    # print(meb.items)
    # readline(prompt="Press [enter] to continue")
    
    meb.cost <- sum(meb.items$price,na.rm = F)
    
    
    new.observation <- c(city,meb.cost)
    
    cities.meb[i,] <- new.observation

}

cities.meb$meb <- round(as.numeric(cities.meb$meb),2)

cities.meb <-cities.meb[order(cities.meb$meb,decreasing = T),]

write.csv(cities.meb,"./output/mebs.csv",row.names = F)

