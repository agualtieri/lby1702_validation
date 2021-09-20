library("readxl")
library("tibble")
library("cleaninginspectoR")
library(magrittr)

#Only item price variables
subset_var <-readRDS("subset.rds")

#Read an excel workbook and extract only item price columns
data <- read_excel(path = "../november_data.xlsx", sheet = "Clean Data") %>% extract(subset_var)

#Run the function inspect_all from the cleaninginspector package and then attach the UUID variable and the city variable to
#the output file
inspect_all(df = data, uuid.column.name = "uuid") %>%
  mutate(uuid= data[.$index,"uuid",drop=TRUE],city =data[.$index,"q_municipality", drop=TRUE]) %>%
  write.csv("../cleaning_check.csv")

# this is usefull to follow-up with the partners. How many price data per item they sent 
#(to insure minimum of  4 price data per item )
data %>% select(-q_district) %>% group_by(q_region,q_municipality, q_orgname) %>% 
            summarise_all(list(~sum(!is.na(.)))) %>% 
              write.csv("../city_org_followup.csv")

#Same thing .How many price data per item they sent (to insure minimum of  4 price data per item )
#different aggregatio
data %>% group_by(q_orgname,q_region, q_district, q_municipality) %>% 
  summarise_all(list(~sum(!is.na(.)))) %>% add_column(.,index= paste(.$q_orgname,.$q_municipality,sep = "_"),.after = 4) %>%
  write.csv("../followup.csv")

#Medians
data %>% select(-uuid) %>% group_by(q_orgname, q_region, q_district, q_municipality) %>% 
  summarise_all(list(~median(.,na.rm = TRUE))) %>% write.csv("../ngo_city_medians.csv")

#Medians
data %>% select(-uuid, -q_district) %>% group_by(q_region,q_municipality, q_orgname) %>% 
  summarise_all(list(~median(.,na.rm = TRUE))) %>% write.csv("../city_ngo_medians.csv")

#Check for the minimum
data[,sapply(data, is.numeric)] %>% 
  summarise_all(list(~min(.,na.rm = TRUE))) %>% write.csv("../min.csv")

#Check for the maximum
data[,sapply(data, is.numeric)] %>% 
  summarise_all(list(~max(.,na.rm = TRUE))) %>% write.csv("../max.csv")

#Medina by cities  
city_medians <- data %>% select(-uuid,-q_orgname) %>% group_by(q_region, q_district, q_municipality) %>% 
  summarise_all(list(~median(.,na.rm = TRUE)))

tripoli_median <- data %>% filter(q_municipality %in% c("Tripoli Center", "Abusliem", "Suq Aljumaa", "Hai Alandalus", "Ain Zara", "Tajoura")) %>%
  .[,sapply(.,is.numeric)] %>% 
  summarise_all(list(~median(.,na.rm = TRUE))) %>% add_column(q_region="West", q_district="Tripoli", q_municipality="Tripoli", .before = 1)


bind_rows(city_medians,tripoli_median) %>% write.csv("../city_medians.csv")
data %>% write.csv("../price_data.csv")
