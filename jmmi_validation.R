### Libya JMMI Validation
rm(list = ls())

library(cleaninginspectoR)
library(tidyverse)
library(openxlsx)
library(cluster)

today <- Sys.Date()

## Sources
source("./R/check_log.R")
source("./R/data_falsification.R")

## Upload data
data <- "./data/reach_lby_dataset_joint_market_monitoring_initiative_jmmi_August_2021.xlsx"

sheets <- openxlsx::getSheetNames(data)
SheetList <- lapply(sheets,openxlsx::read.xlsx,xlsxFile=data)
names(SheetList) <- sheets

data <- SheetList[["Clean Data"]]
log <- SheetList[["Cleaning Log"]]
del <- SheetList[["Deletion Log"]]
tool <- SheetList[["KOBO survey"]]
mebs <- SheetList[["Cost of MEB"]]

## Issues
names(data)[names(data) == "_index"] <- "index"
data <- data %>% mutate(index = 1:nrow(data))
names(data)[names(data) == "_uuid"] <- "uuid"

issues <- inspect_all(df = data, uuid.column.name = "uuid") %>% filter(!is.na(index)) %>%
                      mutate(uuid= data[.$index,"uuid",drop=TRUE],city =data[.$index,"q_municipality", drop=TRUE]) %>%
                      filter(uuid %in% log$uuid)


write.xlsx(issues, paste0("./output/cleaning_check_",today,".xlsx"), overwrite = TRUE)

### Upload cleaning log and check that the same uuid have been checked

log <- log %>% filter(question.names %in% names(data))
log <- log %>% filter(question.names %in% names(data))

log.c <- check_log(data, log, variable = "question.names",  
                   old_log_var = "old.value", new_log_var = "new.value", 
                   uuid_data = "uuid", uuid_log = "uuid") %>% 
                    mutate(., check = ifelse(new.value == value_extracted, "Log applied correctly", "Please check log")) %>%
                            filter(check == "Please check log")

write.xlsx(log.c, paste0("./output/cleaning_log_check_",today,".xlsx"))

### Check MEBS
items <- data %>% select("q_district", "q_municipality","q_salt_price_per_kilo","q_sugar_price_per_kilo",           
                        "q_flour_price_per_kilo", "q_rice_price_per_kilo", "q_pasta_price_per_500g", "q_couscous_price_per_kilo",         
                        "q_tomatop_price_per_400g", "q_chickpeas_price_per_400g", "q_beans_price_per_400g", "q_cmilk_price_per_200ml",          
                        "q_milk_price_per_liter", "q_gtea_price_per_250g", "q_btea_price_per_250g", "q_oil_price_per_liter",             
                        "q_tuna_price_per_200g", "q_eggs_price_per_30eggs", "q_chicken_price_per_kilo", "q_lamb_price_per_kilo",            
                        "q_bread_price_per_5medium_pieces", "q_tomatoes_price_per_kilo", "q_onions_price_per_kilo", "q_pepper_price_per_kilo",           
                        "q_potatoes_price_per_kilo", "q_hwsoap_price_per_piece", "q_lsoap_price_per_kilo", "q_ldet_price_per_litre",           
                        "q_shampoo_price_per_250ml", "q_dsoap_price_per_liter", "q_toothpaste_price_per_tube", "q_toothbrush_price_per_brush",      
                        "q_spads_price_per_10pads", "q_diapers_price_per_30diapers", "q_hsan_price_per_liter", "q_ssan_price_per_liter",            
                        "q_public_gasoline_price_per_liter", "q_private_gasoline_price_per_liter", "q_paracetamol_price_per_12", "q_Ibuprofen_price_per_20",         
                        "q_vitamin_b_complex_price_per_40", "q_amoxicillin_price_per_21", "q_metoclopramide_price_per_40", "q_water_price_per_liter",           
                        "q_fuel_public_price_per_11kg", "q_fuel_private_price_per_11kg", "cooking_fuel_price_per_11kg")


## sources
source("./R/MEBCalculation_HQ.R")

mebs <- mebs %>% select(., "city", "full.meb")

mebs_check <- left_join(mebs, cities.meb, c("city")) %>% mutate(check = ifelse(full.meb == meb, TRUE, FALSE))

write.xlsx(mebs_check, paste0("./output/mebs check_",today,".xlsx"))

## falsification
false.surveys <- calculateDifferences(data, tool) %>% filter(number.different.columns < 5)
write.xlsx(false.surveys, paste0("./output/similar survyes_",today,".xlsx"))
