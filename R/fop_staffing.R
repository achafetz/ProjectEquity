#Provide an output for F/OP countries

library(tidyverse)
library(readxl)

staffing_reg <- read_xlsx("Data/Regionalization Staffing Matrix 20181011.xlsx", sheet = "PEPFAR-Funded Staff", skip = 1)
glimpse(staffing_reg)

#remove example from dataset
staffing_reg <- staffing_reg %>% 
  filter(`Operating Unit` != "EXAMPLE")

staffing_reg <- staffing_reg %>% 
  rename(operatingunit = `Operating Unit`,
         fundingagency_consol = `Funding Agency`,
         employment_type = `Hiring Mechanism`,
         staffing_status = `Filled/Vacant`,
         amt_managed = `Amount of annual funds managed (USD)`
         ) %>% 
  mutate(recordtype = "Staffing - FTEs",
         source = "Regionalization Staffing Matrix")

west_african <- c("Ghana","Liberia", "Mali", "Sierra Leone", "Senegal", "West Africa Regional")

staffing_reg <- staffing_reg %>% 
  filter(operatingunit %in% west_african)

staffing_reg <- staffing_reg %>% 
  mutate(cop = "2018 COP") %>% 
  select(cop, operatingunit, fundingagency_consol, recordtype, employment_type, staffing_status, starts_with("Prorated"), amt_managed, source) 
  

staffing_reg_lng <- staffing_reg %>% 
  select(-amt_managed) %>% 
  mutate_at(vars(starts_with("Prorated")), ~ ifelse(. == 0, NA, .)) %>% 
  gather(position_type, fte, starts_with("Prorated"), na.rm = TRUE) %>% 
  mutate(position_type = str_remove_all(position_type, "(Prorated | LOE)"))

staffing_reg_lng <- staffing_reg_lng %>% 
  mutate(position_type = case_when(position_type == "Administrative" ~ "Administrative and Logistics Support",
                                   position_type == "Leadership"     ~ "US Mission Leadership and Public Diplomacy",
                                   position_type == "Management"     ~ "Technical Leadership/Management",
                                   position_type == "Technical"      ~ "Technical and Programmatic Oversight and Support",
                                   position_type == "???"            ~ "Contracting/Financial/Legal",
                                   TRUE                              ~ "Unknown"
                                   )
         )


staffing_reg_rollup <- staffing_reg %>% 
  mutate(fte_total = `Prorated Administrative LOE` + `Prorated Leadership LOE` + `Prorated Management LOE` + `Prorated Technical LOE`) %>% 
  select(-starts_with("Prorated")) %>% 
  mutate(staff = 1,
         recordtype = "Staffing - Budget Managed")

staffing_combo <- bind_rows(staffing_reg_lng, staffing_reg_rollup)

write_csv(staffing_combo, "Output/mo_staffing_westafrica.csv", na = "" )
