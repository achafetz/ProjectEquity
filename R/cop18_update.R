#COP18 Budget and target to update infographic

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(ICPIutilities)
library(fastR)


# MER ---------------------------------------------------------------------

#import MSD
  df_mer <- read_rds("~/ICPI/Data/MER_Structured_Dataset_OU_IM_FY17-19_20190215_v1_1.rds")

#aggregate HTS & TX_NEW targets to OU & agency level
  targets <- df_mer %>% 
    filter(indicator %in% c("HTS_TST", "TX_NEW"),
           standardizeddisaggregate == "Total Numerator",
           fundingagency != "Dedup") %>% 
    group_by(operatingunit, fundingagency, indicator) %>% 
    summarise_at(vars(fy2019_targets), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    mutate(indicator = paste0("cop18_targets_", tolower(indicator))) %>% 
    spread(indicator, fy2019_targets)

#identify # of IMs reporting
  im_count <- df_mer %>% 
    filter(standardizeddisaggregate == "Total Numerator",
           fy2019_targets > 0,
           !is.na(fy2019_targets)) %>% 
    distinct(operatingunit, fundingagency, mechanismid) %>% 
    count(operatingunit, fundingagency) %>% 
    rename(cop18_im_count = n)

#join
  mer <- full_join(targets, im_count, by = c("operatingunit", "fundingagency"))


# ER ----------------------------------------------------------------------

#identify all fast tools
  files <- list.files("C:/Users/achafetz/Downloads/FASTs 2-21-19", full.names = TRUE)

#read in all fast tools
  df_fast <- map_dfr(.x = files,
                     .f = ~ combo_fastR(.x))
  
#extract PM and other budget
  budget <- df_fast %>% 
    mutate(program = ifelse(program == "PM", "cop18_budget_pm", "cop18_budget_no_pm")) %>% 
    filter(cop == "COP18") %>% 
    group_by(operatingunit, fundingagency,program) %>% 
    summarise_at(vars(amt), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    spread(program, amt, fill = 0) %>% 
    mutate(cop18_budget = cop18_budget_pm + cop18_budget_no_pm)


# COMBINE MER AND ER ------------------------------------------------------


  combo <- full_join(mer, budget, by = c("operatingunit", "fundingagency")) %>% 
    arrange(operatingunit, fundingagency)

  write_csv(combo, "Output/COP18_OU_Agency_Budget_Targets.csv", na = "")


# df_comm <- 
#   map_dfr(.x = files,
#           .f = ~run_fastR(.x, "5 Commodities-E"))
# 
#  df_comm %>% 
#    group_by(operatingunit, fundingagency) %>% 
#    summarise_at(vars(totalitem_budget), sum, na.rm = TRUE) %>% 
#    ungroup() %>% 
#    filter(totalitem_budget > 0)
   
   
 

