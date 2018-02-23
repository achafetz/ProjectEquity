##   PROJECT EQUITY
##   A.Chafetz, J.Davis, & C.Wahle, USAID
##   Purpose: create Agency Budget/Indicator comparison
##   RPM 2018
##   Date: 2018.02.20
##   Updated: 2018.02.22

# Dependencies
  library(tidyverse)
  library(googledrive)
  library(readxl)
  library(here)

#setup access
  drive_auth()
#
  googledrive::drive_find(n_max = 50)
    
  
# OPERATING UNITS ------------------------------------------------------------------------------------------
  #countries each week of RPM
  
  ous_wk1 <- c("Botswana", "Cameroon", "DRC", "Lesotho", "Namibia", "Nigeria", 
               "South Sudan", "Swaziland", "Uganda", "Zambia", "Zimbabwe")
  ous_wk2 <- c("Burundi", "Cote d'Ivoire", "Ethiopia", "Haiti", "Kenya", "Malawi", "Mozambique", 
               "South Africa", "Tanzania", "Ukraine", "Vietnam")
 
## MER ------------------------------------------------------------------------------------------------------
  #source: ICPI Fact View Q4v3_2
  
#import data
  fvdata <- "~/ICPI/Data/" #path where Fact View is stored
  df_mer <- read_rds(file.path(fvdata, "ICPI_FactView_OU_IM_20180209_v3_2.Rds"))
  
#agency totals 
  df_mer_agency <- df_mer %>% 
    filter(indicator %in% c("HTS_TST", "TX_NEW"), standardizeddisaggregate == "Total Numerator", 
           fundingagency %in% c("USAID", "HHS/CDC")) %>% 
    group_by(operatingunit, indicator, fundingagency) %>% 
    summarise_at(vars(fy2018_targets), ~ sum(., na.rm = TRUE)) %>% 
    ungroup %>% 
    mutate(indicator = paste("cop17", indicator, "targets", sep = "_" )) %>% 
    spread(indicator, fy2018_targets) %>% 
    rename_all(tolower) %>% 
    mutate(operatingunit = ifelse(operatingunit == "Democratic Republic of the Congo", "DRC", operatingunit), 
           fundingagency = ifelse(fundingagency == "HHS/CDC", "CDC", fundingagency))
  
#ou total 
  df_mer_ou <- df_mer %>% 
    filter(indicator %in% c("HTS_TST", "TX_NEW"), standardizeddisaggregate == "Total Numerator") %>% 
    group_by(operatingunit, indicator) %>% 
    summarise_at(vars(fy2018_targets), ~ sum(., na.rm = TRUE)) %>% 
    ungroup %>% 
    mutate(indicator = paste("cop17", indicator, "targets", sep = "_" )) %>% 
    spread(indicator, fy2018_targets) %>% 
    rename_all(tolower) %>% 
    mutate(operatingunit = ifelse(operatingunit == "Democratic Republic of the Congo", "DRC", operatingunit),
           fundingagency = "OU Total")
  
## BUDGET --------------------------------------------------------------------------------------------
  #source: FACTSInfo via A.Johnson (Feb 20, 2018)
  
#import week 1
  df_budget_wk1 <- read_excel(here("Data", "RPM M&O.xlsx"), sheet = "Week1 Original request", skip = 2)
  
#import week 2
  df_budget_wk2 <- read_excel(here("Data", "RPM M&O.xlsx"), sheet = "Week2 Original request", skip = 2)


## COMBINE DATASET ----------------------------------------------------------------------------------
  
#append mer
  df_combo <- bind_rows(df_mer_agency, df_mer_ou)
    
# append budget
  df_budget <- bind_rows(df_budget_wk1, df_budget_wk2) %>% 
    select(operatingunit, fundingagency, starts_with("cop17"))
  
#join
  df_combo <- full_join(df_budget, df_combo)
    rm(df_mer_agency, df_mer_ou, df_budget, df_budget_wk1, df_budget_wk2)
  
#define week
  df_combo <- df_combo %>% 
    mutate(wk = case_when(
        operatingunit %in% ous_wk1 ~ 1,
        operatingunit %in% ous_wk2 ~ 2)) 
  
#order & subset
  df_combo <- df_combo %>% 
    filter(!is.na(wk)) %>% 
    mutate(fundingagency = as.factor(fundingagency)) 
  
  df_combo$fundingagency <- fct_relevel(df_combo$fundingagency, "USAID", "CDC", "OU Total")
  
  df_combo <- df_combo %>%
    arrange(wk, operatingunit, fundingagency)
  
#week total
  df_combo_total <- df_combo %>% 
    group_by(wk, fundingagency) %>% 
    summarise_at(vars(starts_with("cop17") ), ~ sum(., na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(operatingunit = paste("Week", wk, "OUs", sep = " "), 
           cop17_mo_share = cop17_mo/cop17_funds_tot)

#append
  df_combo <- bind_rows(df_combo, df_combo_total) %>% 
    select(wk, everything())
    rm(df_combo_total)

## EXPORT --------------------------------------------------------------------------------------------
  
  write_csv(df_combo,here("Output", "equity_output.csv"), na = "")
           