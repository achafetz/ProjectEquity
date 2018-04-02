##   PROJECT EQUITY
##   A.Chafetz, USAID
##   Purpose: munge data pulls from FACTSInfo
##   RPM 2018 follow up
##   Date: 2018.03.28



# Dependencies ------------------------------------------------------------

library(here)
library(tidyverse)
library(readxl)
library(fs)


# Project Folder Setup ----------------------------------------------------

  dir_create("Data")
  dir_create("Output")
  dir_create("Products")


# COP Matrix --------------------------------------------------------------

  #import
    copmatrix <- read_excel(here("Data", "COP15-18 Data 2018-03-23.xlsx"), sheet = "COP Matrix Data", skip = 2)
  
  #rename header without spaces or dashes
    names(copmatrix) <- gsub(" ", "", names(copmatrix))
    names(copmatrix) <- gsub("-", "_", names(copmatrix))
  
  #covert to correct column type and convert to zeros
    copmatrix <- copmatrix %>% 
      mutate(MechanismIdentifier = as.character(MechanismIdentifier)) %>% 
      mutate_at(vars(GAP, HVMS), ~ as.double(.)) %>% 
      mutate_if(is.numeric, ~ ifelse(. == 0, NA, .))
  
  #data frame for mechanism budget code breakdown, long  
    # copmatrix_budgetcode <- copmatrix %>% 
    #   select(OperatingUnit:PrimePartnerPartnerType, HBHC:HVMS) %>% 
    #   gather(budgetcode, total_planned_and_pipeline, HBHC:HVMS, na.rm = TRUE)


# Staffing Data -----------------------------------------------------------

  #import
    staffing <- read_excel(here("Data", "COP15-18 Data 2018-03-23.xlsx"), sheet = "Staffing Data", skip = 2)
  
  #rename header without spaces or dashes
    names(staffing) <- gsub("%", "pct", names(staffing))
    names(staffing) <- gsub(" FTE", "_FTE", names(staffing))
    names(staffing) <- gsub("\\(|\\)", "", names(staffing))
    names(staffing) <- gsub(" - | |,", "_", names(staffing))
    names(staffing) <- gsub("-", "", names(staffing))
  
  #fix column types
    staffing <- staffing %>% 
      mutate(Position_Title = as.character(Position_Title)) %>% 
      mutate_at(vars(CIRC_pct:SIMS_OutofOffice_Days), ~ as.double(.))
  

# CODB --------------------------------------------------------------------

  #import
    codb <- read_excel(here("Data", "COP15-18 Data 2018-03-23.xlsx"), sheet = "CODB Data", skip = 4)
  
  #rename header without spaces or dashes
    names(codb) <- gsub(" |\\)", "", names(codb))
    names(codb) <- gsub("-", "_", names(codb))
    names(codb) <- gsub("\\(", "_", names(codb))
    