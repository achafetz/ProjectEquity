##   PROJECT EQUITY
##   A.Chafetz, USAID
##   Purpose: munge data pulls from FACTSInfo
##   RPM 2018 follow up
##   Date: 2018.03.28
##   Updated: 2018.04.04


# Dependencies ------------------------------------------------------------

library(here)
library(tidyverse)
library(skimr)
library(knitr)
library(scales)

# Open Datasets -----------------------------------------------------------

  codb_ptype <- read_rds("Output/codb_ptype.Rds")

# 2b. # of IMs managed by different agencies, by OU  -----------------------

  # inspect
    skim(codb_ptype, ProcurementType)
    distinct(codb_ptype, ProcurementType)
  
  #table - global
    codb_ptype %>% 
      filter(COP == "2018 COP", MechanismIdentifier != "0") %>% 
      distinct(fundingagency_consol, ProcurementType, MechanismIdentifier) %>% 
      count(OperatingUnit, fundingagency_consol) %>% 
      