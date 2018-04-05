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

copmatrix <- read_rds("Output/copmatrix.Rds")
codb_ptype <- read_rds("Output/codb_ptype.Rds")

# 2a. # of IMs managed by different agencies, by OU  -----------------------

# inspect
skim(copmatrix, COP, OperatingUnit,MechanismIdentifier)
distinct(copmatrix, COP)
distinct(copmatrix, MechanismIdentifier) %>% arrange(MechanismIdentifier)

skim(codb_ptype, ProcurementType)
distinct(codb_ptype, ProcurementType)