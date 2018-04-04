##   PROJECT EQUITY
##   A.Chafetz, J. Davis, & C. Wahle (USAID)
##   Purpose: munge data pulls from FACTSInfo
##   RPM 2018 follow up
##   Date: 2018.03.28
##   Updated: 2018.04.02


# Dependencies ------------------------------------------------------------

library(here)
library(tidyverse)
library(readxl)
library(fs)


# Project Folder Setup ----------------------------------------------------

  dir_create("Data")
  dir_create("Output")
  dir_create("Products")


# Cleaned Agency Names Mapping --------------------------------------------

  #import
    agency_mapping <- read_excel(here("Data", "COP15-18 Data 2018-03-23.xlsx"), sheet = "Reference", range = "A1:D24")
  
  #rename
    agency_mapping <- agency_mapping %>% 
      rename(fundingagency        = `Agency (Full)`,
             fundingagency_abbr   = `Agency (Abbrev)`,
             fundingagency_consol = `Agency (Consolidated)`,
             fundingagency_3      = `Agency (3)`) %>% 
      mutate(fundingagency_consol = factor(fundingagency_consol, 
                                            levels = c("USAID", "HHS/CDC", "PC", "State", "DOD", "HHS/Other")))

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
  
  #rename/clean/merge/arrange funding agencies
    #rename & tweak abbreviations for mapping
    copmatrix <- copmatrix %>% 
      rename(fundingagency_abbr = FundingAgency) %>% 
      mutate(fundingagency_abbr = case_when(
        fundingagency_abbr == "State/SGAC" ~ "State/OGAC",
        fundingagency_abbr == "HHS/OGHA"   ~ "HHS/OGA",
        TRUE                          ~ fundingagency_abbr
      ))
    #merge
    copmatrix <- left_join(copmatrix, agency_mapping, by = "fundingagency_abbr")
    #reorder
    copmatrix <- copmatrix %>% 
      select(concat, OperatingUnit, MechanismIdentifier, RecordType, COP, fundingagency, 
             fundingagency_abbr, fundingagency_consol,  fundingagency_3, everything()) 
      
  #data frame for mechanism budget code breakdown, long  
    # copmatrix_budgetcode <- copmatrix %>% 
    #   select(OperatingUnit:PrimePartnerPartnerType, HBHC:HVMS) %>% 
    #   gather(budgetcode, total_planned_and_pipeline, HBHC:HVMS, na.rm = TRUE)

  #export
    write_rds(copmatrix, here("Output", "copmatrix.Rds"))
    
  
# copmatrix_ptype <- read_excel(here("Data", "Budget Code COP Matrix Report-Combined.xlsx"))

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
  
  #rename OU/funding agency to be used across data sources
    staffing <- staffing %>% 
      rename(OperatingUnit = Operating_Unit,
             fundingagency = Funding_Agency,
             COP = Cycle) %>% 
      select(-Agency_Abbrev, -Agency_Consolidated)
  
  #merge
    staffing <- left_join(staffing, agency_mapping, by = "fundingagency")
  #reorder
    staffing <- staffing %>% 
      select(OperatingUnit, Cycle, fundingagency, fundingagency_abbr, 
             fundingagency_consol,  fundingagency_3, everything()) 
  
  #clean names for tables/viz
    staffing <- staffing %>% 
      mutate(Employment_Type = case_when(
        Employment_Type == "Personal Services Contractor"     ~ "PSC",
        Employment_Type == "Personal Services Agreement"      ~ "PSA",
        Employment_Type == "Non Personal Services Contractor" ~ "Non-PSC",
        TRUE                                                  ~ Employment_Type
      ))
  #export
    write_rds(staffing, here("Output", "staffing.Rds"))
    
# CODB --------------------------------------------------------------------

  #import
    codb <- read_excel(here("Data", "COP15-18 Data 2018-03-23.xlsx"), sheet = "CODB Data ", skip = 3)
  
  #rename header without spaces or dashes
    names(codb) <- gsub(" |\\)", "", names(codb))
    names(codb) <- gsub("-", "_", names(codb))
    names(codb) <- gsub("\\(", "_", names(codb))
  
  #rename funding agency to be used across data sources
    codb <- codb %>% 
      rename(fundingagency = FundingAgency,
             COP = Cycle)
  
  #merge
    codb <- left_join(codb, agency_mapping, by = "fundingagency")
    
  #reorder
    codb <- codb %>% 
      select(OperatingUnit, COP, fundingagency, fundingagency_abbr, 
             fundingagency_consol,  fundingagency_3, everything()) 
  #export
    write_rds(codb, here("Output", "codb.Rds"))
  
  rm(agency_mapping)

############################################################################
  # codb_ptype <- read_excel(here("Data", "Budget Code COP Matrix Report-Combined.xlsx"))
  # 
  # #rename header without spaces or dashes
  # names(codb_ptype) <- gsub(" |\\)", "", names(codb_ptype))
  # names(codb_ptype) <- gsub("-", "_", names(codb_ptype))
  # names(codb_ptype) <- gsub("\\(", "_", names(codb_ptype))
  # 
  # #rename funding agency to be used across data sources
  # codb_ptype <- codb_ptype %>% 
  #   rename(fundingagency = FundingAgency,
  #          COP = Cycle)
  # 
  # #covert to correct column type and convert to zeros
  # codb_ptype <- codb_ptype %>% 
  #   mutate(MechanismIdentifier = as.character(MechanismIdentifier)) %>% 
  #   mutate_if(is.numeric, ~ ifelse(. == 0, NA, .))
  # 
  # #merge
  # codb_ptype <- left_join(codb_ptype, agency_mapping, by = "fundingagency")
  
  ## COMPARE
  # codb_ptype %>% 
  #   group_by(fundingagency_consol, COP) %>% 
  #   summarise(PlannedAmount = sum(PlannedAmount, na.rm = TRUE)) %>% 
  #   ungroup() %>% 
  #   spread(COP, PlannedAmount)  %>% 
  #   kable(format.args = list(big.mark = ",", zero.print = FALSE))
  # 
  # codb %>% 
  #   group_by(fundingagency_consol, COP) %>% 
  #   summarise(TotalPlanned = sum(TotalPlanned, na.rm = TRUE)) %>% 
  #   ungroup() %>% 
  #   spread(COP, TotalPlanned)  %>% 
  #   kable(format.args = list(big.mark = ",", zero.print = FALSE))
  