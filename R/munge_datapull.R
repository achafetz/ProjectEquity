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
             fundingagency_3      = `Agency (3)`)
    
  #factor to order in tables/viz
    #levels_agency_consol <- c("USAID", "HHS/CDC", "PC", "State", "DOD", "HHS/Other")
    #agency_mapping$fundingagency_consol <- factor(agency_mapping$fundingagency_consol, levels = levels_agency_consol)
      #rm(levels_agency_consol)

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
    write_rds(copmatrix, here("Output", "cop_matrix.Rds"))
    
  
 ###   copmatrix_ptype <- read_excel(here("Data", "Budget Code COP Matrix Report-Combined.xlsx"))

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
             fundingagency = Funding_Agency) %>% 
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
    write_rds(copmatrix, here("Output", "staffing.Rds"))
    
# CODB --------------------------------------------------------------------

  #import
    codb <- read_excel(here("Data", "COP15-18 Data 2018-03-23.xlsx"), sheet = "CODB Data", skip = 4)
  
  #rename header without spaces or dashes
    names(codb) <- gsub(" |\\)", "", names(codb))
    names(codb) <- gsub("-", "_", names(codb))
    names(codb) <- gsub("\\(", "_", names(codb))
  
  #rename funding agency to be used across data sources
    codb <- codb %>% 
      rename(fundingagency = FundingAgency) %>% 
      select(-Agency_Abbrev, -Agency_Consolidated)
  
  #merge
    codb <- left_join(codb, agency_mapping, by = "fundingagency")
    
  #reorder
    codb <- codb %>% 
      select(OperatingUnit, fundingagency, fundingagency_abbr, 
             fundingagency_consol,  fundingagency_3, everything()) 
  #export
    write_rds(codb, here("Output", "codb.Rds"))
  
  rm(agency_mapping)
  