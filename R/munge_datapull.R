##   PROJECT EQUITY
##   A.Chafetz, J. Davis, & C. Wahle (USAID)
##   Purpose: munge data pulls from FACTSInfo
##   RPM 2018 follow up
##   Date: 2018.03.28
##   Updated: 2018.08.06


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
    agency_mapping <- read_excel(here("Data", "COP16-18 Data 2018-08-06 v2.xlsx"), sheet = "Reference", range = "A1:D24")
  
  #rename
    agency_mapping <- agency_mapping %>% 
      rename(fundingagency        = `Agency (Full)`,
             fundingagency_abbr   = `Agency (Abbrev)`,
             fundingagency_consol = `Agency (Consolidated)`,
             fundingagency_3      = `Agency (3)`) %>% 
      mutate(fundingagency_consol = factor(fundingagency_consol, 
                                            levels = c("USAID", "HHS/CDC", "PC", "State", "DOD", "HHS/Other")))

# Identify LTS/STAR OUs ---------------------------------------------------
    #source: https://www.pepfar.gov/documents/organization/276459.pdf
    ou_star <- c("Asia Regional Program","Caribbean Region", "Central America Region",
                 "Central Asia Region", "Angola", "Burma", "Cambodia","Dominican Republic", 
                 "Ghana", "India", "Indonesia", "Papua New Guinea")
    
# COP Matrix --------------------------------------------------------------

  #import
    copmatrix <- read_excel(here("Data", "COP16-18 Data 2018-08-06 v2.xlsx"), sheet = "COP Matrix Data", skip = 2)
  
  #rename header without spaces or dashes
    names(copmatrix) <- gsub(" ", "", names(copmatrix))
    names(copmatrix) <- gsub("-", "_", names(copmatrix))
  
  #covert to correct column type and convert to zeros
    copmatrix <- copmatrix %>% 
      mutate(MechanismIdentifier = as.character(MechanismIdentifier)) %>% 
      mutate_at(vars(GAP, HVMS), ~ as.double(.)) %>% 
      mutate_if(is.numeric, ~ ifelse(. == 0, NA, .))
  #star designation
    copmatrix <- copmatrix %>% 
      mutate(ou_type = ifelse(OperatingUnit %in% ou_star, "STAR", "LTS"))
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
      select(concat, OperatingUnit, ou_type, MechanismIdentifier, RecordType, COP, fundingagency, 
             fundingagency_abbr, fundingagency_consol,  fundingagency_3, everything()) 
  #remove extra columns
    copmatrix <- copmatrix %>% 
      select(-`AdolescentGirlsandYoungWomen(AGYW)`:-Water)
  #export
    write_rds(copmatrix, here("Output", "copmatrix.Rds"))
    
  
# copmatrix_ptype <- read_excel(here("Data", "Budget Code COP Matrix Report-Combined.xlsx"))

# Staffing Data -----------------------------------------------------------

  #import
    staffing <- read_excel(here("Data", "COP16-18 Data 2018-08-06 v2.xlsx"), sheet = "Staffing Data", skip = 2)
  
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
      select(-Agency_Abbrev, -Agency_Consolidated) %>% 
      mutate(ou_type = ifelse(OperatingUnit %in% ou_star, "STAR", "LTS"))
  
  #merge
    staffing <- left_join(staffing, agency_mapping, by = "fundingagency")
  #reorder
    staffing <- staffing %>% 
      select(OperatingUnit, ou_type, COP, fundingagency, fundingagency_abbr, 
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
    codb <- read_excel(here("Data", "COP16-18 Data 2018-08-06 v2.xlsx"), sheet = "CODB Data ", skip = 3)
  
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
  
  #star designation
    codb <- codb %>% 
      mutate(ou_type = ifelse(OperatingUnit %in% ou_star, "STAR", "LTS")) 
    
  #reorder
    codb <- codb %>% 
      select(OperatingUnit, ou_type, COP, fundingagency, fundingagency_abbr, 
             fundingagency_consol,  fundingagency_3, everything()) 
  #export
    write_rds(codb, here("Output", "codb.Rds"))
  



# CODB Procurement Type ---------------------------------------------------

  #import
    codb_ptype <- read_excel(here("Data", "Budget Code COP Matrix Report-Combined.xlsx"))
    codb_ptype18 <- read_excel(here("Data", "Budget Code COP Matrix Report- COP 18.xlsx"), skip = 2) %>% 
      rename(Cycle = COP, `Operating Unit` = OU)
  
  #append COP18 onto other years
    codb_ptype <- bind_rows(codb_ptype, codb_ptype18)
      rm(codb_ptype18)
    
  #rename header without spaces or dashes
    names(codb_ptype) <- gsub(" |\\)", "", names(codb_ptype))
    names(codb_ptype) <- gsub("-", "_", names(codb_ptype))
    names(codb_ptype) <- gsub("\\(", "_", names(codb_ptype))

  #rename funding agency to be used across data sources
    codb_ptype <- codb_ptype %>%
      rename(fundingagency = FundingAgency,
             COP = Cycle)

  #covert to correct column type and convert to zeros
  codb_ptype <- codb_ptype %>%
    mutate(MechanismIdentifier = as.character(MechanismIdentifier)) %>%
    mutate_if(is.numeric, ~ ifelse(. == 0, NA, .))

  #merge
    codb_ptype <- left_join(codb_ptype, agency_mapping, by = "fundingagency")
  
  #star designation
    codb_ptype <- codb_ptype %>% 
      mutate(ou_type = ifelse(OperatingUnit %in% ou_star, "STAR", "LTS")) 
    
  #export
    write_rds(codb_ptype, here("Output", "codb_ptype.Rds"))
  
  rm(agency_mapping)
  

# EA ----------------------------------------------------------------------

  
  #import EA file
    ea <- read_rds("~/ICPI/Data/ICPI_EA_Structured_Dataset_PSNU_IM_20180110.Rds")
  
  # cost category groupings
  
    abv <- c("ProgMgmt", "StratInfo", "HlthSys", "Surveillance")
    inv <- c("TR", "CONST_REN", "VEHI", "EQP_FURN", "INV_OTHEXP")
    rec <- c("PERS", "ARV", "NONARV", "HIVTEST", "CONDOM", "OTHSUPPLY", "FOOD", "BLDGRENTAL", "TRVL", "REC_OTHEXP")
    grp <- c(inv, rec)
  
  #clean up
    ea <- ea %>% 
      #subset dataset to just each mechanism's total
      filter(data_type == "DIRECT",                                        #mech level = dup, rather than deduping at snu/ou level
             ((type == "Expenditure" & disaggregate_category %in% grp) |
                (disaggregate_category %in% abv)),      
             fy17 !=0, !is.na(fy17)) %>%                                   #keep only mech reporting expenditure >$0 on any indicator
      #adjustments to match MER data
      rename(operatingunit = ou,
             fundingagency_consol = mech_agency) %>%                                         
      mutate(fundingagency_consol = case_when(
                                fundingagency_consol == "DoD"               ~ "DOD",  
                                fundingagency_consol == "USAID"             ~ "USAID",
                                fundingagency_consol == "CDC"               ~ "HHS/CDC",
                                fundingagency_consol == "Peace Corps"       ~ "PC",
                                fundingagency_consol == "State_AF"          ~ "State",
                                fundingagency_consol == "State_PRM"         ~ "State",
                                fundingagency_consol == "State_EAP"         ~ "State",
                                fundingagency_consol == "State_WHA"         ~ "State",
                                       TRUE                                 ~ "HHS/Other"),
             operatingunit = case_when(operatingunit == "CotedIvoire"       ~ "Cote d'Ivoire",
                                       operatingunit == "DominicanRepublic" ~ "Dominican Republic",
                                       operatingunit == "DRC"               ~ "Democratic Republic of the Congo",
                                       operatingunit == "Guyana"            ~ "Caribbean Region",
                                       operatingunit == "PapuaNewGuinea"    ~ "Papua New Guinea",
                                       operatingunit == "SouthAfrica"       ~ "South Africa",
                                       operatingunit == "SouthSudan"        ~ "South Sudan",
                                       TRUE                                 ~ operatingunit),
             maj_cc = case_when(disaggregate_category %in% rec              ~ "Site - Recurrent",
                                disaggregate_category %in% inv              ~ "Site - Investment",
                                program_area == "LAB"                       ~ "Site - Lab",
                                disaggregate_category == "HlthSys"          ~ "Above Site - HSS",
                                disaggregate_category == "ProgMgmt"         ~ "Above Site - Program Managment",
                                disaggregate_category == "StratInfo"        ~ "Above Site - SI",
                                disaggregate_category == "Surveillance"     ~ "Above Site - Surveillance")
      )
  
  #aggregate up to mech total (currently one line per PSNU)
    ea <- ea %>% 
      group_by(operatingunit, fundingagency_consol, maj_cc) %>% 
      summarise(fy2017_expenditure = sum(fy17)) %>% 
      ungroup() %>% 
      mutate(fy2017_expenditure = round(fy2017_expenditure,0),
             fundingagency_consol = factor(fundingagency_consol, 
                                                  levels = c("USAID", "HHS/CDC", "PC", "State", "DOD", "HHS/Other"))) 
  #star designation
    ea <- ea %>% 
      mutate(ou_type = ifelse(operatingunit %in% ou_star, "STAR", "LTS")) %>%
      select(operatingunit, ou_type, everything())
    
  #export
    write_rds(ea, here("Output", "ea.Rds"))
  
  rm(abv, inv, rec, grp, ou_star)
  
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
  