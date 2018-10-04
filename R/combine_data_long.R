
#Convert long and combine

  #dependencies
    library(tidyverse)
    
  #import
    copmatrix <- read_rds("Output/copmatrix.Rds")
  
  #drop unnecessary columns
    copmatrix_lng <- copmatrix %>% 
      select(-c(concat, fundingagency, fundingagency_abbr, fundingagency_3, GAP, GHP_USAID, 
                GHP_State, TotalAllFundingSources, TotalPlannedAmountandAppliedPipelineAmount)) %>% 
  #remove M&O (included in from the M&O data)
      filter(RecordType != "Management and Operations") %>% 
  #gather 
      gather(BudgetCode, total_budget, AppliedPipelineAmount:HVMS, na.rm = TRUE)
  #seperate out new funding (COPAmount) and pipeline
    copmatrix_lng <- copmatrix_lng %>% 
      mutate(COPAmount = ifelse(BudgetCode != "AppliedPipelineAmount", total_budget, NA),
             AppliedPipeline = ifelse(BudgetCode == "AppliedPipelineAmount", total_budget, NA),
             BudgetCode = ifelse(BudgetCode == "AppliedPipelineAmount", "Applied Pipeline", BudgetCode),
             source = "COP Matrix Data"
             )
    rm(copmatrix)
    
  #import
    codb <- read_rds("Output/codb.Rds")
  
  #drop unnecessary columns
    codb_lng <- codb %>% 
      filter(FundingSource != "Total non-CODB") %>%
      select(-c(fundingagency, fundingagency_abbr, fundingagency_3, FundingSource, TechnicalArea, 
                Acronym, ItemDescription, PreFundingAmount, OnHoldAmount, FTE, PipelineAsOfEndOfQ1))
      
  #aggregate to cut row count
    codb_lng <- codb_lng %>% 
      group_by(OperatingUnit, ou_type, COP, fundingagency_consol, BudgetCode, CostType) %>% 
      summarise_at(vars(COPAmount, AppliedPipeline), ~ sum(., na.rm = TRUE)) %>% 
      ungroup()
    
  #fix issue around pipeline duplication of CostType for every budget code it applies to
    codb_lng_pipeline <- codb_lng %>% 
      select(-c(BudgetCode, COPAmount)) %>%  
      unique() %>% 
      group_by(OperatingUnit, ou_type, COP, fundingagency_consol, CostType) %>% 
      summarise(AppliedPipeline = max(AppliedPipeline, na.rm = TRUE)) %>%
      ungroup() %>% 
      filter(AppliedPipeline!=0)
    
  #append accurate pipeline back on, removing 
    codb_lng <- codb_lng %>% 
      select(-AppliedPipeline) %>% 
      filter(COPAmount != 0) %>% 
      bind_rows(codb_lng_pipeline)
    rm(codb_lng_pipeline)
      
  #create a total column
    codb_lng <- codb_lng %>% 
      mutate_at(vars(COPAmount, AppliedPipeline), ~ ifelse(is.na(.), 0, .)) %>% #convert NAs to 0 for addding
      mutate(total_budget = COPAmount + AppliedPipeline,
             source = "CODB Data",
             RecordType = "Management and Operations") %>% 
      filter(total_budget != 0)
    rm(codb)
    
  #import
    staffing <- read_rds("Output/staffing.Rds")
  
  #drop unnecessary columns
    staffing_lng <- staffing %>% 
      select(-c(fundingagency, Last_Name, First_Name, Staffing_ID, Position_Title, pct_Time_Spent_on_Gender, Location, Other_Roles,
                Country_Where_Based_Or_Assigned, Comments, Total_FTEs, pct_Time_Devoted_to_PEPFAR_by_Each_Individual,
                Schedule, ends_with("_pct"), Intraagency_Admin__Training__Financial_Management_FTE,
                Intraagency_Partner_Management__CoAg_Admin__Site_Visits_FTE, Interagency_Leadership_FTE, Interagency_Other_FTE,
                External_Engagement_Leadership_FTE, External_Engagement_Technical_FTE, Missionwide_Activities_FTE, SIMS_OutofOffice_Days,
                fundingagency_abbr, fundingagency_3
                ))
  #had to drop pct_Time_Devoted_to_PEPFAR_by_Each_Individual due to aggregation. can/should it be included?
  #aggregate
    staffing_lng <- staffing_lng %>%
      group_by(OperatingUnit, COP, Employment_Type, Funding_Type, Citizenship_Type, Staffing_Status, Position_Type, ou_type, fundingagency_consol) %>% 
      summarise_at(vars(Number_of_Individuals, ends_with("_FTE")), ~ sum(., na.rm = TRUE)) %>% 
      ungroup()
    
  #gather
    staffing_lng <- staffing_lng %>% 
      gather(RecordType, amount, Number_of_Individuals, ends_with("_FTE"), na.rm = TRUE) %>% 
      filter(amount != 0)
    
  #munge
    staffing_lng <- staffing_lng %>% 
      mutate(BudgetCode = ifelse(str_detect(RecordType, "_FTE"), str_remove(RecordType, "_FTE"), NA),
             RecordType = ifelse(str_detect(RecordType, "_FTE"), "Staffing - FTEs", RecordType),
             FTE = ifelse(RecordType == "Staffing - FTEs", amount, NA),
             individuals = ifelse(RecordType == "Number_of_Individuals", amount, NA),
             RecordType = ifelse(RecordType == "Number_of_Individuals", "Staffing - # of Individuals/Positions", RecordType),
             source = "Staffing Data"
      ) %>% 
      select(-amount)
             
    rm(staffing)
    
  mo_combo <- full_join(copmatrix_lng, codb_lng)
  mo_combo <- full_join(mo_combo, staffing_lng)
  
  mo_combo <- rename_all(mo_combo, ~ tolower(.))
  
  mo_combo <- mo_combo %>% 
    select(cop, operatingunit, ou_type, mechanismidentifier, fundingagency_consol, primepartner,	mechanismname,	
           primepartnerorgtype,	primepartnerpartnertype, recordtype, budgetcode, costtype,	employment_type,
           funding_type,	citizenship_type,	staffing_status,	position_type, total_budget,	copamount,	
           appliedpipeline, fte,	individuals, source)
  
  write_csv(mo_combo, "Output/MO_combined.csv", na = "")

  