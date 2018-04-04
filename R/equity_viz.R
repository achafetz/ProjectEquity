##   PROJECT EQUITY
##   A.Chafetz, USAID
##   Purpose: munge data pulls from FACTSInfo
##   RPM 2018 follow up
##   Date: 2018.03.28


# Dependencies ------------------------------------------------------------

library(here)
library(tidyverse)
library(skimr)
library(knitr)
library(scales)


# Open Datasets -----------------------------------------------------------

staffing <- read_rds("Output/staffing.Rds")
copmatrix <- read_rds("Output/copmatrix.Rds")
codb <- read_rds("Output/codb.Rds")

# 1. broad M&O categories for all agencies individually -------------------

  #inspect
    staffing %>% skim(fundingagency_consol, Employment_Type, Position_Type)

  #table - overall
    staffing %>% 
      filter(COP == "2018 COP") %>% 
      count(fundingagency_consol, sort = TRUE) %>% 
      kable(format.args = list(big.mark = ",", zero.print = FALSE))
  #table - overall, over time
    staffing %>% 
      count(fundingagency_consol, COP, sort = TRUE) %>% 
      spread(COP, n) %>% 
      kable(format.args = list(big.mark = ",", zero.print = FALSE))
    
  #table - by employment type 
    staffing %>% 
      filter(COP == "2018 COP") %>% 
      count(fundingagency_consol, Employment_Type) %>% 
      spread(Employment_Type, n) %>% 
      kable(format.args = list(big.mark = ",", zero.print = FALSE))
  #table - by position type
    staffing %>% 
      filter(COP == "2018 COP") %>% 
      count(fundingagency_consol, Position_Type) %>% 
      spread(fundingagency_consol, n) %>% 
      kable(format.args = list(big.mark = ",", zero.print = FALSE))
    
  #viz - overall  
    staffing %>%
      filter(COP == "2018 COP") %>% 
      count(fundingagency_consol, Employment_Type) %>% 
      ggplot(aes(reorder(Employment_Type, n), n)) +
        geom_col() +
        coord_flip() +
        labs(x = "", y = "# of staff") +
        scale_y_continuous(labels = comma)
    
   #viz - by employment type
    staffing %>% 
      filter(COP == "2018 COP") %>% 
      count(fundingagency_consol, Employment_Type) %>%
      mutate(usaid = ifelse(fundingagency_consol == "USAID", 1, 0)) %>% 
      ggplot(aes(reorder(Employment_Type, n), n)) +
        geom_col(aes(fill = usaid), show.legend = FALSE) +
        scale_fill_continuous(low = "#99c2eb", high = "#2166ac") +
        coord_flip() +
        labs(x = "", y = "# of staff") +
        scale_y_continuous(labels = comma) +
        facet_grid(. ~ fundingagency_consol)
     ggsave(here("Products", "prj_equity_q1_empl_type.png"), width = 11, height = 2.5, units = "in")
    
    #viz - by position type
     staffing %>% 
       filter(COP == "2018 COP") %>% 
       count(fundingagency_consol, Position_Type) %>%
       mutate(usaid = ifelse(fundingagency_consol == "USAID", 1, 0)) %>% 
       ggplot(aes(reorder(Position_Type, n), n)) +
         geom_col(aes(fill = usaid), show.legend = FALSE) +
         scale_fill_continuous(low = "#99c2eb", high = "#2166ac") +
         coord_flip() +
         labs(x = "", y = "# of staff") +
         scale_y_continuous(labels = comma) +
         facet_grid(. ~ fundingagency_consol)
     ggsave(here("Products", "prj_equity_q1_position_type.png"), width = 11, height = 2.5, units = "in")
  

# 2. # of IMs managed by different agencies, by OU  -----------------------

  # inspect
    skim(copmatrix, COP, OperatingUnit,MechanismIdentifier)
    distinct(copmatrix, COP)
    distinct(copmatrix, MechanismIdentifier) %>% arrange(MechanismIdentifier)
    
  #table - global
     copmatrix %>% 
       filter(COP == "2018 COP", MechanismIdentifier != "0") %>% 
       count(fundingagency_consol) %>% 
       arrange(desc(n))
     
  #table - by OU
     copmatrix %>% 
       filter(COP == "2018 COP", MechanismIdentifier != "0") %>% 
       count(OperatingUnit, fundingagency_consol) %>% 
       spread(fundingagency_consol, n) %>% 
       print(n = Inf)
     
  # viz - by OU
     copmatrix %>% 
       filter(COP == "2018 COP", MechanismIdentifier != "0") %>% 
       count(OperatingUnit, fundingagency_consol) %>%
       group_by(fundingagency_consol) %>% 
       mutate(usaid = ifelse(fundingagency_consol == "USAID", 1, 0),
              order_usaid = ifelse(fundingagency_consol == "USAID", dense_rank(n), 0)) %>% 
       ungroup() %>%
       ggplot(aes(reorder(OperatingUnit, order_usaid), n)) +
         geom_col(aes(fill = usaid), show.legend = FALSE) +
         scale_fill_continuous(low = "#99c2eb", high = "#2166ac") +
         coord_flip() +
         facet_grid(. ~ fundingagency_consol) +
         labs(title = "Mechanisms Supported by Agency", x = "", y = "# of mechanisms", caption = "COP18")


# 3a. M&O - % overall budget ----------------------------------------------

  #table - global
    copmatrix %>% 
      filter(COP == "2018 COP") %>% 
      group_by(fundingagency_consol) %>% 
      summarise(TotalPlannedAmountandAppliedPipelineAmount = 
                   sum(TotalPlannedAmountandAppliedPipelineAmount, na.rm = TRUE)) %>%
      ungroup() %>% 
      mutate(share = TotalPlannedAmountandAppliedPipelineAmount / sum(TotalPlannedAmountandAppliedPipelineAmount)) %>%
      arrange(desc(TotalPlannedAmountandAppliedPipelineAmount)) %>% 
      kable(format.args = list(big.mark = ",", zero.print = FALSE))
  
  #table - by OU
     copmatrix %>% 
       filter(COP == "2018 COP") %>% 
       group_by(OperatingUnit, fundingagency_consol) %>% 
       summarise(TotalPlannedAmountandAppliedPipelineAmount = 
                   sum(TotalPlannedAmountandAppliedPipelineAmount, na.rm = TRUE)) %>%
       ungroup() %>% 
       filter(TotalPlannedAmountandAppliedPipelineAmount != 0) %>% 
       group_by(OperatingUnit) %>% 
       mutate(share = TotalPlannedAmountandAppliedPipelineAmount / sum(TotalPlannedAmountandAppliedPipelineAmount)) %>%
       ungroup() %>% 
       arrange(OperatingUnit, desc(TotalPlannedAmountandAppliedPipelineAmount)) %>% 
       kable(format.args = list(big.mark = ",", zero.print = FALSE))   
     
  #viz - by OU
     copmatrix %>% 
       filter(COP == "2018 COP") %>% 
       group_by(OperatingUnit, fundingagency_consol) %>% 
       summarise(TotalPlannedAmountandAppliedPipelineAmount = 
                   sum(TotalPlannedAmountandAppliedPipelineAmount, na.rm = TRUE)) %>%
       ungroup() %>% 
       filter(TotalPlannedAmountandAppliedPipelineAmount != 0) %>% 
       group_by(OperatingUnit) %>% 
       mutate(share = TotalPlannedAmountandAppliedPipelineAmount / sum(TotalPlannedAmountandAppliedPipelineAmount),
              usaid = ifelse(fundingagency_consol == "USAID", 1, 0)) %>%
       ungroup() %>% 
       ggplot(aes(reorder(OperatingUnit, share), share)) +
         geom_col(aes(fill = usaid), show.legend = FALSE) +
         scale_y_continuous(labels = percent) +
         scale_fill_continuous(low = "#99c2eb", high = "#2166ac") +
         coord_flip() +
         labs(x = "") +
         facet_grid(. ~ fundingagency_consol)
     
# 3b. M&O - % M&O v % IM --------------------------------------------------
     
  #table - global
     copmatrix %>% 
       filter(COP == "2018 COP") %>% 
       group_by(fundingagency_consol, RecordType) %>% 
       summarise(TotalPlannedAmountandAppliedPipelineAmount = 
                   sum(TotalPlannedAmountandAppliedPipelineAmount, na.rm = TRUE)) %>%
       ungroup() %>% 
       group_by(fundingagency_consol) %>% 
       mutate(share = TotalPlannedAmountandAppliedPipelineAmount / sum(TotalPlannedAmountandAppliedPipelineAmount)) %>% 
       filter(RecordType == "Management and Operations", !is.na(fundingagency_consol)) %>% 
       arrange(desc(share)) %>% 
       kable(format.args = list(big.mark = ",", zero.print = FALSE))
     
  #table - by OU
    copmatrix %>% 
      filter(COP == "2018 COP") %>%
      group_by(OperatingUnit, fundingagency_consol, RecordType) %>% 
      summarise(TotalPlannedAmountandAppliedPipelineAmount = 
                  sum(TotalPlannedAmountandAppliedPipelineAmount, na.rm = TRUE)) %>%
      ungroup() %>% 
      group_by(OperatingUnit, fundingagency_consol) %>% 
      mutate(share = TotalPlannedAmountandAppliedPipelineAmount / sum(TotalPlannedAmountandAppliedPipelineAmount)) %>% 
      filter(RecordType == "Management and Operations", !is.na(fundingagency_consol)) %>% 
      arrange(desc(share)) %>% 
      kable(format.args = list(big.mark = ",", zero.print = FALSE))
     
  # viz by OU
    copmatrix %>% 
      filter(COP == "2018 COP") %>%
      group_by(OperatingUnit, fundingagency_consol, RecordType) %>% 
      summarise(TotalPlannedAmountandAppliedPipelineAmount = 
                  sum(TotalPlannedAmountandAppliedPipelineAmount, na.rm = TRUE)) %>%
      ungroup() %>% 
      group_by(OperatingUnit, fundingagency_consol) %>% 
      mutate(share = TotalPlannedAmountandAppliedPipelineAmount / sum(TotalPlannedAmountandAppliedPipelineAmount),
             usaid = ifelse(fundingagency_consol == "USAID", 1, 0)) %>% 
      filter(RecordType == "Management and Operations", !is.na(fundingagency_consol)) %>% 
      ggplot(aes(reorder(OperatingUnit, share), share)) +
        geom_col(aes(fill = usaid), show.legend = FALSE) +
        scale_y_continuous(labels = percent) +
        scale_fill_continuous(low = "#99c2eb", high = "#2166ac") +
        coord_flip() +
        facet_grid(. ~ fundingagency_consol)

# 3c. M&O - subcategories -------------------------------------------------

  #table - global
    codb %>%
      filter(COP == "2018 COP",
             CostType %in% c("Institutional Contractors", 
                             "Management Meetings/Professional Development", 
                             "Peace Corps Volunteer Costs", "Staff Program Travel", 
                             "USG Staff Salaries and Benefits", 
                             "USG Staff Salaries and Benefits - Internationally  Recruited", 
                             "USG Staff Salaries and Benefits - Locally Recruited")) %>% 
      group_by(fundingagency_consol, CostType) %>% 
      summarise(COPAmount = sum(COPAmount, na.rm = TRUE)) %>% 
      ungroup() %>% 
      filter(COPAmount != 0) %>% 
      group_by(fundingagency_consol) %>% 
      mutate(share = COPAmount / sum(COPAmount)) %>% 
      ungroup()
      
  #table - by OU
    codb %>%
      filter(COP == "2018 COP",
             CostType %in% c("Institutional Contractors", 
                             "Management Meetings/Professional Development", 
                             "Peace Corps Volunteer Costs", "Staff Program Travel", 
                             "USG Staff Salaries and Benefits", 
                             "USG Staff Salaries and Benefits - Internationally  Recruited", 
                             "USG Staff Salaries and Benefits - Locally Recruited")) %>% 
      group_by(OperatingUnit, fundingagency_consol, CostType) %>% 
      summarise(COPAmount = sum(COPAmount, na.rm = TRUE)) %>% 
      ungroup() %>% 
      filter(COPAmount != 0) %>% 
      group_by(OperatingUnit, fundingagency_consol) %>% 
      mutate(share = COPAmount / sum(COPAmount)) %>% 
      ungroup()

  #viz - global
    codb %>%
      filter(COP == "2018 COP",
             CostType %in% c("Institutional Contractors", 
                             "Management Meetings/Professional Development", 
                             "Peace Corps Volunteer Costs", "Staff Program Travel", 
                             "USG Staff Salaries and Benefits", 
                             "USG Staff Salaries and Benefits - Internationally  Recruited", 
                             "USG Staff Salaries and Benefits - Locally Recruited")) %>% 
      group_by(fundingagency_consol, CostType) %>% 
      summarise(COPAmount = sum(COPAmount, na.rm = TRUE)) %>% 
      ungroup() %>% 
      filter(COPAmount != 0) %>% 
      group_by(fundingagency_consol) %>% 
      mutate(share = COPAmount / sum(COPAmount),
             usaid = ifelse(fundingagency_consol == "USAID", 1, 0)) %>% 
      ungroup() %>% 
      ggplot(aes(reorder(CostType, share), share)) +
      geom_col(aes(fill = usaid), show.legend = FALSE) +
      scale_y_continuous(labels = percent) +
      scale_fill_continuous(low = "#99c2eb", high = "#2166ac") +
      coord_flip() +
      labs(x = "") +
      facet_grid(. ~ fundingagency_consol)
    
    
    
  #viz - by OU
    codb %>%
      filter(COP == "2018 COP",
             CostType %in% c("Institutional Contractors", 
                             "Management Meetings/Professional Development", 
                             "Peace Corps Volunteer Costs", "Staff Program Travel", 
                             "USG Staff Salaries and Benefits", 
                             "USG Staff Salaries and Benefits - Internationally  Recruited", 
                             "USG Staff Salaries and Benefits - Locally Recruited")) %>% 
      group_by(OperatingUnit, fundingagency_consol, CostType) %>% 
      summarise(COPAmount = sum(COPAmount, na.rm = TRUE)) %>% 
      ungroup() %>% 
      filter(COPAmount != 0) %>% 
      group_by(OperatingUnit, fundingagency_consol) %>% 
      mutate(share = COPAmount / sum(COPAmount),
             usaid = ifelse(fundingagency_consol == "USAID", 1, 0)) %>% 
      ungroup() %>% 
      ggplot(aes(reorder(OperatingUnit, share), share)) +
        geom_col(aes(fill = usaid), show.legend = FALSE) +
        scale_y_continuous(labels = percent) +
        scale_fill_continuous(low = "#99c2eb", high = "#2166ac") +
        coord_flip() +
        labs(x = "") +
        facet_grid(CostType ~ fundingagency_consol)
    
     
     