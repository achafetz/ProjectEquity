##   PROJECT EQUITY
##   A.Chafetz, USAID
##   Purpose: munge data pulls from FACTSInfo
##   RPM 2018 follow up
##   Date: 2018.05.04
##   Updated: 


# Dependencies ------------------------------------------------------------

  library(here)
  library(tidyverse)
  library(skimr)
  library(knitr)
  library(scales)

# Open Datasets -----------------------------------------------------------

  staffing <- read_rds("Output/staffing.Rds")

# 4c. FTE's by budget code ----------------------------------------

  #table - global
    staffing %>% 
      filter(COP =="2018 COP") %>% 
      select(fundingagency_consol, ends_with("FTE")) %>% 
      select(-Intraagency_Admin__Training__Financial_Management_FTE:-External_Engagement_Technical_FTE) %>% 
      gather(budget_code, Total_FTEs, -fundingagency_consol, na.rm = TRUE) %>% 
      group_by(fundingagency_consol, budget_code) %>% 
      summarise(Total_FTEs = sum(Total_FTEs, na.rm = TRUE)) %>% 
      ungroup() %>%
      mutate(budget_code = str_remove(budget_code, "_FTE")) %>% 
      spread(fundingagency_consol, Total_FTEs) 
  
  #viz - global
    staffing %>% 
      filter(COP =="2018 COP") %>% 
      select(fundingagency_consol, ends_with("FTE")) %>% 
      select(-Intraagency_Admin__Training__Financial_Management_FTE:-External_Engagement_Technical_FTE) %>% 
      gather(budget_code, Total_FTEs, -fundingagency_consol, na.rm = TRUE) %>% 
      group_by(fundingagency_consol, budget_code) %>% 
      summarise(Total_FTEs = sum(Total_FTEs, na.rm = TRUE)) %>% 
      ungroup() %>%
      mutate(budget_code = str_remove(budget_code, "_FTE"),
             usaid = ifelse(fundingagency_consol == "USAID", 1, 0)) %>% 
      ggplot(aes(reorder(budget_code, Total_FTEs), Total_FTEs)) +
      geom_col(aes(fill = usaid), show.legend = FALSE) +
      scale_y_continuous(breaks = seq(0, 500, by = 250)) +
      scale_fill_continuous(low = "#99c2eb", high = "#2166ac") +
      coord_flip() +
      labs(y = "Total FTEs", x = "Budget Code") +
      facet_grid(. ~ fundingagency_consol)
    ggsave(here("Products", "prj_equity_q4c_fte_budget_code.png"), width = 11, height = 6, units = "in")
    