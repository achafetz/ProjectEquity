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

  ea <- read_rds("Output/ea.Rds")

# 4c. FTE's by budget code ----------------------------------------
  
  #table - global
    ea %>% 
      group_by(fundingagency_consol, maj_cc) %>% 
      summarize(fy2017_expenditure = sum(fy2017_expenditure, na.rm = TRUE)) %>% 
      ungroup() %>% 
      spread(fundingagency_consol, fy2017_expenditure) 
  
  
    
  #viz - global
    ea %>% 
      group_by(fundingagency_consol, maj_cc) %>% 
      summarize(fy2017_expenditure = sum(fy2017_expenditure, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(fy2017_expenditure = fy2017_expenditure/1000000000,
               usaid = ifelse(fundingagency_consol == "USAID", 1, 0)) %>% 
      ggplot(aes(maj_cc, fy2017_expenditure)) +
        geom_col(aes(fill = usaid), show.legend = FALSE) +
        scale_y_continuous(breaks = seq(0, 1.2, by = .4), labels = comma) +
        scale_fill_continuous(low = "#99c2eb", high = "#2166ac") +
        coord_flip() +
        labs(y = "FY17 Expenditures (bil, USD)", x = "") +
        facet_grid(. ~ fundingagency_consol)
    ggsave(here("Products", "prj_equity_q4c_exp.png"), width = 11, height = 6, units = "in")