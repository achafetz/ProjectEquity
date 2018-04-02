##   PROJECT EQUITY
##   A.Chafetz, USAID
##   Purpose: munge data pulls from FACTSInfo
##   RPM 2018 follow up
##   Date: 2018.03.28


# Dependencies ------------------------------------------------------------

library(tidyverse)
library(skimr)
library(knitr)
library(scales)

# M&O Categories ----------------------------------------------------------

# 1. We need broad M&O categories for all agencies individually (not just CDC/USAID comparisons)

  #inspect
    staffing %>% skim(fundingagency_consol, Employment_Type, Position_Type)

  #table - overall
    staffing %>% 
      filter(Cycle == "2018 COP") %>% 
      count(fundingagency_consol, sort = TRUE) %>% 
      kable(format.args = list(big.mark = ",", zero.print = FALSE))
  #table - overall, over time
    staffing %>% 
      count(fundingagency_consol, Cycle, sort = TRUE) %>% 
      spread(Cycle, n) %>% 
      kable(format.args = list(big.mark = ",", zero.print = FALSE))
    
  #table - by employment type 
    staffing %>% 
      count(fundingagency_consol, Employment_Type) %>% 
      spread(Employment_Type, n) %>% 
      kable(format.args = list(big.mark = ",", zero.print = FALSE))
  #table - by position type
    staffing %>% 
      count(fundingagency_consol, Position_Type) %>% 
      spread(fundingagency_consol, n) %>% 
      kable(format.args = list(big.mark = ",", zero.print = FALSE))
    
  #viz - overall  
    staffing %>% 
      count(fundingagency_consol, Employment_Type) %>% 
      ggplot(aes(reorder(Employment_Type, n), n)) +
        geom_col() +
        coord_flip() +
        labs(x = "", y = "# of staff") +
        scale_y_continuous(labels = comma)
    
   #viz - by employment type
    staffing %>% 
      count(fundingagency_consol, Employment_Type) %>%
      mutate(fundingagency_ordered = factor(fundingagency_consol, levels = c("USAID", "HHS/CDC", "PC", "State", "DOD", "HHS/Other")),
             usaid = ifelse(fundingagency_consol == "USAID", 1, 0)) %>% 
      ggplot(aes(reorder(Employment_Type, n), n)) +
        geom_col(aes(fill = usaid), show.legend = FALSE) +
        scale_fill_continuous(low = "#99c2eb", high = "#2166ac") +
        coord_flip() +
        labs(x = "", y = "# of staff") +
        scale_y_continuous(labels = comma) +
        facet_grid(. ~ fundingagency_ordered)
     ggsave(here("Products", "prj_equity_q1_empl_type.png"), width = 11, height = 2.5, units = "in")
    
    #viz - by position type
     staffing %>% 
       count(fundingagency_consol, Position_Type) %>%
       mutate(fundingagency_ordered = factor(fundingagency_consol, levels = c("USAID", "HHS/CDC", "PC", "State", "DOD", "HHS/Other")),
              usaid = ifelse(fundingagency_consol == "USAID", 1, 0)) %>% 
       ggplot(aes(reorder(Position_Type, n), n)) +
       geom_col(aes(fill = usaid), show.legend = FALSE) +
       scale_fill_continuous(low = "#99c2eb", high = "#2166ac") +
       coord_flip() +
       labs(x = "", y = "# of staff") +
       scale_y_continuous(labels = comma) +
       facet_grid(. ~ fundingagency_ordered)
     ggsave(here("Products", "prj_equity_q1_position_type.png"), width = 11, height = 2.5, units = "in")
  
# 2. IM: We need better understanding of # of IMs managed by different agencies, by OU  

  

