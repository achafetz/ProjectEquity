##   PROJECT EQUITY
##   A.Chafetz, USAID
##   Purpose: munge data pulls from FACTSInfo
##   RPM 2018 follow up
##   Date: 2018.05.03
##   Updated: 


# Dependencies ------------------------------------------------------------

library(here)
library(tidyverse)
library(skimr)
library(knitr)
library(scales)

# Open Datasets -----------------------------------------------------------

staffing <- read_rds("Output/staffing.Rds")

# 4a. FTE type trend (local vs international) -----------------------------

# inspect
  
  staffing %>% 
    count(COP, wt = Total_FTEs, sort = TRUE)
  staffing %>% 
    count(Employment_Type, wt = Total_FTEs, sort = TRUE)
  staffing %>% 
    count(Staffing_Status, wt = Total_FTEs, sort = TRUE)
  staffing %>% 
    count(Funding_Type, wt = Total_FTEs, sort = TRUE)
  staffing %>% 
    count(Citizenship_Type, wt = Total_FTEs, sort = TRUE)
  staffing %>% 
    count(Position_Type, wt = Total_FTEs, sort = TRUE)


  #table - global
    staffing %>% 
      filter(Citizenship_Type %in% c("Host Country National (or legal permanent resident)", "Internationally Recruited Third Country National")) %>% 
      mutate(Citizenship_Type = ifelse(Citizenship_Type == "Host Country National (or legal permanent resident)", "FSN", "TCN")) %>% 
      group_by(fundingagency_consol, COP, Citizenship_Type) %>% 
      summarise(Total_FTEs = sum(Total_FTEs, na.rm = TRUE)) %>% 
      ungroup() %>%
      spread(COP, Total_FTEs) %>% 
      arrange(Citizenship_Type, fundingagency_consol)
    
  #viz - global
    staffing %>% 
      filter(Citizenship_Type %in% c("Host Country National (or legal permanent resident)", "Internationally Recruited Third Country National")) %>% 
      mutate(Citizenship_Type = ifelse(Citizenship_Type == "Host Country National (or legal permanent resident)", "FSN", "TCN")) %>% 
      group_by(fundingagency_consol, COP, Citizenship_Type) %>% 
      summarise(Total_FTEs = sum(Total_FTEs, na.rm = TRUE)) %>% 
      ungroup() %>%
      mutate(COP = str_sub(COP, 3,4),
             usaid = ifelse(fundingagency_consol == "USAID", 1, 0)) %>% 
      ggplot(aes(COP, Total_FTEs, fill = usaid)) +
        geom_col(show.legend = FALSE) +
        labs(x = "COP", y = "FTEs") +
        scale_y_continuous(labels = comma) +
        scale_fill_continuous(low = "#99c2eb", high = "#2166ac") +
        facet_grid(Citizenship_Type ~ fundingagency_consol)
    ggsave(here("Products", "prj_equity_q4a_ftes.png"), width = 11, height = 6, units = "in")
