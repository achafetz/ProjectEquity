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

# 4b. Position type by agency ---- ----------------------------------------

  # inspect
  
    staffing %>% 
      count(Position_Type, wt = Total_FTEs, sort = TRUE)
  
  
  #table - global
    staffing %>% 
      filter(COP =="2018 COP",
             Position_Type != "Unknown") %>% 
      group_by(fundingagency_consol, COP, Position_Type) %>% 
      summarise(Total_FTEs = sum(Total_FTEs, na.rm = TRUE)) %>% 
      ungroup() %>%
      select(-COP) %>% 
      spread(Position_Type, Total_FTEs) 
    
  #viz - global
    staffing %>% 
      filter(COP =="2018 COP",
             Position_Type != "Unknown") %>% 
      group_by(fundingagency_consol, COP, Position_Type) %>% 
      summarise(Total_FTEs = sum(Total_FTEs, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(usaid = ifelse(fundingagency_consol == "USAID", 1, 0)) %>% 
      ggplot(aes(reorder(Position_Type, Total_FTEs), Total_FTEs)) +
       geom_col(aes(fill = usaid), show.legend = FALSE) +
       scale_y_continuous(labels = comma) +
       scale_fill_continuous(low = "#99c2eb", high = "#2166ac") +
       coord_flip() +
       labs(x = "") +
       facet_grid(. ~ fundingagency_consol)
    ggsave(here("Products", "prj_equity_q4b_pos_type.png"), width = 11, height = 6, units = "in")