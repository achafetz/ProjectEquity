##   PROJECT EQUITY
##   A.Chafetz, USAID
##   Purpose: munge data pulls from FACTSInfo
##   RPM 2018 follow up
##   Date: 2018.03.28
##   Updated: 2018.04.04


# Dependencies ------------------------------------------------------------

library(here)
library(tidyverse)
library(skimr)
library(knitr)
library(scales)

# Open Datasets -----------------------------------------------------------

codb <- read_rds("Output/codb.Rds")

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

