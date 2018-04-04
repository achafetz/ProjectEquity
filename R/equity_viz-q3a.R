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

copmatrix <- read_rds("Output/copmatrix.Rds")

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