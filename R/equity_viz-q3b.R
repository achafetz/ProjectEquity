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
      select(-TotalPlannedAmountandAppliedPipelineAmount) %>% 
      spread(fundingagency_consol, share) %>% 
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
      labs(x = "") +
      coord_flip() +
      facet_grid(. ~ fundingagency_consol)
    ggsave(here("Products", "prj_equity_q3b_budget_moshare.png"), width = 11, height = 6, units = "in")
    