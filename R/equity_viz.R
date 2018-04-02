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
    staffing %>% skim(fundingagency_consol, Employment_Type)

staffing %>% 
  count(fundingagency_consol, sort = TRUE) %>% 
  kable(format.args = list(big.mark = ",", zero.print = FALSE))


  staffing %>% 
    count(fundingagency_consol, Employment_Type) %>% 
    spread(Employment_Type, n) %>% 
    kable(format.args = list(big.mark = ",", zero.print = FALSE))

  staffing %>% 
    count(fundingagency_consol, Employment_Type) %>% 
    ggplot(aes(reorder(Employment_Type, n), n)) +
      geom_col() +
      coord_flip() +
      labs(x = "", y = "# of staff") +
      scale_y_continuous(labels = comma)
  
  staffing %>% 
    count(fundingagency_consol, Employment_Type) %>%
    mutate(usaid = ifelse(fundingagency_consol == "USAID", 1, 0)) %>% 
    ggplot(aes(reorder(Employment_Type, n), n)) +
      geom_col(aes(fill = usaid), show.legend = FALSE) +
      scale_fill_continuous(low = "#99c2eb", high = "#2166ac") +
      coord_flip() +
      labs(x = "", y = "# of staff") +
      scale_y_continuous(labels = comma) +
      facet_grid(. ~ fundingagency_consol)
  
  ggsave(here("Products", "prj_equity_q1_staffing_cat_a.png"), width = 11, height = 2.5, units = "in")

  staffing %>% 
    count(fundingagency_consol, Employment_Type) %>% 
    mutate(usaid = ifelse(fundingagency_consol == "USAID", 1, 0)) %>% 
    ggplot(aes(reorder(fundingagency_consol, n), n)) +
      geom_col(aes(fill = usaid), show.legend = FALSE) +
      scale_fill_continuous(low = "#99c2eb", high = "#2166ac") +
      coord_flip() +
      labs(x = "", y = "# of staff") +
      scale_y_continuous(labels = comma) +
      facet_grid(. ~ Employment_Type)
  
  ggsave(here("Products", "prj_equity_q1_staffing_cat_b.png"), width = 11, height = 2.5, units = "in")

  
staffing %>% 
  count(fundingagency_consol, Position_Type) %>% 
  spread(Position_Type, n) %>% 
  kable(format.args = list(big.mark = ",", zero.print = FALSE))
