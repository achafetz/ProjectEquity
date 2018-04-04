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