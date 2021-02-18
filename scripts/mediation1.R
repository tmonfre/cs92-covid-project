## QSS 82/83 Winter 2020
## Mediation Analysis

# Initial settings --------------------------------------------------------

library(tidyverse)
library(lubridate)
library(mediation)

# Load/wrangle data -------------------------------------------------------

dta <- read.csv("data/combined_data.csv", stringsAsFactors = TRUE) %>% 
  filter(!is.na(cases),
         !is.na(margin),
         !is.na(popest_2019),
         month(date) == 4) %>% 
  mutate(margin = ifelse(party == "REP", -1 * margin, margin))

med.fit <- lm(margin ~ percent_change_from_baseline +
                cases + 
                date + 
                state + 
                county + 
                mobility_label +
                C6_Stay.at.home.requirements +
                popest_2019, 
              data = dta)
