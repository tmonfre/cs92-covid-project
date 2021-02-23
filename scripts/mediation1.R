## QSS 82/83 Winter 2020
## Mediation Analysis

# Initial settings --------------------------------------------------------

library(tidyverse)
library(lubridate)
library(mediation)

# Load/wrangle data -------------------------------------------------------

dta <- read.csv("data/combined_data.csv", stringsAsFactors = TRUE) %>% 
  filter(month(date) == 4) # downsample to run some tests

med.fit <- lm(margin ~ percent_change_from_baseline +
                cases + 
                date + 
                state + 
                county + 
                mobility_label +
                lockdowns +
                popest_2019, 
              data = dta)
