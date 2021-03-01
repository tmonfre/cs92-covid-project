## QSS 82/83 Winter 2020
## Mediation Analysis

# Initial settings --------------------------------------------------------

library(tidyverse)
library(mediation)
library(parallel)

# Load/wrangle data -------------------------------------------------------
# https://stackoverflow.com/questions/49910861/removing-comma-from-numbers-in-r
replaceCommas<-function(x){
  x<-as.numeric(gsub("\\,", "", x))
}

dta <- read.csv("data/combined_data.csv") %>% 
  filter(state %in% c("Alabama", "Wisconsin")) # downsample to run some tests

dta$popest_2019 <- sapply(dta$popest_2019, replaceCommas)

# do cases per 100,000 to factor in population
dta <- dta %>% 
  mutate(cases = (cases / popest_2019) * 100000) %>% 
  dplyr::select(-popest_2019)

dtalist <- dta %>%
  mutate(state = factor(state),) %>%
  group_split(state)

# Run models --------------------------------------------------------------

mediation_analysis <- function(dataset) {
  med.fit <- lm(margin ~ cases + 
                  involuntary +
                  date + 
                  county +
                  lockdowns, 
                data = dataset)
  
  out.fit <- lm(voluntary ~ cases +
                  margin +
                  involuntary +
                  date + 
                  county +
                  lockdowns, 
                data = dataset)
  
  med.out <- mediate(med.fit, out.fit, treat = "cases", mediator = "margin", sims = 1000, boot = TRUE)
  summary(med.out)
}

rslt <- mclapply(dtalist, mediation_analysis, mc.cores = 3)

# Run non-parallel
# mediation_analysis(dta)
