## QSS 82/83 Winter 2020
## Mediation Analysis

# Initial settings --------------------------------------------------------

library(tidyverse)
library(lubridate)
library(mediation)
library(parallel)

# Load/wrangle data -------------------------------------------------------
# https://stackoverflow.com/questions/49910861/removing-comma-from-numbers-in-r
replaceCommas<-function(x){
  x<-as.numeric(gsub("\\,", "", x))
}

dta <- read.csv("data/combined_data.csv") %>% 
  filter(month(date) == 4,
         county != "",
         state != "",
         !is.na(margin),
         !is.na(lockdowns)) # downsample to run some tests

dta$popest_2019 <- sapply(dta$popest_2019, replaceCommas)

updateFactors <- function(x) {
  return(x %>% 
           filter(!is.na(county), !is.na(date)) %>% 
           mutate(county = factor(county),
                  county = droplevels(county),
                  date = factor(date)))
}

dtalist <- dta %>% 
  mutate(state = factor(state)) %>% 
  group_split(state) %>% 
  map(updateFactors)

# Run models --------------------------------------------------------------

mediation_analysis <- function(dataset) {
  med.fit <- lm(margin ~ cases + 
                  involuntary +
                  date +
                  county +
                  lockdowns +
                  popest_2019, 
                data = dataset)
  
  out.fit <- lm(voluntary ~ cases +
                  margin +
                  involuntary +
                  date +
                  county +
                  lockdowns +
                  popest_2019,
                data = dataset)

  med.out <- mediate(med.fit, out.fit, treat = "cases", mediator = "lockdowns", sims = 1000, boot = TRUE)
  return(summary(med.out))
}

mediation_analysis(dtalist[[1]])

rslt <- mclapply(dtalist2, mediation_analysis, mc.cores = 4)

# Run non-parallel
# mediation_analysis(dta)
