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
  # filter(month(date) %in% c(4:5)) # downsample to run some tests

dta$popest_2019 <- sapply(dta$popest_2019, replaceCommas)

cleanUpData <- function(x) {
  
  county_counts <- x %>% 
    group_by(county) %>% 
    summarize(ccount = n()) %>% 
    ungroup()
  
  date_counts <- x %>% 
    group_by(date) %>% 
    summarize(dcount = n()) %>% 
    ungroup()
  
  x_dta <- x %>% 
    left_join(county_counts, by="county") %>% 
    left_join(date_counts, by="date") %>% 
    filter(ccount >= 10,
           dcount >= 10) %>% 
    dplyr::select(county,
                  state,
                  voluntary,
                  margin,
                  cases,
                  involuntary,
                  date,
                  lockdowns)
  
  return(x_dta)
}

factorData <- function(x) {
  x_factored <- x %>% 
    filter(!is.na(county), !is.na(date)) %>% 
    mutate(county = factor(county),
           county = droplevels(county),
           date = factor(date))
  
  return(x_factored)
}

# do cases per 100,000 to factor in population
dta <- dta %>% 
  mutate(cases = (cases / popest_2019) * 100000) %>% 
  dplyr::select(-popest_2019)

zeroRows <- function(x) {
  return(nrow(x) == 0)
}

dtalist <- dta %>% 
  mutate(state = factor(state)) %>% 
  # filter(state %in% c("Alabama", "Wisconsin")) %>% # downsample to run some tests
  group_split(state) %>%
  map(cleanUpData) %>% # Get rid of small county/date counts
  purrr::discard(zeroRows) %>% # Get rid of empty tibbles
  map(factorData) # Properly factor the counties and dates based on the statewide data

# dtalist <- dta %>%
#   mutate(state = factor(state),) %>%
#   group_split(state)

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

  med.out <- mediate(med.fit, out.fit, treat = "cases", mediator = "margin", sims = 100, boot = TRUE)
  return(summary(med.out))
}

# Run parallel mediation analysis
rslt <- mclapply(dtalist, mediation_analysis, mc.cores = 4)


# Run non-parallel
# mediation_analysis(dta)