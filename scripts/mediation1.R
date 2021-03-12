## QSS 82/83 Winter 2020
## Mediation Analysis

# Initial settings --------------------------------------------------------

library(tidyverse)
# library(lubridate) # Uncomment when limiting by date
library(mediation)
library(parallel)

# Load/wrangle data -------------------------------------------------------
# https://stackoverflow.com/questions/49910861/removing-comma-from-numbers-in-r
replaceCommas<-function(x){
  x<-as.numeric(gsub("\\,", "", x))
}

dta <- read.csv("data/combined_data.csv") # %>% 
  # filter(month(date) %in% c(4:5)) # downsample to run some tests

dta$popest_2019 <- sapply(dta$popest_2019, replaceCommas)

cols <- c("date", "state", "county", "margin", "voluntary", "involuntary", "cases", "lockdowns")

cleanUpData <- function(x) {
  x_factored <- x %>% 
    filter(!is.na(county), !is.na(date)) %>% 
    mutate(county = factor(county),
           county = droplevels(county),
           date = factor(date))
  
  county_counts <- x %>% 
    group_by(county) %>% 
    mutate(ccount = n()) %>% 
    ungroup() %>% 
    filter(ccount >= 10)
  
  date_counts <- x %>% 
    group_by(date) %>% 
    mutate(dcount = n()) %>% 
    ungroup() %>% 
    filter(dcount >= 10)
  
  x_dta <- county_counts %>% 
    inner_join(date_counts, by=cols) %>% 
    group_by(county) %>% 
    mutate(ccount = n()) %>% 
    ungroup() %>% 
    group_by(date) %>% 
    mutate(dcount = n()) %>% 
    ungroup() %>% 
    filter(ccount >= 10 & dcount >= 10) %>% 
    dplyr::select(all_of(cols))
  
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

# Run models --------------------------------------------------------------

mediation_analysis <- function(dataset) {
  med.fit <- lm(lockdowns ~ cases + 
                  involuntary +
                  date +
                  county +
                  margin, 
                data = dataset)
  
  out.fit <- lm(voluntary ~ cases +
                  margin +
                  involuntary +
                  date +
                  county +
                  lockdowns,
                data = dataset)

  med.out <- mediate(med.fit, out.fit, treat = "cases", mediator = "lockdowns", sims = 10, boot = TRUE)
  return(summary(med.out))
}

mediation_analysis(dtalist[[1]])

# Run parallel mediation analysis
rslt <- mclapply(dtalist, mediation_analysis, mc.cores = 3)

# Loop through and analyze 1 by 1
rslt_looped <- list()
for (idx in c(1:length(dtalist))) {
  print(paste0('running analysis on subset ', idx))
  rslt_looped[[idx]] <- mediation_analysis(dtalist[[idx]])
}

rslt_looped1 <- list()
for (idx in c(1:20)) {
  print(paste0('running analysis on subset ', idx))
  rslt_looped1[[idx]] <- mediation_analysis(dtalist[[idx]])
}

rslt_looped2 <- list()
for (idx in c(20:40)) {
  print(paste0('running analysis on subset ', idx))
  rslt_looped2[[idx]] <- mediation_analysis(dtalist[[idx]])
}

# Run non-parallel
# mediation_analysis(dta)