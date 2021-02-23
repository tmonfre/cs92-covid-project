## QSS 82/83 Winter 2020
## Mediation Analysis

# Initial settings --------------------------------------------------------

library(tidyverse)
library(lubridate)
library(mediation)
library(parallel)


# Set up parallel computing -----------------------------------------------

parallel::mclapply()

# Load/wrangle data -------------------------------------------------------
# https://stackoverflow.com/questions/49910861/removing-comma-from-numbers-in-r
replaceCommas<-function(x){
  x<-as.numeric(gsub("\\,", "", x))
}

dta <- read.csv("data/combined_data.csv") 

dta2 <- dta# %>% 
  # mutate(state = factor(state),
  #        county = factor(county),
  #        margin = as.factor(margin),
  #        mobility_label = factor(mobility_label)) %>%
  # filter(month(date) == 4) #%>%  # downsample to run some tests
  # mutate(date = factor(date))

dta$popest_2019 <- sapply(dta2$popest_2019, replaceCommas)


dtalist <- dta2 %>% 
  mutate(fact_state = factor(state)) %>% 
  group_split(fact_state)

# Run models --------------------------------------------------------------

medfit <- function(dataset) {
  med.fit <- lm(margin ~ percent_change_from_baseline +
                  cases + 
                  date + 
                  # state + 
                  county +
                  mobility_label +
                  lockdowns +
                  popest_2019, 
                data = dataset)
  summary(med.fit)
}

rslt <- mclapply(dtalist, medfit, mc.cores = 3)

# Run non-parallel
# medfit(dta)
