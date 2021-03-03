## QSS 82/83 Winter 2020
## Mediation Analysis

# Initial settings --------------------------------------------------------

library(tidyverse)
library(lubridate)
library(mediation)
library(parallel) # Only if necessary.
library(lme4) # IF we do random effects. 


full_dta <- read.csv("data/combined_data.csv")

dat2 <- full_dta %>%
  mutate(pop19 = parse_number(popest_2019),
         cs_p1k = (cases/pop19)*100000,
         state = factor(state),
         month = month(date))


# CURRENT ISSUES:
# In your code, mediator model basically only has cross-sectional variation.
# Currently you have time and county fixed effects. 
# Lockdowns and popest have dropped out. 
# Solution 1: Adjust population to population per 100000. Drop popest.
# Solution 2: We may want to consider a transformation of cases variable.
# Solution 3: If we are considered

##########
# Some descriptives and plots of variables. 

hist(dat2$voluntary)
hist(dat2$cs_p1k) # heavily skewed, right tail. 
hist(log(dat2$cs_p1k)) # more normal-ish. Maybe even a different transform.
hist(dat2$involuntary) # sharply normal, negative mean. 
hist(dat2$lockdowns) # Which Oxford index pieces go into this?

# Check our basic OLS models to make sure they work. 

med.fit <- lm(margin ~ cs_p1k+ 
                involuntary +
                lockdowns,
              data = dat2)


out.fit <- lm(voluntary ~ margin + 
                log(cs_p1k) + 
                involuntary +
                lockdowns,
              data = dat2)

summary(med.fit)
summary(out.fit)

hist(med.fit$residuals)
hist(out.fit$residuals)

# Also not a bad idea to do some cross-validation.
# Take random samples of 1-2000 from the data and re-estimate everything. 


##################################
# Split data into list. 

fulldat_split <- dat2 %>%
  group_by(state) %>%
  group_split()


# Create our own function for the 2 models and mediation analysis. 

med_models <- function(list){
  med.fit <- lm(margin ~ cs_p1k+ 
                  involuntary +
                  lockdowns,
                data = list)
  
  out.fit <- lm(voluntary ~ cs_p1k +
                  margin +
                  involuntary +
                  lockdowns,
                data = list)
  
  med.out <- mediate(med.fit, out.fit, treat = "cs_p1k", 
                     mediator = "margin", sims = 1000, boot = TRUE)
  
  outputs <- list(summary(med.fit), summary(out.fit), summary(med.out))
  print(outputs)
  return(outputs)
}

# Now try to 'map' function to list. Of just 2 states, AL and AR.
# The map() function applies a function to a list, like lapply. 

library(tictoc)

tic()
rslt <- fulldat_split %>%
  map(., med_models) # This works. 
toc() # 1260.281 sec elapsed. 21 minutes, approx. 


##################################
# EXAMPLE (re could be time or state/county)
# One option: add random effects. IF we want to control for confounding variables 
# ... affecting change in time or state/county. 

med.fit.re  <- lmer(margin ~ log(cs_p1k) + involuntary + lockdowns + (1|date) , data = dat2)
out.fit.re <- lmer(voluntary ~ log(cs_p1k) + involuntary + lockdowns + (1|date), data = dat2)

med.fit.re2  <- lmer(margin ~ log(cs_p1k) + involuntary + lockdowns + (1|state) , data = dat2)
out.fit.re2 <- lmer(voluntary ~ log(cs_p1k) + involuntary + lockdowns + (1|state), data = dat2)

med.fit.re3  <- lmer(margin ~ log(cs_p1k) + involuntary + lockdowns + (1|county) , data = dat2)
out.fit.re3 <- lmer(voluntary ~ log(cs_p1k) + involuntary + lockdowns + (1|county), data = dat2)

summary(out.fit.re)
summary(med.fit.re)


# Analyzing p-values ------------------------------------------------------

mediation_results <- data.frame("state" = fulldat_split[[1]][1, 'state'], 
                                "ACME" = c(rslt[[1]][[3]]['d0']),
                                "ACME.p" = c(rslt[[1]][[3]]['d0.p']),
                                "ADE" = c(rslt[[1]][[3]]["z0"]),
                                "ADE.p" = c(rslt[[1]][[3]]["z0.p"]))
for (idx in c(2:length(fulldat_split))) {
  temp <- data.frame("state" = fulldat_split[[idx]][1, 'state'], 
                     "ACME" = c(rslt[[idx]][[3]]['d0']),
                     "ACME.p" = c(rslt[[idx]][[3]]['d0.p']),
                     "ADE" = c(rslt[[idx]][[3]]["z0"]),
                     "ADE.p" = c(rslt[[idx]][[3]]["z0.p"]))
  mediation_results <- rbind(mediation_results, temp)
}

mediation_results <- mediation_results %>% 
  rename(ACME = d0,
         ACME.p = d0.p,
         ADE = z0,
         ADE.p = z0.p)

ggplot(mediation_results) +
  geom_boxplot(aes(ACME))

quartiles_acme <- quantile(mediation_results$ACME)
iqr_acme <- quartiles_acme[4] - quartiles_acme[2]
lower_fence <- quartiles_acme[2] - (1.5 * iqr_acme)
upper_fence <- quartiles_acme[4] + (1.5 * iqr_acme)

ggplot(mediation_results) +
  geom_boxplot(aes(ACME), outlier.shape = NA) + 
  scale_x_continuous(limits = c(lower_fence, upper_fence))
