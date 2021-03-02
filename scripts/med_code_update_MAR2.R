## QSS 82/83 Winter 2020
## Mediation Analysis

# Initial settings --------------------------------------------------------

library(tidyverse)
library(lubridate)
library(mediation)
library(parallel) # Only if necessary.
library(lme4) # IF we do random effects. 
library(ggthemes)


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

# hist(dat2$voluntary)
ggplot(data = dat2, mapping = aes(x = voluntary)) +
  geom_density() +
  labs(x = "Voluntary Mobility", y = "Density", title = "Density Plot of Voluntary Mobility") +
  theme_grey()

# hist(dat2$cs_p1k) # heavily skewed, right tail. 
ggplot(data = dat2, mapping = aes(x = cs_p1k)) +
  geom_density() +
  labs(x = "COVID-19 Cases Per 100,000", y = "Density", title = "Density Plot of COVID-19 Cases Per 100,000") +
  theme_grey()

# hist(log(dat2$cs_p1k)) # more normal-ish. Maybe even a different transform.
ggplot(data = dat2, mapping = aes(x = log(cs_p1k))) +
  geom_density() +
  labs(x = "Log COVID-19 Cases Per 100,000", y = "Density", title = "Density Plot of Log COVID-19 Cases Per 100,000") +
  theme_grey()

# hist(dat2$involuntary) # sharply normal, negative mean. 
ggplot(data = dat2, mapping = aes(x = involuntary)) +
  geom_density() +
  labs(x = "Involuntary Mobility", y = "Density", title = "Density Plot of Involuntary Mobility") +
  theme_grey()

# hist(dat2$lockdowns) # Which Oxford index pieces go into this?
table(dat2$lockdowns)

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

n_samples = 500
sample_size = 2000

cs_p1k_med_sum <- 0
margin_out_sum <- 0

for (i in 1:n_samples) {
  sample_data <- dat2[sample(nrow(dat2), sample_size),]
  
  sample_med.fit <- lm(margin ~ cs_p1k + 
                  involuntary +
                  lockdowns,
                data = sample_data)
  
  
  sample_out.fit <- lm(voluntary ~ margin + 
                  log(cs_p1k) + 
                  involuntary +
                  lockdowns,
                data = sample_data)
  
  summary(sample_med.fit) # cs_p1k
  summary(sample_out.fit) # margin
  
  cs_p1k_med_sum <- cs_p1k_med_sum + summary(sample_med.fit)$coefficients[[2]]
  margin_out_sum <- margin_out_sum + summary(sample_out.fit)$coefficients[[2]]
}

print(paste0("Point-estimate comparison of cases per 1k, between med.fit on entire dataset, and ", n_samples, " subsamples each of size ", sample_size))
print(paste0("Entire dataset: ", summary(med.fit)$coefficients[[2]]))
print(paste0("Avg of samples: ", cs_p1k_med_sum / n_samples))

print("##################################")

print(paste0("Point-estimate comparison of partisanship margin, between out on entire dataset, and ", n_samples, " subsamples each of size ", sample_size))
print(paste0("Entire dataset: ", summary(out.fit)$coefficients[[2]]))
print(paste0("Avg of samples: ", margin_out_sum / n_samples))


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
}

# Now try to 'map' function to list. Of just 2 states, AL and AR.
# The map() function applies a function to a list, like lapply. 

library(tictoc)

tic()
fulldat_split %>%
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

