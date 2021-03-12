## QSS 82/83 Winter 2020
## Mediation Analysis

# Initial settings --------------------------------------------------------

library(tidyverse)
library(lubridate)
library(mediation)
library(parallel) # Only if necessary.
library(lme4) # IF we do random effects. 
library(ggthemes)
library(gridExtra)


full_dta <- read.csv("data/combined_data.csv")

dat2 <- full_dta %>%
  mutate(pop19 = parse_number(popest_2019),
         cs_p1k = (cases/pop19)*100000,
         state = factor(state),
         month = month(date))

ggplot(dat2) + 
  geom_point(aes(cs_p1k, involuntary))

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
# hist(dat2$cs_p1k) # heavily skewed, right tail. 
# hist(log(dat2$cs_p1k)) # more normal-ish. Maybe even a different transform.
# hist(dat2$involuntary) # sharply normal, negative mean. 
# hist(dat2$lockdowns) # Which Oxford index pieces go into this?

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
out.fit.re3 <- lmer(voluntary ~ log(cs_p1k) + margin + involuntary + lockdowns + (1|county), data = dat2)

summary(out.fit.re3)
summary(med.fit.re3)

med.out.re <- mediate(med.fit.re3, out.fit.re3, treat = "log(cs_p1k)", mediator = "margin", sims = 1000)

# Analyzing p-values ------------------------------------------------------

mediation_results <- data.frame(state = fulldat_split[[1]][1, 'state'], 
                                ACME = c(rslt[[1]][[3]]['d0']),
                                ACME.p = c(rslt[[1]][[3]]['d0.p']),
                                ADE = c(rslt[[1]][[3]]["z0"]),
                                ADE.p = c(rslt[[1]][[3]]["z0.p"]),
                                Total = c(rslt[[1]][[3]]["tau.coef"]),
                                Total.p = c(rslt[[1]][[3]]["tau.p"]))

# sens <- list()
# for (idx in c(1:length(fulldat_split))) {
  # print(sens[[idx]][['rho.by']])
# }

for (idx in c(2:length(fulldat_split))) {
  temp <- data.frame(state = fulldat_split[[idx]][1, 'state'], 
                     ACME = c(rslt[[idx]][[3]]['d0']),
                     ACME.p = c(rslt[[idx]][[3]]['d0.p']),
                     ADE = c(rslt[[idx]][[3]]["z0"]),
                     ADE.p = c(rslt[[idx]][[3]]["z0.p"]),
                     Total = c(rslt[[idx]][[3]]["tau.coef"]),
                     Total.p = c(rslt[[idx]][[3]]["tau.p"]))
  mediation_results <- rbind(mediation_results, temp)
}

## Average, weighted for each state's sample size
## Example of partial mediation, full mediation, no mediation
# 1 table at most, subset of mediation outputs (average ACME, average ADE, prop mediated, total effect)

mediation_results <- mediation_results %>% 
  rename(ACME = d0,
         ACME.p = d0.p,
         ADE = z0,
         ADE.p = z0.p,
         Total = tau.coef,
         Total.p = tau.p)


quartiles_acme <- quantile(mediation_results$ACME)
iqr_acme <- quartiles_acme[4] - quartiles_acme[2]
lower_fence <- quartiles_acme[2] - (1.5 * iqr_acme)
upper_fence <- quartiles_acme[4] + (1.5 * iqr_acme)

ggplot(mediation_results) +
  geom_boxplot(aes(ACME), fill="#bccbcb") +
  scale_y_continuous(labels = c()) +
  labs(title = "Average Causal Mediation Effect",
       x = "") +
  guides(fill=FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 18))

ggplot(mediation_results) +
  geom_boxplot(aes(ACME), fill="#bccbcb", outlier.shape = NA) + 
  scale_x_continuous(limits = c(lower_fence, upper_fence)) + 
  scale_y_continuous(labels = c()) +
  labs(title = "Average Causal Mediation Effect, Outliers Removed",
       x = "") +
  theme_minimal() +
  theme(text = element_text(size = 18))

ggplot(mediation_results) +
  geom_boxplot(aes(ADE), fill="#bccbcb") +
  scale_y_continuous(labels = c()) +
  labs(title = "Average Direct Effect",
       x = "") +
  guides(fill=FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 18))

ggplot(mediation_results) +
  geom_boxplot(aes(ADE), fill="#bccbcb", outlier.shape = NA) + 
  scale_x_continuous(limits = c(lower_fence, upper_fence)) + 
  scale_y_continuous(labels = c()) +
  labs(title = "Average Direct Effect, Outliers Removed",
       x = "") +
  theme_minimal() +
  theme(text = element_text(size = 18))
