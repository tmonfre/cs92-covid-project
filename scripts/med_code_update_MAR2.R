## QSS 82/83 Winter 2020
## Mediation Analysis

# Initial settings --------------------------------------------------------

library(tidyverse)
library(lubridate)
library(mediation)
library(parallel) # Only if necessary.
library(lme4) # IF we do random effects. 
library(ggthemes)
library(cowplot)


full_dta <- read.csv("data/combined_data.csv")

dat2 <- full_dta %>%
  mutate(pop19 = parse_number(popest_2019),
         cs_p1k = (cases/pop19)*100000,
         state = factor(state),
         month = month(date))

dta_pres <- dat2 %>% 
  dplyr::select(-margin_senate) %>% 
  rename(margin = margin_election)

dta_senate <- dat2 %>% 
  dplyr::select(-margin_election) %>% 
  rename(margin = margin_senate)


# Descriptives and plots of variables -------------------------------------


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

rm(dat2)

# Check our basic OLS models to make sure they work.  ---------------------

# Presidential data
med.fit <- lm(margin ~ cs_p1k+ 
                involuntary +
                lockdowns,
              data = dta_pres)


out.fit <- lm(voluntary ~ margin + 
                log(cs_p1k) + 
                involuntary +
                lockdowns,
              data = dta_pres)

summary(med.fit)
summary(out.fit)

hist(med.fit$residuals)
hist(out.fit$residuals)

# Senate data
med.fit <- lm(margin ~ cs_p1k+ 
                involuntary +
                lockdowns,
              data = dta_pres)


out.fit <- lm(voluntary ~ margin + 
                log(cs_p1k) + 
                involuntary +
                lockdowns,
              data = dta_pres)

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
  sample_data <- dta_pres[sample(nrow(dta_pres), sample_size),]
  
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


# Split data into list ----------------------------------------------------

pres_split <- dta_pres %>%
  group_by(state) %>%
  group_split()

senate_split <- dta_senate %>%
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
rslt_pres <- pres_split %>%
  map(., med_models) # This works. 
toc() # 1260.281 sec elapsed. 21 minutes, approx. 


tic()
rslt_senate <- senate_split %>%
  map(., med_models) # This works. 
toc() # On Joseph's computer, took approx. 30-40 minutes 

##################################
# EXAMPLE (re could be time or state/county)
# One option: add random effects. IF we want to control for confounding variables 
# ... affecting change in time or state/county. 

med.fit.re  <- lmer(margin ~ log(cs_p1k) + involuntary + lockdowns + (1|date) , data = dta_pres)
out.fit.re <- lmer(voluntary ~ log(cs_p1k) + involuntary + lockdowns + (1|date), data = dta_pres)

med.fit.re2  <- lmer(margin ~ log(cs_p1k) + involuntary + lockdowns + (1|state) , data = dta_pres)
out.fit.re2 <- lmer(voluntary ~ log(cs_p1k) + involuntary + lockdowns + (1|state), data = dta_pres)

med.fit.re3  <- lmer(margin ~ log(cs_p1k) + involuntary + lockdowns + (1|county) , data = dta_pres)
out.fit.re3 <- lmer(voluntary ~ log(cs_p1k) + margin + involuntary + lockdowns + (1|county), data = dta_pres)

summary(out.fit.re3)
summary(med.fit.re3)

med.out.re <- mediate(med.fit.re3, out.fit.re3, treat = "log(cs_p1k)", mediator = "margin", sims = 1000)

# Analyzing p-values ------------------------------------------------------

output_pres <- data.frame(state = pres_split[[1]][1, 'state'],
                          size = nrow(pres_split[[1]]),
                          ACME = c(rslt_pres[[1]][[3]]['d0']),
                          ACME.p = c(rslt_pres[[1]][[3]]['d0.p']),
                          ADE = c(rslt_pres[[1]][[3]]["z0"]),
                          ADE.p = c(rslt_pres[[1]][[3]]["z0.p"]),
                          Total = c(rslt_pres[[1]][[3]]["tau.coef"]),
                          Total.p = c(rslt_pres[[1]][[3]]["tau.p"]),
                          prop_med = c(rslt_pres[[idx]][[3]]["n0"]),
                          prop_med.p = c(rslt_pres[[idx]][[3]]["n0.p"]))

output_senate <- data.frame(state = senate_split[[1]][1, 'state'],
                            size = nrow(senate_split[[1]]),
                            ACME = c(rslt_senate[[1]][[3]]['d0']),
                            ACME.p = c(rslt_senate[[1]][[3]]['d0.p']),
                            ADE = c(rslt_senate[[1]][[3]]["z0"]),
                            ADE.p = c(rslt_senate[[1]][[3]]["z0.p"]),
                            Total = c(rslt_senate[[1]][[3]]["tau.coef"]),
                            Total.p = c(rslt_senate[[1]][[3]]["tau.p"]),
                            prop_med = c(rslt_senate[[1]][[3]]["n0"]),
                            prop_med.p = c(rslt_senate[[1]][[3]]["n0.p"]))

# sens <- list()
# for (idx in c(1:length(fulldat_split))) {
  # print(sens[[idx]][['rho.by']])
# }

for (idx in c(2:length(pres_split))) {
  temp_p <- data.frame(state = pres_split[[idx]][1, 'state'], 
                       size = nrow(pres_split[[idx]]),
                       ACME = c(rslt_pres[[idx]][[3]]['d0']),
                       ACME.p = c(rslt_pres[[idx]][[3]]['d0.p']),
                       ADE = c(rslt_pres[[idx]][[3]]["z0"]),
                       ADE.p = c(rslt_pres[[idx]][[3]]["z0.p"]),
                       Total = c(rslt_pres[[idx]][[3]]["tau.coef"]),
                       Total.p = c(rslt_pres[[idx]][[3]]["tau.p"]),
                       prop_med = c(rslt_pres[[idx]][[3]]["n0"]),
                       prop_med.p = c(rslt_pres[[idx]][[3]]["n0.p"]))
  output_pres <- rbind(output_pres, temp_p)
  
  temp_s <- data.frame(state = senate_split[[idx]][1, 'state'],
                       size = nrow(senate_split[[idx]]),
                       ACME = c(rslt_senate[[idx]][[3]]['d0']),
                       ACME.p = c(rslt_senate[[idx]][[3]]['d0.p']),
                       ADE = c(rslt_senate[[idx]][[3]]["z0"]),
                       ADE.p = c(rslt_senate[[idx]][[3]]["z0.p"]),
                       Total = c(rslt_senate[[idx]][[3]]["tau.coef"]),
                       Total.p = c(rslt_senate[[idx]][[3]]["tau.p"]),
                       prop_med = c(rslt_senate[[idx]][[3]]["n0"]),
                       prop_med.p = c(rslt_senate[[idx]][[3]]["n0.p"]))
  
  output_senate <- rbind(output_senate, temp_s)
}

## Average, weighted for each state's sample size
## Example of partial mediation, full mediation, no mediation
# 1 table at most, subset of mediation outputs (average ACME, average ADE, prop mediated, total effect)

output_pres <- output_pres %>% 
  rename(ACME = d0,
         ACME.p = d0.p,
         ADE = z0,
         ADE.p = z0.p,
         Total = tau.coef,
         Total.p = tau.p,
         prop_med = n0,
         prop_med.p = n0.p)

output_senate <- output_senate %>% 
  rename(ACME = d0,
         ACME.p = d0.p,
         ADE = z0,
         ADE.p = z0.p,
         Total = tau.coef,
         Total.p = tau.p,
         prop_med = n0,
         prop_med.p = n0.p)

avg_acme_pres <- output_pres %>% 
  mutate(acme_weight = ACME * size,
         ade_weight = ADE * size,
         total_weight = Total * size,
         prop_weight = prop_med * size) %>% 
  summarise(avg_acme = sum(acme_weight) / sum(size),
            avg_ade = sum(ade_weight) / sum(size),
            avg_tot = sum(total_weight) / sum(size),
            avg_prop = sum(prop_weight) / sum(size))

avg_acme_senate <- output_senate %>% 
  mutate(acme_weight = ACME * size,
         ade_weight = ADE * size,
         total_weight = Total * size,
         prop_weight = prop_med * size) %>% 
  summarise(avg_acme = sum(acme_weight) / sum(size),
            avg_ade = sum(ade_weight) / sum(size),
            avg_tot = sum(total_weight) / sum(size),
            avg_prop = sum(prop_weight) / sum(size))


# Visualizations ----------------------------------------------------------

## Example mediation plots for results section in paper
# No mediating effect
med_plot_1 <- ~plot(rslt_pres[[31]][[3]], 
                    main="a. Georgia",
                    cex.axis = 1.3,
                    cex.main = 1.5) 
# Partial mediating effect
med_plot_2 <- ~plot(rslt_pres[[12]][[3]], 
                    main="b. Illinois", 
                    cex.axis = 1.3,
                    cex.main = 1.5) 
# Full mediating effect
med_plot_3 <- ~plot(rslt_pres[[2]][[3]], 
                    main="c. Arizona", 
                    cex.axis = 1.3,
                    cex.main = 1.5) 

# Combine plots into one figure for use in paper (1333x333 png)
plot_grid(med_plot_1, med_plot_2, med_plot_3, ncol=3)

# Plot to show range of ACMEs in slide deck

quartiles_acme <- quantile(output_pres$ACME)
iqr_acme <- quartiles_acme[4] - quartiles_acme[2]
lower_fence <- quartiles_acme[2] - (1.5 * iqr_acme)
upper_fence <- quartiles_acme[4] + (1.5 * iqr_acme)

ggplot(output_pres) +
  geom_boxplot(aes(ACME), fill="#bccbcb") +
  scale_y_continuous(labels = c()) +
  labs(title = "Average Causal Mediation Effect",
       x = "") +
  guides(fill=FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 18))

ggplot(output_pres) +
  geom_boxplot(aes(ACME), fill="#bccbcb", outlier.shape = NA) + 
  scale_x_continuous(limits = c(lower_fence, upper_fence)) + 
  scale_y_continuous(labels = c()) +
  labs(title = "Average Causal Mediation Effect, Outliers Removed",
       x = "") +
  theme_minimal() +
  theme(text = element_text(size = 18))

ggplot(output_pres) +
  geom_boxplot(aes(ADE), fill="#bccbcb") +
  scale_y_continuous(labels = c()) +
  labs(title = "Average Direct Effect",
       x = "") +
  guides(fill=FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 18))

ggplot(output_pres) +
  geom_boxplot(aes(ADE), fill="#bccbcb", outlier.shape = NA) + 
  scale_x_continuous(limits = c(lower_fence, upper_fence)) + 
  scale_y_continuous(labels = c()) +
  labs(title = "Average Direct Effect, Outliers Removed",
       x = "") +
  theme_minimal() +
  theme(text = element_text(size = 18))
