## QSS Quarter Project, Winter 2021
## Derek Lue, Thomas Monfre, Joseph Notis, Michael Zhang
## Path Models

library(tidyverse)
library(ggdag)

# Create relationships
# CHANGE FROM V1 to V2: removed path from Lockdowns --> COVID Cases
dag1 <- dagify(Cases ~ cp + ur + ivr + s,
               vmr ~ cp + ur + s + ld + Cases,
               ivr ~ ur,
               ld ~ Cases,
               cp ~ ur,
               outcome = "vmr",
               exposure="Cases",
               labels = c("Cases" = "COVID Cases",
                          "vmr" = "Voluntary\n Mobility\n Rate",
                          "cp" = "County\n Partisanship",
                          "ur" = "Urban vs. Rural",
                          "ivr" = "Involuntary\n Mobility\n Rate",
                          "s" = "Seasonality",
                          "ld" = "Lockdowns"))

# Generate plot of path model
ggdag(dag1, text=FALSE, use_labels = "label") + 
  labs(title = "Path Model") +
  theme_dag()

# Available Paths from IV to DV
ggdag_paths(dag1, shadow=TRUE, text=FALSE, use_labels = "label") + 
  labs(title = "Available Paths from COVID Cases to Voluntary Mobility Rate") +
  theme_dag()

# Adjustment Set
ggdag_adjustment_set(dag1, text = FALSE, use_labels = "label", shadow = TRUE)+
  labs(title = "Adjustment Set") +
  theme_dag()
