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

# Run all code from here to line 60 to properly load path model
tidydag <- dag1 %>% 
  tidy_dagitty() %>% 
  dag_label(labels = c("Cases" = "COVID Cases",
                        "vmr" = "Voluntary\n Mobility\n Rate",
                        "cp" = "County\n Partisanship",
                        "ur" = "Urban vs. Rural",
                        "ivr" = "Involuntary\n Mobility\n Rate",
                        "s" = "Seasonality",
                        "ld" = "Lockdowns"))

locations <- data.frame(name = c("ivr", "Cases", "cp", "s", "ld", "ur", "vmr"),
                        x_new = c(8.7, 8, 8.3, 8.2, 8.7, 8.45, 9),
                        y_new = c(9.6, 9.5, 9.6, 9.4, 9.4, 9.4, 9.5))

tidydag[[1]] <- tidydag[[1]] %>%
  left_join(locations, by="name")

tidydag[[1]]$x = tidydag[[1]]$x_new
tidydag[[1]]$y = tidydag[[1]]$y_new

tidydag[[1]] <- tidydag[[1]] %>%
  left_join(locations, by=c("to" = "name"))

tidydag[[1]]$xend = tidydag[[1]]$x_new.y
tidydag[[1]]$yend = tidydag[[1]]$y_new.y

tidydag %>%
  mutate(linetype = ifelse(direction == "<->", "dashed", "solid")) %>% 
ggplot(aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_dag_point() + 
  # geom_dag_text() +
  geom_dag_label_repel(aes(label = label)) +
  geom_dag_edges(aes(edge_linetype = linetype), 
                 show.legend = FALSE) +
  theme_dag()

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
