## QSS 82/83 Winter 2020
## Data Tidying for US mobility data

# Load packages -----------------------------------------------------------

library(tidyverse)

# Read in data ------------------------------------------------------------
# Relative path of working directory is ../ (the project directory)

dta <- read.csv("data/2020_US_Region_Mobility_Report.csv")

# Data wrangling ----------------------------------------------------------

voluntary <- c("retail_and_recreation",
              "parks")

involuntary <- c("grocery_and_pharmacy",
                "transit_stations",
                "workplaces",
                "residential")

mobility_type <- c(voluntary, involuntary)

mobility_summary <- dta %>% 
  group_by(sub_region_2, date) %>% 
  summarise(grocery_and_pharmacy = mean(grocery_and_pharmacy_percent_change_from_baseline, na.rm = TRUE),
            transit_stations = mean(transit_stations_percent_change_from_baseline, na.rm = TRUE),
            workplaces = mean(workplaces_percent_change_from_baseline, na.rm = TRUE),
            residential = mean(residential_percent_change_from_baseline, na.rm = TRUE),
            retail_and_recreation = mean(retail_and_recreation_percent_change_from_baseline, na.rm = TRUE),
            parks = mean(parks_percent_change_from_baseline, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(grocery_and_pharmacy = ifelse(is.nan(grocery_and_pharmacy), NA, grocery_and_pharmacy),
         transit_stations = ifelse(is.nan(transit_stations), NA, transit_stations),
         workplaces = ifelse(is.nan(workplaces), NA, workplaces),
         residential = ifelse(is.nan(residential), NA, residential),
         retail_and_recreation = ifelse(is.nan(retail_and_recreation), NA, retail_and_recreation),
         parks = ifelse(is.nan(parks), NA, parks))

# Group mobility by county into voluntary and involuntary

mobility <- mobility_summary %>% 
  pivot_longer(cols = all_of(mobility_type), 
               names_to = "mobility_type", 
               values_to = "percent_change_from_baseline") %>% 
  mutate(mobility_label = ifelse(mobility_type %in% involuntary, "involuntary", "voluntary")) %>% 
  group_by(date, sub_region_2, mobility_label) %>% 
  summarize(percent_change_from_baseline = mean(percent_change_from_baseline, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(percent_change_from_baseline = ifelse(is.nan(percent_change_from_baseline), 
                                               NA, 
                                               percent_change_from_baseline))

# Very rough visualization of mobility over time - no conditioning on county

mobility_categorized <- mobility %>% 
  group_by(date, mobility_label) %>% 
  summarise(percent_change_from_baseline = mean(percent_change_from_baseline, na.rm = TRUE)) %>% 
  ungroup()

months <- c("2020-03-01", 
            "2020-04-01", 
            "2020-05-01", 
            "2020-06-01", 
            "2020-07-01", 
            "2020-08-01", 
            "2020-09-01",
            "2020-10-01",
            "2020-11-01",
            "2020-12-01")


ggplot(mobility_categorized, aes(date, 
                                 percent_change_from_baseline, 
                                 group = mobility_label, 
                                 color = mobility_label)) + 
  geom_point() +
  scale_x_discrete(breaks = months)

