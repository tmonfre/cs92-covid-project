## QSS 82/83 Winter 2020
## Data Tidying for US mobility data

# Load packages -----------------------------------------------------------

library(tidyverse)
library(lubridate)

# Note: Working directory is the project directory, which is ../ relative to this file

# Functions ---------------------------------------------------------------

# Remove the word "County" from the datasets that mark counties as "___ County"
remove_county_word <- function(county) {
  if(grepl("County", county)) {
    idx <- str_locate(county, " County") - 1
    return(str_sub(county, 1, idx)[1])
  } else {
    return(county)
  }
}

# Mobility data -----------------------------------------------------------

mobility_dta <- read.csv("data/2020_US_Region_Mobility_Report.csv")


voluntary <- c("retail_and_recreation",
              "parks")

involuntary <- c("grocery_and_pharmacy",
                "transit_stations",
                "workplaces",
                "residential")

mobility_type <- c(voluntary, involuntary)

mobility_summary <- mobility_dta %>% 
  group_by(sub_region_1, sub_region_2, date) %>% 
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
  group_by(date, sub_region_1, sub_region_2, mobility_label) %>% 
  summarize(percent_change_from_baseline = mean(percent_change_from_baseline, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(percent_change_from_baseline = ifelse(is.nan(percent_change_from_baseline), 
                                               NA, 
                                               percent_change_from_baseline)) %>% 
  rename(state = sub_region_1, county = sub_region_2)

mobility_foranalysis <- mobility %>% 
  pivot_wider(names_from = mobility_label, values_from = percent_change_from_baseline) %>% 
  filter(!is.na(voluntary), !is.na(involuntary))

mobility_foranalysis$county = sapply(mobility_foranalysis$county, remove_county_word)

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
            "2020-12-01",
            "2021-01-01")


ggplot(mobility_categorized, aes(date, 
                                 percent_change_from_baseline, 
                                 group = mobility_label, 
                                 color = mobility_label)) + 
  geom_point() +
  geom_smooth() + 
  scale_x_discrete(breaks = months) + 
  scale_color_brewer(palette="Set2") +
  labs(title = "United States Mobility Percent Change from Baseline, February 2020 to January 2021",
       caption = "Data Source: Google",
       x = "",
       y = "Mean Percent Change from Google's Established Baseline",
       color = "Mobility Type")

# Partisanship Data - 2020 Election ---------------------------------------

election_dta <- read.csv("data/president_county_candidate.csv")

election <- election_dta %>% 
  group_by(county, state) %>% 
  mutate(won = ifelse(won == "True", TRUE, FALSE),
         votes_in_county = sum(total_votes),
         vote_share = (total_votes/votes_in_county)) %>% 
  ungroup() %>% 
  filter(party %in% c("DEM", "REP")) %>% 
  group_by(county, state) %>% 
  mutate(margin = vote_share - min(vote_share)) %>%
  filter(margin == max(margin)) %>%
  # Following 2 lines ignore "REP" tags for tied votes - potential bias?
  mutate(margin = ifelse(margin == 0 & party == "REP", NA, margin)) %>% 
  filter(!is.na(margin))%>% 
  ungroup() %>% 
  mutate(margin = ifelse(party == "REP", margin * -1, margin)) %>% 
  select(state, county, margin)

election$county <- sapply(election$county, remove_county_word)

senate_election_dta <- read.csv("data/senate_data.csv")

combined_election <- election %>% 
  left_join(senate_election_dta, by = c("state", "county")) %>% 
  select(-X) %>% 
  rename(margin_election = margin.x, margin_senate = margin.y) %>% 
  dplyr::select(state, county, margin_election, margin_senate)

election <- combined_election


# COVID-19 Cases ----------------------------------------------------------

# The data is already in a tidy format,
# Next step is to see how we'll combine it with other datasets, and will further tidy from there
covid_dta <- read.csv("data/covid-counties.csv")

# Lockdown Data - Oxford Coronavirus Government Response Tracker ----------

# Codebook for data: https://github.com/OxCGRT/covid-policy-tracker/blob/master/documentation/codebook.md
lockdown_dta <- read.csv("data/lockdowns_by_state.csv")

# Only keep metadata and containment/closure policies
lockdowns <- lockdown_dta[1:29] %>%
  rename(state = RegionName) %>% 
  mutate(date = as.character(ymd(Date)))

# Census Data -------------------------------------------------------------


census_dta <- read.csv("data/census-population.csv") 

# Only keep county, state, and latest population estimate (2019)
# Also clean up string and remove "County" from county names to match previous datasets
census <- census_dta %>% 
  select(county = County, state = State, popest_2019 = X2019) %>% 
  mutate(state = str_trim(state))
         
census$county <- sapply(census$county, remove_county_word)

# Join all data into 1 frame ----------------------------------------------

# Data without dates (elections and census)
elections_census <- election %>% 
  inner_join(census, by=c("county", "state"))

# Data with dates (mobility, lockdowns, cases)
dated_dta <- mobility_foranalysis %>% 
  left_join(covid_dta, by=c("date", "state", "county")) %>% 
  left_join(lockdowns, by=c("date", "state"))

regression_vars <- c("date",
                     "state",
                     "county",
                     "margin_election",
                     "margin_senate",
                     "voluntary",
                     "involuntary",
                     "cases",
                     "C6_Stay.at.home.requirements",
                     "popest_2019")

# Data frame with everything
dta <- dated_dta %>% 
  left_join(elections_census, by=c("state", "county")) %>% 
  filter(state != "", 
         county != "", 
         !is.na(cases),
         !is.na(popest_2019)) %>%
  select(all_of(regression_vars)) %>% 
  rename(lockdowns = C6_Stay.at.home.requirements)

# Write final dataset into csv
write.csv(dta, "./data/combined_data.csv")
