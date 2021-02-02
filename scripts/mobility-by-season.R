library(tidyverse)

spring_start_date <- "2020-03-19"
summer_start_date <- "2020-06-20"
fall_start_date <- "2020-09-22"
winter_start_date <- "2020-12-21"

google_mobility <- read.csv("./data/Global_Mobility_Report.csv", header = TRUE)

us_mobility <- google_mobility %>% 
  filter(country_region == "United States") %>% 
  mutate(voluntary_mobility = retail_and_recreation_percent_change_from_baseline) %>% 
  mutate(season = ifelse(date < spring_start_date, "Winter", 
                         ifelse(date >= spring_start_date & date < summer_start_date, "Spring",
                                ifelse(date >= summer_start_date & date < fall_start_date, "Summer",
                                       ifelse(date >= fall_start_date & date < winter_start_date, "Fall", "Winter")))))

# determine split by season (average, removing NAs)
mobility_by_season_mean <- us_mobility %>% 
  select(season, voluntary_mobility) %>% 
  group_by(season) %>% 
  summarise(voluntary_mobility = mean(voluntary_mobility, na.rm = TRUE))

mobility_by_season_mean


# determine split by season (median, removing NAs)
mobility_by_season_median <- us_mobility %>% 
  select(season, voluntary_mobility) %>% 
  group_by(season) %>% 
  summarise(voluntary_mobility = median(voluntary_mobility, na.rm = TRUE))

mobility_by_season_median


# compare winter to summer
t.test(us_mobility$voluntary_mobility[us_mobility$season == "Winter"],
       us_mobility$voluntary_mobility[us_mobility$season == "Summer"])

# compare spring to summer
t.test(us_mobility$voluntary_mobility[us_mobility$season == "Spring"],
       us_mobility$voluntary_mobility[us_mobility$season == "Summer"])

# compare spring to fall
t.test(us_mobility$voluntary_mobility[us_mobility$season == "Summer"],
       us_mobility$voluntary_mobility[us_mobility$season == "Fall"])

# compare fall to winter
t.test(us_mobility$voluntary_mobility[us_mobility$season == "Fall"],
       us_mobility$voluntary_mobility[us_mobility$season == "Winter"])

# compare spring to fall
t.test(us_mobility$voluntary_mobility[us_mobility$season == "Spring"],
       us_mobility$voluntary_mobility[us_mobility$season == "Fall"])


ggplot(data = mobility_by_season_mean, mapping = aes(x = season, y = voluntary_mobility)) +
  geom_col()

