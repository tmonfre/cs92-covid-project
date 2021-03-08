## QSS 82/83 Winter 2020
## Data Tidying for US Senate data

# Load packages -----------------------------------------------------------

library(tidyverse)

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

# Senate data -----------------------------------------------------------

senate16 <- read.csv("data/senate-elections/2016.csv") %>% 
  filter(office == "US Senate" & party %in% c("democratic", "republican") & mode == "total") %>% 
  select(state, jurisdiction, party, votes) %>% 
  rename(county = jurisdiction) %>% 
  group_by(state, county, party) %>% 
  summarise(votes = sum(votes)) %>% 
  spread(party, votes) %>% 
  mutate(dem_share = democratic / (democratic + republican)) %>% 
  mutate(rep_share = republican / (democratic + republican)) %>% 
  mutate(margin = dem_share - rep_share) %>% 
  select(state, county, margin) %>% 
  filter(!is.na(margin))

senate18 <- read.csv("data/senate-elections/2018.csv") %>% 
  filter(party %in% c("Democrat", "Republican")) %>% 
  select(state, county, party, votes) %>% 
  group_by(state, county, party) %>% 
  summarise(votes = sum(votes)) %>% 
  spread(party, votes) %>% 
  mutate(dem_share = Democrat / (Democrat + Republican)) %>% 
  mutate(rep_share = Republican / (Democrat + Republican)) %>% 
  mutate(margin = dem_share - rep_share) %>% 
  select(state, county, margin) %>% 
  filter(!is.na(margin))


senate20 <- read.csv("data/senate-elections/2020.csv") %>% 
  filter(party %in% c("REP", "DEM")) %>% 
  mutate(county = remove_county_word(county)) %>% 
  select(state, county, party, total_votes) %>% 
  group_by(state, county, party) %>% 
  summarise(votes = sum(total_votes)) %>% 
  spread(party, votes) %>% 
  mutate(dem_share = DEM / (DEM + REP)) %>% 
  mutate(rep_share = REP / (DEM + REP)) %>% 
  mutate(margin = dem_share - rep_share) %>% 
  select(state, county, margin) %>% 
  filter(!is.na(margin))

senateData <- rbind(senate16, senate18, senate20)


# Write final dataset into csv
write.csv(senateData, "./data/senate_data.csv")
