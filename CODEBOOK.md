# QSS 82/83 Team 3
## Codebook Version 1:

Run [senate-race-tidying.R](./scripts/senate-race-tidying.R), then [data-tidying1.R](./scripts/data-tidying1.R) to load all data into and save a copy of [combined_data.csv](./data/combined_data.csv)

### Data used in analysis:
[Data](./data/combined_data.csv)

Documentation:

| Variable          | Description     | 
| :-------------    | :---------- | 
|`date`             | Data of data collection   |
|`state`            | State of data |
| `county`          | County of data |
| `margin_election` | Democrat - Republican difference of vote share in 2020 Presidential Election |
|`margin-senate`    | Democrat - Republican difference of vote share in most recent Senate elction out of 2016, 2018, and 2020 |
|`voluntary`        | Google's `percent_change_in_baseline` mobility metric for voluntary mobility |
|`involuntary`      | Google's `percent_change_in_baseline` mobility metrix for involuntary mobility |
|`cases`            | Number of known COVID-19 cases |
|`lockdowns`        | Presence of a state-mandated lockdown due to COVID-19 |
|`popest_2019`      | United States Census Bureau's estimated population for the county in 2019 |


### Mobility Data:

[Data](./data/2020_US_Region_Mobility_Report.csv)

Source: [Google COVID-19 Community Mobility Reports](https://www.google.com/covid19/mobility/index.html?hl=en)

[Documentation for raw data](https://www.google.com/covid19/mobility/data_documentation.html)

Tidied data documentation:

| Variable       | Description     | 
| :------------- | :---------- | 
|`date`          | Data of data collection   |
| `county` | County of data |
| `mobility_label` | Type of mobility (voluntary or involuntary)|
|`percent_change_from_baseline`| Change in length of visits and stays compared to an arbitrary baseline calculcated by Google [(more info here)](https://www.google.com/covid19/mobility/data_documentation.html#about-this-data)|

### Partisanship Data:

[Data](./data/president_county_candidate.csv)

Source: [Kaggle](https://www.kaggle.com/unanimad/us-election-2020?select=president_county_candidate.csv)

Tidied data documentation (mostly from the source):

| Variable       | Description     | 
| :------------- | :---------- | 
|`state`          | U.S. State name |
|`county` | County name |
|`candidate` | Candidate name |
|`party`| Candidate party name |
|`total_votes` | Reported votes cast for the candidate |
|`won`| Whether a candidate received the most votes in a county |
|`votes_in_county`| Total votes cast in a county |
|`vote_share`| Fraction of votes the candidate received|

### Covid-19 Case Data:

[Data](./data/covid-counties.csv)

Source: [New York Times us-counties.csv](https://github.com/nytimes/covid-19-data/)

[Rough data documentation](https://github.com/nytimes/covid-19-data/#historical-data)

Data documentation:

| Variable       | Description     | 
| :------------- | :---------- | 
|`date`          | Date of data collection |
|`county` | County name |
|`state` | U.S. State name |
|`fips`| County FIPS code [see list here](https://www.nrcs.usda.gov/wps/portal/nrcs/detail/national/home/?cid=nrcs143_013697)  |
|`cases` | Reported COVID-19 cases |
|`deaths`| Reported COVID-19 deaths |

### Government Lockdown Data:

[Data](./data/lockdowns_by_state.csv)

Source: [Oxford Coronavirus Government Response Tracker USA State-level data](https://github.com/OxCGRT/USA-covid-policy)

[Codebook provided by Oxford](https://github.com/OxCGRT/covid-policy-tracker/blob/master/documentation/codebook.md)

We focus on the [Containment and closure policies](https://github.com/OxCGRT/covid-policy-tracker/blob/master/documentation/codebook.md#containment-and-closure-policies) tracked by Oxford

### US Census Data:

[Data](./data/census-population.csv)

Source: [United States Census](https://www.census.gov/content/census/en/data/datasets/time-series/demo/popest/2010s-counties-total.html)

Data documentation:

| Variable       | Description     | 
| :------------- | :---------- | 
|`county` | County name |
|`state` | U.S. State name |
|`census` | County population from 2010 census |
|`X2011`-`X2019`| expeced population for a year (2011 through 2019)|
