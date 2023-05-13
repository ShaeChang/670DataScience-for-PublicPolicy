library(tidyverse)
library(lubridate)

# use this url to download the data directly into R
df <- read_csv("https://data.cityofnewyork.us/api/views/43nn-pn8j/rows.csv")

# clean names with janitor
sampled_df <- df %>% 
  janitor::clean_names() 

# create an inspection year variable
sampled_df <- sampled_df %>%
  mutate(inspection_date = mdy(inspection_date)) %>%
  mutate(inspection_year = year(inspection_date))

# get most-recent inspection
sampled_df <- sampled_df %>%
  group_by(camis) %>%
  filter(inspection_date == max(inspection_date)) %>%
  ungroup()

# subset the data
sampled_df <- sampled_df %>%
  select(camis, boro, zipcode, cuisine_description, inspection_date,
         action, violation_code, violation_description, grade,
         inspection_type, latitude, longitude, council_district,
         census_tract, inspection_year, critical_flag) %>%
  filter(complete.cases(.)) %>%
  filter(inspection_year >= 2017) %>%
  filter(grade %in% c("A", "B", "C")) 

# create the binary target variable
sampled_df <- sampled_df %>%
  mutate(grade = if_else(grade == "A", "A", "Not A")) %>%
  mutate(grade = as.factor(grade))

# create extra predictors
sampled_df <- sampled_df %>%
  group_by(boro, zipcode, cuisine_description, inspection_date,
           action, violation_code, violation_description, grade,
           inspection_type, latitude, longitude, council_district,
           census_tract, inspection_year)  %>%
  mutate(vermin = str_detect(violation_description, pattern = "mice|rats|vermin|roaches")) %>%
  summarize(violations = n(),
            vermin_types = sum(vermin),
            critical_flags = sum(critical_flag == "Y")) %>%
  ungroup()

# write the data
write_csv(sampled_df, "restaurant_grades.csv")
