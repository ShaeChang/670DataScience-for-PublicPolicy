library(tidyverse)
library(tidycensus)
library(sf)

census_api_key("your-key-string")

dc_income <- get_acs(geography = "tract", 
                     variables = "B19013_001", 
                     state = "DC", 
                     county = "District of Columbia", 
                     geometry = TRUE,
                     progress = FALSE)

dc_income %>%
  ggplot() +
  geom_sf(aes(fill = estimate), color = "white", size = 0.1) +
  scale_fill_gradient(
    low = "#cfe8f3", 
    high = "#062635",
    labels = scales::dollar
  ) +
  theme_void() +
  labs(
    title = "DC is Highly Segregated by Household Income",
    caption = "2015-2019 5-Year ACS",
    fill = "Household Income"
  )

{r #5 retrieve}
  readRenviron("~/.Renviron")
  v17 <- load_variables(2017, "acs5", cache = TRUE)
  cookcounty_r <- as_tibble(
    get_acs(
      geography = "tract",
      variables = c(medianinc = "B19013_001",
                    poppov = "B17001_001",
                    popdegree = "B15003_022"),
      state = 17,
      country = 031,
      geometry = TRUE,
      year = 2017
    )
  )
  view(cookcounty_r)

