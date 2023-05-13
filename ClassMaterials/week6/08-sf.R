library(tigris)
library(tidyverse)
library(sf)

mcbroken <- read.csv(here("tutorials", "08_sf-geospatial","data", "mcbroken.csv"))
mcbroken_sf <- st_as_sf(mcbroken, coords = c("lon","lat")) %>%
  st_set_crs(value = 4326) %>%
  filter(state == "VA")

virginia <- states(cb = TRUE) %>%
  filter()
  
ggplot()+
  geom_sf(data = virginia)
  

library(tidycensus)
install.packages("dotenv")
census_api_key("your-key-string", install = TRUE)

library(dotenv)
Sys.getenv("CENSUS_API_KEY")

dc_income <- get_acs(geography = "tract",
                     variables = "B19013_001",
                     state = "DC",
                     county = "District of Columbia",
                     geometry = TRUE,
                     year = 2019,
                     progress = FALSE)