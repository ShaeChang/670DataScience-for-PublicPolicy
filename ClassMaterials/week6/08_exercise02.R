library(tidyverse)
library(sf)
library(here)
library(tigris)

mcbroken <- read_csv(here("tutorials", "08_sf-geospatial", "data", "mcbroken.csv")) %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(value = 4326) %>%
  filter(state == "VA")

virginia <- states(cb = TRUE) %>%
  filter(STUSPS == "VA")

ggplot() +
  geom_sf(data = virginia) +
  geom_sf(data = mcbroken, mapping = aes(color = is_broken), alpha = 0.2) +
  theme_void()
