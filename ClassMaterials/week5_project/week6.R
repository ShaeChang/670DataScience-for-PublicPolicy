install.packages(c("httr", "jsonlite"))
library(httr)
library(jsonlite)
library(tidyverse)

# make the URL
url <-
  "https://api.census.gov/data/2014/pep/natstprc?get=STNAME,POP&DATE_=7&for=state:*"
# use the URL to make a request from the API
pop_json <- GET(url = url)
http_status(pop_json)
# get the contents of the response as a text string
pop_json <- content(pop_json, as = "text")
pop_matrix <- fromJSON(pop_json)
# turn the body of the character matrix into a tibble
pop_data <- as_tibble(pop_matrix[2:nrow(pop_matrix), ],
                      .name_repair = "minimal")
# add variable names to the tibble
names(pop_data) <- pop_matrix[1, ]
pop_data

library(sf)
install.packages("tigris")
library(tigris)
DC_roads <- roads(state = "DC", county = "District of Columbia")
DC_roadsmap <- ggplot(DC_roads) +
  geom_sf() +
  theme_void()
DC_roadsmap

# Exercise 2
mcbrocken <- read_csv("mcbroken.csv")
mcbroken_sf <- st_as_sf(mcbrocken, coords = c("lon", "lat")) %>%
  st_set_crs(value = 4326)
mcbrocken_map <- ggplot(mcbroken_sf, color = is_broken) +
  geom_sf() +
  theme_void()
mcbrocken_map
