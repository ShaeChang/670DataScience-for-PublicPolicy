library(tidyverse)
#at the beginning always library "tidyverse"

base_url <- "https://www2.census.gov/programs-surveys/demo/datasets/hhp/"
week_url <- "2021/wk40/HPS_Week40_PUF_CSV.zip"
pulse_url <- paste0(base_url, week_url)
# For Mac, *.nix systems:
download.file(
  pulse_url,
  destfile = "data/pulse40.zip"
)

unzip(
  zipfile = "data/pulse40.zip",
  exdir = "data/")
#don't need any other folders before "data", because we are already in the week5_project folder.

install.packages("janitor")
library(janitor)

pulse <- read.csv("data/pulse2021_puf_40.csv") %>%
  clean_names(pulse)
glimpse(pulse)
#glimpse和直接打出来tibble名字的最大差别在于，glimpse可以看到所有colmns的名称，因为它把这个放在纵轴上。

dc_centroids <- read_csv("data/geocorr2014_dc.csv")


