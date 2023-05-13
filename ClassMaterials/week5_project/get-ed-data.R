library(httr)
library(jsonlite)
library(tidyverse)

url <- 
  "https://educationdata.urban.org/api/v1/college-university/ipeds/enrollment-full-time-equivalent/2016/?level_of_study=2&fips=11"
## 这两个URL都可以用，有点奇怪哦"https://educationdata.urban.org/api/v1/college-university/ipeds/enrollment-full-time-equivalent/2016/2/?fips=11"

ed_data <- GET(url = url,
               user_agent("Georgetown Univ. Student Data Collector (xz551@georgetown.edu)."))
http_status(ed_data)
