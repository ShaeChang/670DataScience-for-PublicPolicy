library(tidyverse)
library(rvest)
library(lubridate)


# get EO metadata ---------------------------------------------------------

# download executive order metadata
# note: "All Executive Orders since 1994" is incorrectly limited to 1,000 rows
# instead, we download the individual files and combine them
csvs <- list.files("metadata/", full.names = TRUE)
presidents <- c("obama", "trump", "bush", "biden", "clinton")

assign_president <- function(file, president) {
  
  read_csv(file) %>%
    mutate(president = president)
  
}

meta_data <- map2_dfr(.x = csvs, .y = presidents, .f = assign_president)

# add a URL with the plain text for each EO
meta_data <- meta_data %>%
  mutate(publication_date = mdy(publication_date)) %>%
  mutate(text_url = as.character(str_glue("https://www.govinfo.gov/content/pkg/FR-{publication_date}/html/{document_number}.htm")))


# downloads EO text -------------------------------------------------------

# this downloads the raw text and saves it to a directory
# the codes sleeps for 3 seconds between each pull to be courteous

parse_text <- function(url, id) {
  
  Sys.sleep(3)
  
  url <- url(url, "rb")
  
  html <- read_html(x = url) %>%
    html_element("body") %>%
    html_text2()
  close(url)
  
  write.table(x = html, file = paste0("text/", str_pad(id, width = 4, pad = "0", side = "left"), ".txt"))
  
}

# get the vector of URLS
plain_text_eo_url <- pull(meta_data, text_url)

# downalod the text
map(.x = 1:1127, ~parse_text(plain_text_eo_url[.x], .x))

# one-row-per-line format -------------------------------------------------

# list all of the downloaded files
eos <- list.files("text", full.names = TRUE)

# parse the messy EOs into one-row-per-line format
clean_eo <- function(file, info) {
  
  eo <- tibble(
    text = read_lines(file)
  ) 
  
  eo <- eo %>%
    mutate(text = str_trim(text)) 
  
  eo <- eo %>%
    mutate(index = cumsum(str_detect(text, "^\\(Presidential Sig.\\)"))) %>%
    filter(index == 0)
  
  eo <- eo %>%
    mutate(index = cumsum(text == "Presidential Documents")) %>%
    filter(index > 0)
  
  eo <- eo %>%
    mutate(eo_row = cumsum(str_detect(text, "^Executive Order"))) %>%
    mutate(power_row = cumsum(str_detect(text, "^By the authority vested"))) %>%
    mutate(eo_row = if_else(power_row == 1, 0L, eo_row)) %>%
    mutate(cut_row = cumsum(eo_row == max(eo_row) & eo_row != lag(eo_row))) %>%
    filter(cut_row > 0) %>%
    select(text)
  
  eo <- eo %>%
    mutate(
      president = info$president,
      executive_order_number = info$executive_order_number,
      signing_date = info$signing_date
    )

  return(eo)
  
}

# 1082 isn't a traditional EO
# it was released by the Secretary of Defense instead of the President
eos <- eos[-1082]
meta_data <- meta_data[-1082, ]

# iterate
eos_tidy <- map_dfr(1:length(eos), ~clean_eo(eos[.x], meta_data[.x, ]))

# arrange
eos_tidy <- eos_tidy %>%
  arrange(signing_date)

# save
write_csv(eos_tidy, file = "executive-orders.csv")
