# this lab analyzes the text of executive orders from 1993 to the present
# metadata come from https://www.federalregister.gov/presidential-documents/executive-orders
# text scraped from https://www.govinfo.gov/content/pkg/FR-{publication_date}/html/{document_number}.htm

# the bigram plot and log odds ratios are based on examples in Text Mining with 
# R by Julia Silge and David Robinson

library(tidyverse)
library(tidytext)
library(lubridate)
library(SnowballC)
library(igraph)
library(ggraph)

theme_set(theme_minimal())

# load one-row-per-line data ----------------------------------------------
eos <- read_csv("executive-orders.csv")

eos <- ## FILL IN HERE ## remove missing values for the variable text

# tokenize the text -------------------------------------------------------
tidy_eos <- eos %>%
  unnest_tokens(
    output = ## FILL IN HERE ##, 
    input = ## FILL IN HERE ##
  )

# summary statistics ------------------------------------------------------

# number of executive orders by each President
tidy_eos %>%
  count(executive_order_number, signing_date, president) %>%
  count(president)

# words in executive orders for each President
tidy_eos %>%
  count(president, executive_order_number) %>%
  group_by(president) %>%
  summarize(
    total_words = ## FILL IN HERE ##,
    average_words = ## FILL IN HERE ##,
    minimum_words = ## FILL IN HERE ##,
    maximum_words = ## FILL IN HERE ##
  )

# plot words per year
tidy_eos %>%
  ## FILL IN HERE ## 
  ## use year() to create variable called year from signing_date
  count(year) %>%
  ggplot(aes(year, n)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(
    breaks = seq(1992, 2024, 4),
    limits = c(NA, 2024)) +
  annotate(geom = "text", x = 2021, y = 175000, label = "Biden\nInauguration", size = 2) +
  annotate(geom = "text", x = 2017, y = 95000, label = "Trump\nInauguration", size = 2) +
  labs(title = "EO Volume Spikes in Inauguration Years")

# plot words per month
tidy_eos %>%
  ## FILL IN HERE ## 
  ## use floor_date() with unit = "month" to create variable called month from signing_date
  count(month) %>%
  ggplot(aes(month, n)) +
  geom_line() +
  labs(title = "EO Volume Spikes in Inauguration Months")

# clean tokenized text ----------------------------------------------------

# create domain-specific stop words
domain_stop_words <- tribble(
  ~word, 
  "clinton",
  "bush", 
  "obama", 
  "trump",
  "biden",
  "section",
  "authority",
  "vested",
  "federal",
  "president"
) %>%
  mutate(lexicon = "custom")

stop_words <- bind_rows(
  stop_words,
  domain_stop_words
)

# remove stop words with anti_join() and the stop_words tibble
tidy_eos <- tidy_eos %>%
  ## FILL IN HERE ## 
  ## use anti_join() to remove stop words

# remove words that are entirely numbers
tidy_eos <- tidy_eos %>%
  filter(!str_detect(word, pattern = "^\\d")) 

# stem words with wordStem()
tidy_eos <- tidy_eos %>%
  ## FILL IN HERE ## 
  ## use wordStem() with word to create a new variable called stem

# compare the top words and top stems
# do you notice any big changes?
tidy_eos %>%
  count(word, sort = TRUE) %>% 
  print(n = 20)

tidy_eos %>%
  count(stem, sort = TRUE) %>% 
  print(n = 20)

# compare President Obama and President Trump -----------------------------
# let's start by comparing two presidents

# calculate the relative frequency of terms for President Obama and President Trump
frequency <- tidy_eos %>%
  filter(president %in% c("obama", "trump")) %>%
  count(president, stem) %>%
  ## FILL IN HERE ## group by president
  ## FILL IN HERE ## calculate the relative frequency of each word within each 
  ##                 president's EOs. call the variable freq
  ##                 the values should be between 0 and 1, and sum to 1 for each
  ##                 president
  ungroup()

# create a comparison plot
# the diagonal line represents equivalence
frequency %>%
  pivot_wider(-n, names_from = president, values_from = freq) %>%
  ggplot(aes(obama, trump)) +
  geom_point(alpha = 0.2) +
  geom_text(aes(label = stem), check_overlap = TRUE) +
  ## FILL IN HERE ## add a red line on a 45-degree angle to represent equivalence
  coord_equal()

# calculate the log odds ratio for President Obama / President Trump
# see page 114 of Text Mining with R for a description
word_ratios <- frequency %>%
  filter(n > 10) %>% 
  pivot_wider(-freq, names_from = president, values_from = n, values_fill = 0) %>%
  mutate(across(.cols = where(is.numeric), .fns = ~(.x + 1) / sum(.x + 1))) %>%
  mutate(log_odds_ratio = log(obama / trump))

# plot the most extreme log odds ratios
word_ratios %>%
  group_by(log_odds_ratio < 0) %>%
  slice_max(order_by = abs(log_odds_ratio), n = 15) %>%
  ungroup() %>%
  ## FILL IN HERE ## use mutate() and reorder() to reorder stem based on log_odds_ratio
  ##                 you can run the code first to see the effect of reorder()
  ggplot(aes(x = log_odds_ratio, y = stem, fill = log_odds_ratio < 0)) +
  geom_col() +
  guides(fill = "none") +
  labs(x = "log odds ratio (Obama/Trump)")

# the above code compares one president to another
# we could also compare one president to all other presidents aggregated
# alternatively, we can use TF-IDF to simultaneously compare all presidents

# use bind_tf_idf() to calculate tf-idf for where the stems are terms and the 
# documents are presidents
tf_idf <- tidy_eos %>%
  count(president, stem, sort = TRUE) %>%
  bind_tf_idf(term = stem, document = president, n = n)

# plot the most extreme tf-idf for each president
tf_idf %>%
  group_by(president) %>%
  ## FILL IN HERE ## use slice_max() to pick the top 15 tf_idf for each president
  mutate(stem = reorder(stem, tf_idf)) %>%
  ggplot(aes(tf_idf, stem, fill = president)) +
  geom_col() +
  facet_wrap(~president, scales = "free") +
  theme_minimal() +
  guides(fill = "none")

# compare words over time -------------------------------------------------

# repeat the log odds ratio and tf-idf analysis from above using years instead
# of presidents

# calculate the log odds ratio for 2009 and 2021
# first, calculate the relative frequency of terms for 2009 and 2021
frequency_time <- tidy_eos %>%
  ## FILL IN HERE ## create year
  filter(year %in% c(2009, 2021)) %>%
  count(year, stem)

# second, calculate the log odds ratio for 2021 / 2009
# see page 114 of Text Mining with R for a description
word_ratios_time <- frequency_time %>%
  ## FILL IN HERE ## 

# plot the most extreme log odds ratios
word_ratios_time %>%
  ## FILL IN HERE ## 

# this compares one year to another year
# we could also compare one year to all other years aggregated
# alternatively, we can use TF-IDF to simultaneously compare all years

# perform tf-idf for 2009-2016
tf_idf_time <- tidy_eos %>%
  mutate(year = year(signing_date)) %>%
  filter(year %in% 2009:2016) %>%
  ## FILL IN HERE ## 

# plot the most extreme tf-idf for each year
tf_idf_time %>%
  ## FILL IN HERE ## 

# Good work!
# Summary measures like term frequency, log odds ratios, and TF-IDF are very 
# sensitive to text pre-processing. Good pre-processing requires a blend of 
# generic text analysis experience and subject matter expertise.
# 
# If you have time, rerunt he above analaysis without domain_stop_words or with
# additional words added to domain_stop_words and see how it impacts the analysis
# 