library(tidyverse)
library(tidymodels)
library(factoextra)
library(broom)

# read in employment data from Tidy Tuesday
# Source: Julia Silge, https://juliasilge.com/blog/kmeans-employment/ 
employed <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/employed.csv") %>%
  filter(!is.na(employ_n)) %>%
  mutate(occupation = paste(industry, minor_occupation)) %>%
  mutate(occupation = snakecase::to_snake_case(occupation))


# calculate average number employed in each industry/race_gender group from 2015-2020
# don't forget to ungroup() at the end!
employed_tidy <- employed %>%
  ## FILL IN HERE ##

# reshape data to wide so each row is a unique observation, filter to rows 
# where the total employment is greater than 1000, clean column names using 
# janitor::clean_names(), and convert each race/gender column to a proportion 
# of the total. You should have 220 observations when you're done
employment_demo <- employed_tidy %>%
  ## FILL IN HERE ##

employment_rec <- employment_demo %>%
  select(-occupation, -men)

# create a recipe with no outcome variable and all predictors
clust_rec <- recipe(~., data = employment_rec) %>%
  # take the log of total
  ## FILL IN HERE ##
  # center and scale all predictors
  ## FILL IN HERE ##
  # run prep to prepare recipe
  prep()

# apply recipe to employment_rec
employment_clust <- clust_rec %>%
  bake(new_data = NULL)

# PCA ---------------------------------------------------------------------

# create a correlation matrix on employment_clust
## FILL IN HERE ##

# conduct PCA on the employment_clust data
principal_components <- ## FILL IN HERE ##

# obtain summary metrics
## FILL IN HERE ##

# obtain loadings
## FILL IN HERE ##

# obtain component values for each observation
pca_data <- as_tibble(principal_components$x) %>%
  select(PC1, PC2, PC3)

# cluster analysis --------------------------------------------------------

# set a seed because the clusters are not deterministic
set.seed(20200205)

# total within sum of squares
fviz_nbclust(employment_clust, FUN = kmeans, method = "wss")

# total silhouette width
fviz_nbclust(employment_clust, FUN = kmeans, method = "silhouette")

# gap statistic
fviz_nbclust(employment_clust, FUN = kmeans, method = "gap_stat")

# run kmeans with the optimal number of clusters using the employment_clust data, set nstart = 100
clust_kmeans <- ## FILL IN HERE ##

# examine the cluster means 
tidy(clust_kmeans)

# visualization -----------------------------------------------------------

# create a dataframe that binds together the occupation, PCA, and cluster data
occ_clusters <- bind_cols(
  select(employment_demo, occupation), 
  select(pca_data, PC1, PC2), 
  cluster = clust_kmeans$cluster
)

# get names of most central occupations to each cluster
minor_occ_names <- occ_clusters %>%
  left_join(employed %>% select(occupation, minor_occupation) %>% unique()) %>%
  group_by(cluster) %>%
  mutate(dist = sqrt((PC1 - mean(PC1)) ^ 2 + (PC2 - mean(PC2)) ^ 2)) %>%
  slice_min(dist) %>%
  ungroup()

# create a plot of the clusters with PC1 and PC2 as the x and y axis
ggplot() +
  geom_point(
    data = ## FILL IN HERE ##
    mapping = ## FILL IN HERE ##
    alpha = 0.5
  ) +
  ggrepel::geom_text_repel(
    data = minor_occ_names %>% left_join(occ_clusters), 
    mapping = ## FILL IN HERE ##
    size = 2
  ) +
  scale_color_manual(values = c("blue", "red", "green")) +
  labs(
    ## FILL IN HERE ##
  ) + 
  theme_minimal() +
  guides(text = NULL)

# interactive visualization -----------------------------------------------

# the same visualization as above with a tooltip to explore the minor 
# occupation
cluster_plot <- ggplot() +
  geom_point(
    data = ## FILL IN HERE ##, 
    mapping = aes(x = ## FILL IN HERE ##, 
                  y = ## FILL IN HERE ##, 
                  color = ## FILL IN HERE ##,
                  text = minor_occupation),  #added "text" mapping to work with tooltip
    alpha = 0.5
  ) +
  scale_color_manual(values = c("blue", "red", "green")) +
  labs(
    ## FILL IN HERE ##
  ) + 
  theme_minimal() +
  guides(text = NULL)

# Use the plotly package and the ggplotly() function to create an interactive
# version of cluster_plot with a tooltip for the minor_occupation field

library(plotly)
ggplotly(cluster_plot, tooltip = ## Fill in Here ##)
