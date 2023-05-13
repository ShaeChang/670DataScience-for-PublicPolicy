install.packages("rsample")
library(rsample)
install.packages("parsnip")
library(parsnip)
install.packages("recipes")
library(recipes)
install.packages("workflows")
library(workflows)
install.packages("tune")
library(tune)
install.packages("yardstick")
library(yardstick)

library(tidymodels)
set.seed(20201004)
x <- runif(n = 1000, min = 0, max = 10)
data1 <- bind_cols(
  x = x,
  y = 10 * sin(x) + x + 20 + rnorm(n = length(x), mean = 0, sd = 2)
)

set.seed(20201007)
# create a split object
data1_split <- initial_split(data = data1, prop = 0.75)
# create the training and testing data
data1_train <- training(x = data1_split)
data1_test  <- testing(x = data1_split)

# visualize the data
library(ggplot2)
data1_train %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(alpha = 0.25) +
  labs(title = "Example 1 Data") +
  theme_minimal()

# create a knn model specification
install.packages("kknn")
library(kknn)
knn_mod <-
  nearest_neighbor(neighbors = 5) %>%
  set_engine(engine = "kknn") %>%
  set_mode(mode = "regression")
# fit the knn model specification on the training data
knn_fit <- knn_mod %>%
  fit(formula = y ~ x, data = data1_train)

