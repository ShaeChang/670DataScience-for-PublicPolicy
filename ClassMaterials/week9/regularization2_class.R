library(tidyverse)
library(tidymodels)
library(patchwork)

# multicollinearity can reek havoc on estimated coefficients
# consider a simulation where the true model is y = 3 * x1 + e but x2 and x3
# are highly correlated with x1
true_coefs <- tribble(
  ~term, ~estimate,
  "x1", 3,
  "x2", 0, 
  "x3", 0,
  "(Intercept)", 0
)

set.seed(19920401)
data_train <- tibble(
  x1 = runif(1000),
  x2 = 2 + x1 + rnorm(1000, 0, 0.001), 
  x3 = 3 + x1 + x2 + rnorm(1000, 0, 0.001),
  y = 3 * x1 + rnorm(1000, 0, 0.2) 
)

cor(data_train)

# let's estimate an ordinary least squares regression model and a LASSO 
# regression model to see what happens to the estimated coefficients
data_rec <- 
  recipe(y ~ ., data = data_train)

set.seed(20211102)
folds <- vfold_cv(data = data_train, v = 10, repeats = 3)


# lm ----------------------------------------------------------------------

# create a linear regression model object with "lm" as the engine by modifying the line below
lm_mod <- ### FILL IN HERE ###

# create a workflow by modifying the line below
lm_wf <- ### FILL IN HERE ###

# this function is needed to extract the estimated coefficients from each 
# resample
get_lm_coefs <- function(x) {
  
  x %>% 
    extract_fit_engine() %>% 
    tidy()
  
}

tidy_ctrl <- control_grid(extract = get_lm_coefs)

# estimate the models
lm_cv <- lm_wf %>%
  fit_resamples(resamples = folds, control = tidy_ctrl)


# LASSO -------------------------------------------------------------------

# create a grid of lambdas
lasso_grid <- grid_regular(penalty(), levels = 50)

# create a linear regression model object that can be tuned for penalty 
# with the appropriate mixture value for lasso by modifying the line below
lasso_mod <- ### FILL IN HERE ### %>%
  set_engine("glmnet", path_values = lasso_grid$penalty)

# create a workflow by modifying the line below
lasso_wf <- ### FILL IN HERE ###

# this function is needed to extract the estimated coefficients from each 
# resample
get_glmnet_coefs <- function(x) {
  
  x %>% 
    extract_fit_engine() %>% 
    tidy(return_zeros = TRUE) %>% 
    rename(penalty = lambda)
  
}

parsnip_ctrl <- control_grid(extract = get_glmnet_coefs)

# estimate the models using tune_grid, setting the control argument to parsnip_ctrl
# and any other arguments that are needed by modifying the line below
lasso_cv <- ### FILL IN HERE ###


# compare models ----------------------------------------------------------

# the predictions are comparable
bind_rows(
  lm = filter(collect_metrics(lm_cv), .metric == "rmse"),
  LASSO = show_best(lasso_cv, n = 1),
  .id = "model"
)

# extract lm coefficients
lm_coefs <- lm_cv %>% 
  select(id, .extracts) %>% 
  unnest(.extracts)  %>% 
  unnest(.extracts) 

plot1 <- ggplot() +
  geom_point(
    data = lm_coefs,
    mapping = aes(estimate, term), 
    alpha = 0.1
  ) +
  geom_point(
    data = true_coefs,
    mapping = aes(estimate, term), 
    color = "red"
  ) +
  labs(title = "lm has unstable and biased coefficients") +
  theme_minimal()

# extract coefficients
lasso_coefs <- 
  lasso_cv %>% 
  select(id, .extracts) %>% 
  unnest(.extracts) %>% 
  select(id, .extracts) %>% 
  unnest(.extracts) %>%
  group_by(id, term, penalty) %>%
  slice(1) %>%
  ungroup()

plot2 <- ggplot() +
  geom_point(
    data = lasso_coefs,
    mapping = aes(estimate, term), 
    alpha = 0.1
  ) +
  geom_point(
    data = true_coefs,
    mapping = aes(estimate, term), 
    color = "red"
  ) +
  labs(title = "LASSO manages to recreate the true coefficients") +
  theme_minimal()

plot1 + plot2

