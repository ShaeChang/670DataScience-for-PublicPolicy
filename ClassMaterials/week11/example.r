# load packages
library(palmerpenguins)
library(tidyverse)
library(tidysynth)

# create a practice data set
penguins_complete <- penguins %>%
  select(-year) %>%
  filter(complete.cases(.)) %>%
  mutate(flipper_length_mm = as.numeric(flipper_length_mm),
         body_mass_g = as.numeric(body_mass_g))











# create "starting data"
set.seed(1)

starting_data <- penguins_complete %>% 
  select(species, island, sex) %>%
  slice_sample(n = nrow(penguins_complete), replace = TRUE)
  

# visit_sequence ----------------------------------------------------------
# create a synthesis order based on correlation with bill_length_mm
visit_sequence <- visit_sequence(conf_data = penguins_complete,
                                 start_data = starting_data,
                                 type = "correlation",
                                 cor_var = "bill_length_mm")



# roadmap -----------------------------------------------------------------
# create an object that is the basis for all subsequent operations
roadmap <- roadmap(conf_data = penguins_complete,
                   start_data = starting_data,
                   visit_sequence = visit_sequence)




# synth_spec --------------------------------------------------------------
# use library(parsnip) and library(recipes) to create specifications for
# each variable
rpart_mod <- parsnip::decision_tree() %>% 
  parsnip::set_engine("rpart")

synth_spec <- synth_spec(roadmap = roadmap,
                         synth_algorithms = rpart_mod,
                         recipes = construct_recipes(roadmap = roadmap),
                         predict_methods = sample_rpart)

# noise -------------------------------------------------------------------
# don't add extra noise to predictions
noise <- noise(roadmap = roadmap,
               add_noise = FALSE,
               exclusions = 0)


# constraints -------------------------------------------------------------
# don't impose constraints
constraints <- constraints(roadmap = roadmap,
                           constraints = NULL,
                           max_z = 0)


# replicates --------------------------------------------------------------
# only generate one synthetic data set
replicates <- replicates(replicates = 1,
                         workers = 1,
                         summary_function = NULL)


# presynth ----------------------------------------------------------------
presynth1 <- presynth(roadmap = roadmap,
                      synth_spec = synth_spec,
                      noise = noise, 
                      constraints = constraints,
                      replicates = replicates)


# synthesize --------------------------------------------------------------
set.seed(1)
synth1 <- synthesize(presynth1)


# evaluate results --------------------------------------------------------
bind_rows(
  "confidential" = penguins_complete,
  "synthetic" = synth1$synthetic_data,
  .id = "source"
) %>%
  select(source, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>%
  pivot_longer(-source, names_to = "variable") %>%
  ggplot(aes(value, fill = source)) +
  geom_density(alpha = 0.3, color = NA) +
  facet_wrap(~variable, scales = "free") +
  labs(title = "The Simple Synthesis Recreates Univariate Distributions") +
  theme_minimal()



