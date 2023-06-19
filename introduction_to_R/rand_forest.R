data = readRDS(url("https://github.com/stepanpaulik/courts_and_judges/raw/main/data/rand_forest_exercise.rds")) %>% drop_na()
# Familiarise yourself with the data
skim(data)
view(data)

# Split the data into testing and training data
data_split = initial_split(data, prop = 3/4)
train_data = training(data_split)
test_data  = testing(data_split)

# Prepare data for k-fold cross-validation
folds = vfold_cv(train_data, v = 6)

# Not all features of this data set are helpful, determine which ones would you like to go in in your formula
rec = recipe(successful ~ #, data = train_data) %>% 
  update_role(#, new_role = "ID") %>%
  update_role(#, new_role = "outcome") %>% 
  update_role(all_predictors(), new_role = "predictor")
rec

mod_tune = rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

wfl_tune = workflow() %>%
  add_recipe(rec) %>%
  add_model(mod_tune)

fit_tune = tune_grid(
  wfl_tune,
  resamples = folds,
  grid = 20
)

# Plot the tuning result
fit_tune %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

best_auc <- select_best(regular_res, "roc_auc")

final_rf <- finalize_model(
  tune_spec,
  best_auc
)

final_rf