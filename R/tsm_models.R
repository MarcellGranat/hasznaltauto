cars_data <- readRDS("C:/rprojects/hasznaltauto/data/cars_data.RDS")
source('C:/rprojects/hasznaltauto/R/functions.R')
library(tidymodels)

set.seed(123)

cars_data <- cars_data %>%
  na.omit() %>% 
  janitor::clean_names() %>% 
  mutate_if(is.character, factor) %>% 
  select(- url_to_car)

doParallel::registerDoParallel(cores = 6)

initial_cars_cv <- vfold_cv(cars_data, 10, strata = vetelar)

initial_cars_rec <- recipe(vetelar ~ ., data = cars_data) %>% 
  step_string2factor(all_nominal())

tree_spec <- decision_tree(tree_depth = tune(), 
                min_n = tune(), 
                cost_complexity = tune()) %>%
  set_engine('rpart') %>%
  set_mode('regression')

initial_tree_wf <- workflow() %>% 
  add_recipe(initial_cars_rec) %>% 
  add_model(tree_spec)

initial_tree_tune <- tune_grid(
  initial_tree_wf, 
  resamples = initial_cars_cv,
  grid = 18 # TODO INCREASE
)

initial_tree_fit <- finalize_model(tree_spec, select_best(initial_tree_tune, "rsq")) %>%
  fit(formula = vetelar ~., data = cars_data)


important_features <- initial_tree_fit %>% 
  vip::vi() %>% 
  pull(Variable)


# Resample ---------------------------------------------------------------------

message("resample")

cars_split <- cars_data %>% 
  select(c("vetelar", important_features[1:15])) %>% 
  initial_split(.01, strata = vetelar)

cars_training <- training(cars_split) 

cars_training <- cars_data %>% 
  select(c("vetelar", important_features[1:5]))

cars_training_cv <- vfold_cv(cars_training, 10, strata = vetelar)

# RandomForest ----------------------------------------------------------------

message("Forest")

rand_forest_spec <- rand_forest(mtry = tune(), min_n = tune()) %>%
  set_engine('randomForest') %>%
  set_mode('regression')

rand_forest_wf <- workflow() %>% 
  add_formula(vetelar ~ .) %>% 
  add_model(rand_forest_spec)

rand_forest_tune <- tune_grid(
  rand_forest_wf, 
  resamples = cars_training_cv,
  grid = 18 # TODO INCREASE
)

rand_forest_fit <- finalize_model(
  rand_forest_spec,
  select_best(rand_forest_tune, "rsq")
) %>% 
  fit_resamples(vetelar ~ ., data = cars_training_cv)

rand_forest_pred <- finalize_model(
  rand_forest_spec,
  select_best(rand_forest_tune, "rsq")
) %>% 
  fit(vetelar ~ ., data = cars_training)


# MARS ------------------------------------------------------------------------------

message("MARS")

mars_earth_spec <-
  mars(prod_degree = tune()) %>%
  set_engine('earth') %>%
  set_mode('regression')

mars_wf <- rand_forest_wf %>% 
  update_model(mars_earth_spec)

mars_tune <- tune_grid(
  mars_wf, 
  resamples = cars_training_cv,
  grid = 18 # TODO INCREASE
)

mars_fit <- finalize_model(
  mars_earth_spec,
  select_best(mars_tune, "rsq")
) %>% 
  fit_resamples(vetelar ~ ., data = cars_training_cv)

mars_pred <- finalize_model(
  mars_earth_spec,
  select_best(mars_tune, "rsq")
) %>% 
  fit(vetelar ~ ., data = cars_training)


# LASSO -----------------------------------------------------------------------------

message("LASSO")

linear_reg_glmnet_spec <-
  linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine('glmnet')

lasso_wf <- rand_forest_wf %>% 
  update_model(linear_reg_glmnet_spec)

lasso_tune <- tune_grid(
  lasso_wf, 
  resamples = cars_training_cv,
  grid = 18 # TODO INCREASE
)


lasso_fit <- finalize_model(
  linear_reg_glmnet_spec,
  select_best(lasso_tune, "rsq")
) %>% 
  fit_resamples(vetelar ~ ., data = cars_training_cv)

lasso_pred <- finalize_model(
  linear_reg_glmnet_spec,
  select_best(lasso_tune, "rsq")
) %>% 
  fit(vetelar ~ ., data = cars_training)


# Xboost ----------------------------------------------------------------------------

message("xgboost")

boost_tree_xgboost_spec <-
  boost_tree(tree_depth = tune(), trees = tune(), learn_rate = tune(), min_n = tune(), loss_reduction = tune(), sample_size = tune()) %>%
  set_engine('xgboost') %>%
  set_mode('regression')

xgboost_wf <- rand_forest_wf %>% 
  update_model(boost_tree_xgboost_spec)

xgboost_tune <- tune_grid(
  xgboost_wf, 
  resamples = cars_training_cv,
  grid = 18 # TODO INCREASE
)


xgboost_fit <- finalize_model(
  boost_tree_xgboost_spec,
  select_best(xgboost_tune, "rsq")
) %>% 
  fit_resamples(vetelar ~ ., data = cars_trainin_cv)

xgboost_pred <- finalize_model(
  boost_tree_xgboost_spec,
  select_best(xgboost_tune, "rsq")
) %>% 
  fit(vetelar ~ ., data = cars_training)


# lm --------------------------------------------------------------------------------

message("lm")

linear_reg_lm_spec <-
  linear_reg() %>%
  set_engine('lm')

lm_rec <- recipe(vetelar ~ ., data = cars_training) %>% 
  step_dummy(all_nominal())

lm_wf <- workflow() %>% 
  add_model(linear_reg_lm_spec) %>% 
  add_recipe(lm_rec)

lm_tune <- lm_wf %>% 
  fit_resamples(cars_training_cv)

lm_fit <- lm_wf %>% 
  fit(cars_training)


# tree ------------------------------------------------------------------------------

message("tree")

tree_wf <- rand_forest_wf %>% 
  update_model(tree_spec)

tree_tune <- tune_grid(
  tree_wf, 
  resamples = cars_training_cv,
  grid = 18 # TODO INCREASE
)

tree_fit <- finalize_model(
  tree_spec,
  select_best(tree_tune, "rsq")
) %>% 
  fit_resamples(vetelar ~ ., data = cars_training)

tree_pred <- finalize_model(
  tree_spec,
  select_best(tree_tune, "rsq")
) %>% 
  fit(vetelar ~ ., data = cars_training)

save.image(file = "c:/rprojects/hasznaltauto/tsm_models_output.RData")
