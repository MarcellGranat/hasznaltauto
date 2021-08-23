library(tidyverse)
library(tidymodels)

load("C:/rprojects/hasznaltauto/tsm_models_output.RData")

cars_data <- cars_data %>% 
  select(c("vetelar", important_features[1:15]))

lm_pred <- lm_fit %>% 
  predict(new_data = cars_data)

mars_pred <- mars_fit %>% 
  predict(new_data = cars_data)

lasso_pred <- lasso_fit %>% 
  predict(new_data = cars_data)

tree_pred <- tree_fit %>% 
  predict(new_data = cars_data)

rand_forest_pred <- rand_forest_fit %>% 
  predict(new_data = cars_data)

xgboost_pred <- xgboost_fit %>% 
  predict(new_data = cars_data)

cars_pred <- list(select(cars_data, vetelar), lm_pred, mars_pred, lasso_pred, tree_pred, rand_forest_pred, xgboost_pred) %>% 
  reduce(cbind) %>% 
  set_names("vetelar", "lm_pred", "mars_pred", "lasso_pred", "tree_pred", "rand_forest_pred", "xgboost_pred") %>% 
  tibble()

save(list = 'cars_pred', file = "C:/rprojects/hasznaltauto/tsm_models_preds.RData")
