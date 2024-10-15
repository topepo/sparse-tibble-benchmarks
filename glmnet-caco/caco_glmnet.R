set.seed(183)
caco_split <- initial_split(caco_data, strata = Class)
caco_tr <- training(caco_split)
caco_te <- testing(caco_split)
caco_rs <- vfold_cv(caco_tr, strata = Class)

# ------------------------------------------------------------------------------

mod_spec <- 
  multinom_reg(penalty = 0.01, mixture = 1.0) %>% 
  set_engine("glmnet")

rec <- 
  recipe(Class ~ ., data = caco_tr) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors())

# Avoid (for now) centering./scaling converting the data to dense
# mod_wflow <- workflow(rec, mod_spec)
mod_wflow <- 
  workflow() %>% 
  add_model(mod_spec) %>% 
  add_variables(predictors = everything(), outcomes = Class)

set.seed(610)
mod_fit <- fit(mod_wflow, data = caco_tr)
pred_test <- augment(mod_fit, caco_te) 
brier_test <- pred_test %>% brier_class(Class, .pred_L, .pred_M, .pred_H)

set.seed(610)
mod_res <-
  mod_wflow %>%
  fit_resamples(resamples = caco_rs,
                metrics = metric_set(roc_auc, accuracy, brier_class))

glmnet_mtr <- collect_metrics(mod_res)


