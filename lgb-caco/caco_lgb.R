set.seed(183)
caco_split <- initial_split(caco_data, strata = Class)
caco_tr <- training(caco_split)
caco_te <- testing(caco_split)
caco_rs <- vfold_cv(caco_tr, strata = Class)

# ------------------------------------------------------------------------------

mod_spec <- 
  boost_tree(trees = 500, learn_rate = 0.1) %>% 
  set_mode("classification") %>% 
  set_engine("lightgbm")

mod_wflow <- 
  workflow() %>% 
  add_model(mod_spec) %>% 
  add_variables(predictors = everything(), outcomes = Class)

set.seed(610)
mod_fit <- fit_xy(mod_spec, x = caco_tr %>% select(-Class), y = caco_tr$Class)
mod_fit <- fit(mod_wflow, data = caco_tr)
pred_test <- augment(mod_fit, caco_te) 
brier_test <- pred_test %>% brier_class(Class, .pred_L, .pred_M, .pred_H)

set.seed(610)
mod_res <-
  mod_wflow %>%
  fit_resamples(resamples = caco_rs,
                metrics = metric_set(roc_auc, accuracy, brier_class))

lgb_mtr <- collect_metrics(mod_res)


