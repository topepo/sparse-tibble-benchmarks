library(tidymodels)
library(patchwork)

# ------------------------------------------------------------------------------

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE, pillar.min_title_chars = Inf)

# ------------------------------------------------------------------------------

load("xgb-caco/dense.RData")
load("xgb-caco/sparse.RData")

load("glmnet-caco/dense.RData")
load("glmnet-caco/sparse.RData")

# ------------------------------------------------------------------------------

p_glmnet_caco <- 
  bind_rows(glmnet_prof_dns, glmnet_prof_sprs) %>% 
  mutate(rel_time = as.numeric(rel_time)) %>% 
  ggplot(aes(rel_time, rss, col = encoding)) + 
  geom_line(linewidth = 1, alpha = 3 / 4) +
  labs(y = "Resident Set Size (RSS)", x = "Execution Time (s)",
       title = "glmnet") +
  scale_color_brewer(palette = "Set2") +
  theme(legend.position = "none")

p_xgb_caco <- 
  bind_rows(xgb_prof_dns, xgb_prof_sprs) %>% 
  mutate(rel_time = as.numeric(rel_time)) %>% 
  ggplot(aes(rel_time, rss, col = encoding)) + 
  geom_line(linewidth = 1, alpha = 3 / 4) +
  labs(y = NULL, x = "Execution Time (s)", title = "xgboost") +
  scale_color_brewer(palette = "Set2")

p_glmnet_caco + p_xgb_caco 

