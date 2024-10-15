library(tidymodels)
library(patchwork)

# ------------------------------------------------------------------------------

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE, pillar.min_title_chars = Inf)

# ------------------------------------------------------------------------------

load("xgb-caco/dense.RData")
load("xgb-caco/sparse.RData")

load("ranger-caco/dense.RData")
load("ranger-caco/sparse.RData")

load("glmnet-caco/dense.RData")
load("glmnet-caco/sparse.RData")

# ------------------------------------------------------------------------------

bind_rows(glmnet_prof_dns, glmnet_prof_sprs) %>% 
  bind_rows(ranger_prof_dns, ranger_prof_sprs) %>% 
  bind_rows(xgb_prof_dns, xgb_prof_sprs) %>% 
  mutate(rel_time = as.numeric(rel_time)) %>% 
  ggplot(aes(rel_time, rss, col = encoding)) + 
  geom_line(linewidth = 1, alpha = 3 / 4) +
  facet_wrap(~ model, scales = "free_x") +
  labs(y = "Resident Set Size (RSS)", x = "Execution Time (s)") +
  scale_color_brewer(palette = "Set2") +
  theme(legend.position = "top")
