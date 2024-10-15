library(tidymodels)
library(bonsai)
library(sparsevctrs)
library(QSARdata)
library(lobstr)
library(syrup)

# ------------------------------------------------------------------------------

tidymodels_prefer()
theme_set(theme_bw())
options(pillar.advice = FALSE, pillar.min_title_chars = Inf)

# ------------------------------------------------------------------------------

data(caco)

caco_tbl <- 
  full_join(caco_AtomPair, caco_PipelinePilot_FP, by = "Molecule") %>% 
  as_tibble()

# Add outcome
caco_tbl <- 
  full_join(caco_tbl, caco_Outcome, by = "Molecule") %>% 
  select(-Molecule) %>% 
  mutate(Class = factor(as.character(Class), ordered = FALSE, levels = levels(Class)))

# ------------------------------------------------------------------------------

caco_data <- caco_tbl

glmnet_prof_dns <- syrup(
  analysis_time_dns <- system.time(
    source("glmnet-caco/caco_glmnet.R")
  ),
  interval = 0.1
)

worker_ppid <- ps::ps_pid()
glmnet_prof_dns<- 
  glmnet_prof_dns %>% 
  mutate(
    encoding = "dense",
    model = "glmnet",
    rel_time = difftime(time, min(time))
  ) %>%
  filter(ppid == worker_ppid | pid == worker_ppid)

glmnet_time_dns <- 
  analysis_time_dns %>% 
  enframe() %>% 
  mutate(
    encoding = "dense",
    model = "glmnet"
  ) %>% 
  filter(name == "elapsed")

glmnet_mtr_dns <- glmnet_mtr %>% mutate(encoding = "dense")

save(glmnet_prof_dns, glmnet_time_dns, glmnet_mtr_dns, file = "glmnet-caco/dense.RData")

# ------------------------------------------------------------------------------

sessioninfo::session_info()

if (!interactive()) {
  q("no")
}
