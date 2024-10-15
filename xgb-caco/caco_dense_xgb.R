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

xgb_prof_dns <- syrup(
  analysis_time_dns <- system.time(
    source("xgb-caco/caco_xgb.R")
  ),
  interval = 0.1
)

worker_ppid <- ps::ps_pid()
xgb_prof_dns<- 
  xgb_prof_dns %>% 
  mutate(
    encoding = "dense",
    model = "xgb",
    rel_time = difftime(time, min(time))
  ) %>%
  filter(ppid == worker_ppid | pid == worker_ppid)

xgb_mtr_dns <- xgb_mtr %>% mutate(encoding = "dense")

xgb_time_dns <- 
  analysis_time_dns %>% 
  enframe() %>% 
  mutate(
    encoding = "dense",
    model = "xgboost"
  ) %>% 
  filter(name == "elapsed")

save(xgb_prof_dns, xgb_time_dns, xgb_mtr_dns, file = "xgb-caco/dense.RData")

# ------------------------------------------------------------------------------

sessioninfo::session_info()

if (!interactive()) {
  q("no")
}
