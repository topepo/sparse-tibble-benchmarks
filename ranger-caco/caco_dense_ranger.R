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

ranger_prof_dns <- syrup(
  analysis_time_dns <- system.time(
    source("ranger-caco/caco_ranger.R")
  ),
  interval = 0.1
)

worker_ppid <- ps::ps_pid()
ranger_prof_dns<- 
  ranger_prof_dns %>% 
  mutate(
    encoding = "dense",
    model = "ranger",
    rel_time = difftime(time, min(time))
  ) %>%
  filter(ppid == worker_ppid | pid == worker_ppid)

ranger_mtr_dns <- ranger_mtr %>% mutate(encoding = "dense")

ranger_time_dns <- 
  analysis_time_dns %>% 
  enframe() %>% 
  mutate(
    encoding = "dense",
    model = "ranger"
  ) %>% 
  filter(name == "elapsed")

save(ranger_prof_dns, ranger_time_dns, ranger_mtr_dns, file = "ranger-caco/dense.RData")

# ------------------------------------------------------------------------------

sessioninfo::session_info()

if (!interactive()) {
  q("no")
}
