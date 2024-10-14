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

caco_data <- caco_caco_tbl

res_syrup_dns <- syrup(
  analysis_time_dns <- system.time(
    source("xgb-caco/caco_xgb.R")
  )
)

res_syrup_dns <- 
  res_syrup_dns %>% 
  mutate(encoding = "dense")


save(res_syrup_dns, analysis_time_dns, file = "dense.RData")

if (!interactive()) {
  q("no")
}
