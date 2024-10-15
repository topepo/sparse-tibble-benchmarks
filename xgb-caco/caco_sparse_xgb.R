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

convert_to_sparse <- function(.data, threshold = 25, exclude = "class", verbose = FALSE) {
  if (verbose) {
    cli::cli_inform("Initial size: {format(lobstr::obj_size(.data))}")
  }
  num_vals <- map_int(.data, vctrs::vec_unique_count)
  col_types <- map_chr(.data, ~ typeof(.x))
  # fun fact typeof(factor) == "integer" yay!
  convertable <- map_lgl(.data, ~ is.numeric(.x) | is.logical(.x) | is.character(.x))
  
  rm_cols <- names(num_vals) %in% exclude | !convertable
  num_vals <- num_vals[!rm_cols]
  col_types <- col_types[!rm_cols]
  
  cols_to_convert <- names(num_vals)[num_vals <= threshold]
  types_to_convert <- col_types[num_vals <= threshold]
  
  tbl <- table(types_to_convert)
  type_count <- paste0(names(tbl), " (", format(tbl, big.mark = ","), ")")
  
  if (verbose) {
    cli::cli_inform("Columns to convert: {type_count}")
  }
  
  for (.col in cols_to_convert) {
    if (typeof(.data[[.col]]) == "integer") {
      .data[[.col]] <- as_sparse_integer(.data[[.col]])
    } else if (typeof(.data[[.col]]) == "double") {
      .data[[.col]] <- as_sparse_double(.data[[.col]])
    }
  }
  if (verbose) {
    cli::cli_inform("Final size: {format(lobstr::obj_size(.data))}")
  }
  .data
}

caco_sprs <- convert_to_sparse(caco_tbl, threshold = 25)

# ------------------------------------------------------------------------------

caco_data <- caco_sprs

xgb_prof_sprs <- syrup(
  analysis_time_sprs <- system.time(
    source("xgb-caco/caco_xgb.R")
  ),
  interval = 0.1
)

worker_ppid <- ps::ps_pid()
xgb_prof_sprs<- 
  xgb_prof_sprs %>% 
  mutate(
    encoding = "sparse",
    model = "xgb",
    rel_time = difftime(time, min(time))
  ) %>%
  filter(ppid == worker_ppid | pid == worker_ppid)

xgb_mtr_sprs <- xgb_mtr %>% mutate(encoding = "sparse")

xgb_time_sprs <- 
  analysis_time_sprs %>% 
  enframe() %>% 
  mutate(
    encoding = "sparse",
    model = "xgboost"
  ) %>% 
  filter(name == "elapsed")

save(xgb_prof_sprs, xgb_time_sprs, xgb_mtr_sprs, file = "xgb-caco/sparse.RData")

# ------------------------------------------------------------------------------

sessioninfo::session_info()

if (!interactive()) {
  q("no")
}
