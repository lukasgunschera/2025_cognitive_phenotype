## ============================================================================================================================ ##
## Script:    COMBINE PROCESSED AND MODELING DATA
## ============================================================================================================================ ##
## Authors:   Lukas J. Gunschera
## Date:      Fri Apr 26 11:55:04 2024
## ============================================================================================================================ ##
##
## ============================================================================================================================ ##

## SETUP ====================================================================================================================

library(renv)
renv::restore()

set.seed(777) # set seed for random processes

# load required packages
library(here)
library(purrr)
library(psych)
library(readr)
library(haven)
library(tidyr)
library(dplyr)
library(tibble)
library(ggplot2)
library(ggpmisc)
library(viridis)
library(validate)
library(magrittr)
library(patchwork)
library(ggcorrplot)
library(errorlocate)
library(viridisLite)
library(PupillometryR)

# set directory
here::i_am("renv.lock") # set directory

# load custom functions
source(here::here("code", "functions", "fun_plots.R"))
source(here::here("code", "functions", "fun_helper.R"))
source(here::here("code", "functions", "fun_convergence_check.R"))
source(here::here("code", "functions", "fun_load_model_results.R"))

## LOAD PROCESSED DATA ======================================================================================================
# dd_data             = contains behavioural task data (list)
# dd_w02 ... dd_w06   = contains behavioural task data for each wave (df)
# datq05 ... datq07   = contains questionnaire results (processed in lcid_dd_preprocessing.R) (df)
# dat_qes             = contains all questionnaire data across waves (df)
# dat_dem             = contains all demographics data (df)
# dat_master          = contains merged questionnaire and demographics data (df)
# mod_results         = contains dataframes of each measurement wave model parameters (list)

### Behavioural task data ---------------------------------------------------------------------------------------------------
dd_data <- readRDS(here("data", "processed", "dd_task.Rds"))
dd_w02 <- dd_data[[1]]
dd_w03 <- dd_data[[2]]
dd_w04 <- dd_data[[3]]
dd_w05 <- dd_data[[4]]
dd_w06 <- dd_data[[5]]

### Questionnaire & Demographic Data ----------------------------------------------------------------------------------------
dat_master <- read.csv(file = here::here("data", "processed", "masterfile.csv"), header = TRUE)

## CREATE MASTERDAT (TVAR) ==================================================================================================
# time-variant refers to the delay discounting parameter k

mod_results <- load_model_results(data_location = "local", folder = "modelfit", load_local = TRUE)

dd_hyperbo_params_02 <- mod_results$dd_hyperbo_params_02$individual_params %>%
  tidyr::pivot_wider(names_from = parameter, values_from = c(estimate, hdi_lower, hdi_upper)) %>%
  dplyr::mutate(subjID = subj_id) %>%
  dplyr::select(subjID, everything(), -subj_id) %>%
  dplyr::rename_with(~ paste0("w02_", .), -subjID)

dd_hyperbo_params_03 <- mod_results$dd_hyperbo_params_03$individual_params %>%
  tidyr::pivot_wider(names_from = parameter, values_from = c(estimate, hdi_lower, hdi_upper)) %>%
  dplyr::mutate(subjID = subj_id) %>%
  dplyr::select(subjID, everything(), -subj_id) %>%
  dplyr::rename_with(~ paste0("w03_", .), -subjID)

dd_hyperbo_params_04 <- mod_results$dd_hyperbo_params_04$individual_params %>%
  tidyr::pivot_wider(names_from = parameter, values_from = c(estimate, hdi_lower, hdi_upper)) %>%
  dplyr::mutate(subjID = subj_id) %>%
  dplyr::select(subjID, everything(), -subj_id) %>%
  dplyr::rename_with(~ paste0("w04_", .), -subjID)

dd_hyperbo_params_05 <- mod_results$dd_hyperbo_params_05$individual_params %>%
  tidyr::pivot_wider(names_from = parameter, values_from = c(estimate, hdi_lower, hdi_upper)) %>%
  dplyr::mutate(subjID = subj_id) %>%
  dplyr::select(subjID, everything(), -subj_id) %>%
  dplyr::rename_with(~ paste0("w05_", .), -subjID)

dd_hyperbo_params_06 <- mod_results$dd_hyperbo_params_06$individual_params %>%
  tidyr::pivot_wider(names_from = parameter, values_from = c(estimate, hdi_lower, hdi_upper)) %>%
  dplyr::mutate(subjID = subj_id) %>%
  dplyr::select(subjID, everything(), -subj_id) %>%
  dplyr::rename_with(~ paste0("w06_", .), -subjID)

ddtvar <- dd_hyperbo_params_02 %>%
  dplyr::left_join(., dd_hyperbo_params_03, by = "subjID") %>%
  dplyr::left_join(., dd_hyperbo_params_04, by = "subjID") %>%
  dplyr::left_join(., dd_hyperbo_params_05, by = "subjID") %>%
  dplyr::left_join(., dd_hyperbo_params_06, by = "subjID") %>%
  dplyr::left_join(., dat_master, by = "subjID") %>%
  # drop columns that are all NA
  dplyr::select(where(~ !all(is.na(.))))

# remove trailing "_c" from age columns
ddtvar %<>%
  dplyr::rename_with(~ str_replace_all(., "_c", ""), contains("age"))

# add log-transformed delay discounting parameter k for each wave
ddtvar %<>%
  dplyr::mutate(
    w02_logk = -log(w02_estimate_k),
    w02_logk_hdi_lower = -log(w02_hdi_lower_k),
    w02_logk_hdi_upper = -log(w02_hdi_upper_k),
    w03_logk = -log(w03_estimate_k),
    w03_logk_hdi_lower = -log(w03_hdi_lower_k),
    w03_logk_hdi_upper = -log(w03_hdi_upper_k),
    w04_logk = -log(w04_estimate_k),
    w04_logk_hdi_lower = -log(w04_hdi_lower_k),
    w04_logk_hdi_upper = -log(w04_hdi_upper_k),
    w05_logk = -log(w05_estimate_k),
    w05_logk_hdi_lower = -log(w05_hdi_lower_k),
    w05_logk_hdi_upper = -log(w05_hdi_upper_k),
    w06_logk = -log(w06_estimate_k),
    w06_logk_hdi_lower = -log(w06_hdi_lower_k),
    w06_logk_hdi_upper = -log(w06_hdi_upper_k)
  ) %>%
  dplyr::select(subjID, contains("logk"), contains("_k"), everything())

# create long dataframe for plotting
ddtvar_long <- ddtvar %>%
  tidyr::pivot_longer(
    cols = contains("w0"),
    names_to = c("wave", ".value"),
    names_pattern = "w(\\d+)_(.*)"
  ) %>%
  dplyr::select(subjID, wave, everything()) %>%
  dplyr::mutate(wave = as.numeric(wave)) %>%
  dplyr::arrange(subjID, wave) %>%
  dplyr::select(subjID, wave, contains("logk"), contains("_k"), everything())

## SAVE MASTERDAT ===========================================================================================================
write.csv(ddtvar, file = here::here("data", "processed", "tvar_masterdat_wide.csv"))
write.csv(ddtvar_long, file = here::here("data", "processed", "tvar_masterdat_long.csv"))

## ERROR CHECKS =============================================================================================================

#### Check age ranges -------------------------------------------------------------------------------------------------------
summary(validate::confront(
  ddtvar,
  validate::validator(
    r1 = w01_age >= 6.75,
    r2 = w02_age >= 7.75,
    r3 = w03_age >= 8.75,
    r4 = w04_age >= 9.75,
    r5 = w05_age >= 10.75,
    r6 = w06_age >= 11.75,
    r7 = w07_age >= 12.75
  )
))

## AUXILIARY VARIABLES ======================================================================================================

ddtvar_wide <- ddtvar %>%
  mutate(
    w05_missing = is.na(w05_sm_total),
    w06_missing = is.na(w06_sm_total),
    w07_missing = is.na(w07_sm_total)
  ) %>%
  mutate(
    w05_missing = as.integer(as.logical(w05_missing)),
    w06_missing = as.integer(as.logical(w06_missing)),
    w07_missing = as.integer(as.logical(w07_missing))
  )

# Initialise colnames object for each wave
wave_05_columns <- grep("^w05", names(ddtvar_wide), value = TRUE)
wave_06_columns <- grep("^w06", names(ddtvar_wide), value = TRUE)
wave_07_columns <- grep("^w07", names(ddtvar_wide), value = TRUE)

### Logistic Regressions ----------------------------------------------------------------------------------------------------

auxvars <- misty::na.auxiliary(ddtvar_wide, tri = "lower", digits = 3)

# Add rowname to create correlation matrix
aux_tbl <- auxvars$result$cor %>%
  as_tibble() %>%
  add_column(var_id = auxvars$result$cor %>% row.names())

#### Missingness predictor --------------------------------------------------------------------------------------------------

aux_miss <- aux_tbl %>%
  pivot_longer(
    cols = contains("_missing"),
    names_to = "missing",
    values_to = "value"
  ) %>%
  mutate(
    value = abs(value)
  ) %>%
  select(missing, var_id, value) %>%
  filter(!str_detect(var_id, "dropout|missing|participation|hdi|X.1|logk|pds_k|pds_g")) %>%
  arrange(desc(value))

# Remove the wave prefixes from the variable names
aux_miss %<>%
  mutate(
    missing_clean = str_replace(missing, "^(w05_|w06_|w07_)", "")
  )

# Group by the cleaned variable names and calculate the average correlation
aux_miss_corrs <- aux_miss %>%
  group_by(var_id, missing_clean) %>%
  summarise(avg_value = mean(value, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(avg_value))

#### Delay discounting predictors -------------------------------------------------------------------------------------------

aux_log <- aux_tbl %>%
  select(-contains("w02_logk"), -contains("w03_logk"), -contains("w04_logk")) %>%
  pivot_longer(
    cols = matches("_logk(?!.*hdi)", perl = TRUE),
    names_to = "logk",
    values_to = "value"
  ) %>%
  mutate(
    value = abs(value)
  ) %>%
  filter(!str_detect(var_id, "dropout|missing|participation|hdi|X.1|estimate|logk|pds_k|pds_g")) %>%
  select(logk, var_id, value) %>%
  arrange(desc(value))

# Remove the wave prefixes from the variable names
aux_log %<>%
  mutate(
    log_clean = str_replace(logk, "^(w05_|w06_|w07_)", "")
  )

# Group by the cleaned variable names and calculate the average correlation
aux_log_corrs <- aux_log %>%
  group_by(var_id, log_clean) %>%
  summarise(avg_value = mean(value, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(avg_value))

#### Social media use predictors --------------------------------------------------------------------------------------------

aux_sm <- aux_tbl %>%
  pivot_longer(
    cols = contains("sm_postandscroll"),
    names_to = "sm_postandscroll",
    values_to = "value"
  ) %>%
  mutate(
    value = abs(value)
  ) %>%
  filter(!str_detect(var_id, "dropout|missing|participation|hdi|X.1|sm|logk|estimate|pds_k|pds_g")) %>%
  select(sm_postandscroll, var_id, value) %>%
  arrange(desc(value))

# Remove the wave prefixes from the variable names
aux_sm %<>%
  mutate(
    sm_clean = str_replace(sm_postandscroll, "^(w05_|w06_|w07_)", "")
  )

# Group by the cleaned variable names and calculate the average correlation
aux_sm_corrs <- aux_sm %>%
  group_by(var_id, sm_clean) %>%
  summarise(avg_value = mean(value, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(avg_value))

combined_aux <- aux_sm_corrs %>%
  rename(avg_value_sm = avg_value) %>%
  full_join(aux_miss_corrs %>% rename(avg_value_miss = avg_value), by = c("var_id")) %>%
  full_join(aux_log_corrs %>% rename(avg_value_logk = avg_value), by = c("var_id"))

combined_aux <- combined_aux %>%
  rowwise() %>%
  mutate(
    avg_correlation = mean(c(avg_value_sm, avg_value_miss, avg_value_logk), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(avg_correlation, var_id) %>%

  # remove indicators that are subject-specific or duplicates (e.g., pds items specific to girls or boys)
  filter(!str_detect(var_id, "sex_c|pds_b|pds_g")) %>%
  arrange(desc(avg_correlation)) %>%
  filter(avg_correlation > .1)


aux_vars <- combined_aux %>% slice_head(n = 10) %>% pull(var_id) %>% as.character(.)
aux_vars_sm <- aux_sm_corrs %>%  slice_head(n = 10) %>% pull(var_id) %>% as.character(.)
aux_vars_miss <- aux_miss_corrs %>% slice_head(n = 10) %>% pull(var_id) %>% as.character(.)
aux_vars_log <- aux_log_corrs %>%  slice_head(n = 10) %>% pull(var_id) %>% as.character(.)

union(aux_vars, aux_vars_sm) %>%
  union(.,  aux_vars_miss) %>%
  union(., aux_vars_log)

# Save resulting variables in vector to be called in 'lcid_mplus_processing.R'
aux_variables <- c("w05_cius_total", "w06_cius_total", "w05_sex", "ses", "w01_education_op",
                   "w06_bsi_total", "w06_bsi_depression", "w06_bsi_anxiety",
                   "w07_bsi_total", "w07_bsi_depression", "w07_bsi_anxiety",
                   "w07_pds_2", "w05_pds_total", "w06_pds_total", "w07_pds_total")

saveRDS(aux_variables, here::here("data", "processed", "auxiliary_variables.rds"))

## ======================================================================================================================= ##
# The selection of the auxiliary variables is determined by the above logistic regressions. The selection criteria applied are
# arbitrary and based on a) the average correlation between the construct and the variables included in the model, b) parsimony,
# and c) measurement consistency (is the measure available for waves 5, 6, and 7).
#
# These variables are included as 'Auxiliary' in all MPLUS models to follow, and serve to make the FIML computation more accurate
## ======================================================================================================================= ##

