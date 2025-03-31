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

# set global parameters determining the model fitting process
FIT_MODEL <- TRUE # TRUE will fit model to data, FALSE will skip model fitting
FIT_ALLDATA <- TRUE # TRUE will fit model to all participants, FALSE will fit model to a subset of participants
FIT_CLUSTER <- FALSE # TRUE will fit model on high performance computing cluster (hard-coded and may require adjustments)

ITER_WARMUP <- 2000 # Set warm up iterations for later models
ITER_SAMPLING <- 10000 # Set sampling iterations for later models

NUM_CORES <- parallel::detectCores() - 1 # Set cores to use for computation

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
