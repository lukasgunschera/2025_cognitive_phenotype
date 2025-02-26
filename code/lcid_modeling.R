## ======================================================================================================================= ##
## Script:       MODEL DELAY DISCOUNTING HYPERBOLIC FUNCTION
## ======================================================================================================================= ##
## Authors:      Lukas Gunschera
## Contact:      l.gunschera@outlook.com
##
## Date created: 2024-07-12
## ======================================================================================================================= ##
##
## Prior to executing this script ensure to set parameters FIT_MODEL, FIT_ALLDATA, and FIT_CLUSTER to appropriate values.
## FIT_MODEL set to true executes script, set to false skips model fitting.
## FIT_ALLDATA set to true fits model to all participants, set to false fits model to a subset of participants.
## FIT_CLUSTER set to true fits model on high performance computing cluster, set to false fits model on local machine.
##
## Importantly, FIT_CLUSTER == TRUE is hard-coded and may require changes to path for the stan executable.
##
## ======================================================================================================================= ##

## SETUP ====================================================================================================================

library(renv)
renv::restore()

set.seed(777) # set seed for random processes

# load required packages
library(loo)
library(here)
library(purrr)
library(readr)
library(haven)
library(tidyr)
library(dplyr)
library(hexbin)
library(stringr)
library(ggplot2)
library(ggpmisc)
library(viridis)
library(parallel)
library(magrittr)
library(cmdstanr)
library(hBayesDM)
library(bayesplot)
library(patchwork)
library(ggcorrplot)
library(viridisLite)

# set global parameters determining the model fitting process
FIT_MODEL   <- TRUE     # TRUE will fit model to data, FALSE will skip model fitting
FIT_ALLDATA <- TRUE     # TRUE will fit model to all participants, FALSE will fit model to a subset of participants
FIT_CLUSTER <- FALSE    # TRUE will fit model on high performance computing cluster (hard-coded and may require adjustments)

ITER_WARMUP <- 2000     # Set warm up iterations for later models
ITER_SAMPLING <- 10000  # Set sampling iterations for later models

NUM_CORES <- parallel::detectCores() - 1  # Set cores to use for computation

# set directory
here::i_am("renv.lock") # set directory

# load custom functions
source(here::here("code", "functions", "fun_plots.R"))
source(here::here("code", "functions", "fun_helper.R"))
source(here::here("code", "functions", "fun_get_params.R"))
source(here::here("code", "functions", "fun_convergence_check.R"))
source(here::here("code", "functions", "fun_model_preprocessing.R"))
source(here::here("code", "functions", "fun_posterior_predictions.R"))

#### Load data --------------------------------------------------------------------------------------------------------------

dd_data <- readRDS(here::here("data", "processed", "dd_task.Rds")) # task data loading

# @k      = discounting rate
# @beta   = inverse temperature
# FIT_ALLDATA == FALSE == subset three subjects from dataframe to illustrate modeling procedure
# FIT_ALLDATA == TRUE  == use entire dataset and continue with full modeling procedure

task_data_02 <- subset_x_participants(dd_data[[1]], FIT_ALLDATA, participants = 5)
task_data_03 <- subset_x_participants(dd_data[[2]], FIT_ALLDATA, participants = 5)
task_data_04 <- subset_x_participants(dd_data[[3]], FIT_ALLDATA, participants = 5)
task_data_05 <- subset_x_participants(dd_data[[4]], FIT_ALLDATA, participants = 5)
task_data_06 <- subset_x_participants(dd_data[[5]], FIT_ALLDATA, participants = 5)

### Load Model from Stan ----------------------------------------------------------------------------------------------------

if (FIT_MODEL && FIT_CLUSTER) {

  # set cmd_stan installation path to location on cluster (change as required)
  set_cmdstan_path("/group/orben/software/linux/cmdstan/cmdstan-2.33.1")

  # load model (Linux)
  dd_hyperbolic_stan <- cmdstanr::cmdstan_model(
    here::here("code", "stan", "linux", "dd_hyperbolic.stan"))

  print("MODEL WILL RUN ON CLUSTER")

} else if (FIT_MODEL && !FIT_CLUSTER) {

  # load (Mac & Windows)
  dd_hyperbolic_stan <- cmdstanr::cmdstan_model(
    here::here("code", "stan", "dd_hyperbolic.stan"))

  print("MODEL WILL RUN ON LOCAL MACHINE, CODE EXECUTION MAY TAKE LONGER")

} else {

  # output message
  print("DID NOT RUN MODELS, YOU CAN LOAD RESULTS FROM FILE")
}

## Process Model Data =======================================================================================================

process_task_data <- function(task_data) {
  model_preprocessing(
    raw_data = task_data,
    retest = FALSE,
    subjs = unique(task_data$subjID),
    n_subj = length(unique(task_data$subjID)),
    t_subjs = aggregate(trial ~ subjID, FUN = max, data = task_data)[, 2],
    t_max = max(aggregate(trial ~ subjID, FUN = max, data = task_data)[, 2])
  )
}

# arrange data for stan modeling
dd_model_dat_02 <- process_task_data(task_data_02)
dd_model_dat_03 <- process_task_data(task_data_03)
dd_model_dat_04 <- process_task_data(task_data_04)
dd_model_dat_05 <- process_task_data(task_data_05)
dd_model_dat_06 <- process_task_data(task_data_06)

# extract subject identifiers from data
dd_subj_02 <- unique(task_data_02$subjID)
dd_subj_03 <- unique(task_data_03$subjID)
dd_subj_04 <- unique(task_data_04$subjID)
dd_subj_05 <- unique(task_data_05$subjID)
dd_subj_06 <- unique(task_data_06$subjID)

saveRDS(list(dd_subj_02, dd_subj_03, dd_subj_04, dd_subj_05, dd_subj_06),
        here::here("data", "processed", "dd_subjIDs.RDS"))

## FIT MODELS ===============================================================================================================

if (FIT_MODEL) {

  dd_hyperbo_fit_02 <- dd_hyperbolic_stan$sample(
    data = dd_model_dat_02, iter_warmup = ITER_WARMUP, iter_sampling = ITER_SAMPLING,
    refresh = 0, chains = 4, parallel_chains = 4, adapt_delta = 0.8, step_size = 1, max_treedepth = 10,
    save_warmup = TRUE, output_dir = NULL)

  dd_hyperbo_fit_03 <- dd_hyperbolic_stan$sample(
    data = dd_model_dat_03, iter_warmup = ITER_WARMUP, iter_sampling = ITER_SAMPLING,
    refresh = 0, chains = 4, parallel_chains = 4, adapt_delta = 0.8, step_size = 1, max_treedepth = 10,
    save_warmup = TRUE, output_dir = NULL)

  dd_hyperbo_fit_04 <- dd_hyperbolic_stan$sample(
    data = dd_model_dat_04, iter_warmup = ITER_WARMUP, iter_sampling = ITER_SAMPLING,
    refresh = 0, chains = 4, parallel_chains = 4, adapt_delta = 0.8, step_size = 1, max_treedepth = 10,
    save_warmup = TRUE, output_dir = NULL)

  dd_hyperbo_fit_05 <- dd_hyperbolic_stan$sample(
    data = dd_model_dat_05, iter_warmup = ITER_WARMUP, iter_sampling = ITER_SAMPLING,
    refresh = 0, chains = 4, parallel_chains = 4, adapt_delta = 0.8, step_size = 1, max_treedepth = 10,
    save_warmup = TRUE, output_dir = NULL)

  dd_hyperbo_fit_06 <- dd_hyperbolic_stan$sample(
    data = dd_model_dat_06, iter_warmup = ITER_WARMUP, iter_sampling = ITER_SAMPLING,
    refresh = 0, chains = 4, parallel_chains = 4, adapt_delta = 0.8, step_size = 1, max_treedepth = 10,
    save_warmup = TRUE, output_dir = NULL)

  #### Process results function ---------------------------------------------------------------------------------------------
  process_model <- function(model_fit, task_data, model_dat, wave) {

    check <- convergence_check(model_fit,
                               params = c("k", "beta"),
                               Rhat = TRUE, ess = TRUE,
                               trace_plot = TRUE, rank_hist = FALSE)

    trace_plot <- check$trace_plot
    loo_result <- model_fit$loo()

    parameters <- get_params(subj_id = unique(task_data$subjID),
                             model_fit = model_fit,
                             n_subj = length(unique(task_data$subjID)),
                             n_params = 2, param_names = c("k", "beta"))

    # get model data for current iteration
    dd_model_dat <- get(paste0("dd_model_dat_0", wave))

    # run posterior predictive function
    ppredicts <- posterior_predictions(model_fit = model_fit, real_dat = dd_model_dat,
                                       n_trials = dd_model_dat$T, n_subj = dd_model_dat$N)

    # save results
    saveRDS(list(check$Rhat, check$ess, trace_plot), here::here("output", "modelfit", paste0("dd_hyperbo_check_0", wave, ".RDS")))
    saveRDS(loo_result,here::here("output", "modelfit", paste0("dd_hyperbo_loo_0", wave, ".RDS")))
    saveRDS(parameters, here::here("output", "modelfit", paste0("dd_hyperbo_parameters_0", wave, ".RDS")))
    saveRDS(ppredicts,here::here("output", "modelfit", paste0("dd_hyperbo_ppc_0", wave, ".RDS")))

    # Assign outputs to environment
    assign(paste0("dd_hyperbo_check_0", wave), check, envir = .GlobalEnv)
    assign(paste0("dd_hyperbo_loo_0", wave), loo_result, envir = .GlobalEnv)
    assign(paste0("dd_hyperbo_parameters_0", wave), parameters$individual_params, envir = .GlobalEnv)
    assign(paste0("dd_hyperbo_ppc_0", wave), ppredicts, envir = .GlobalEnv)

  }

  # get Model Results across Waves
  for (wave in 2:6) {
    model_fit <- get(paste0("dd_hyperbo_fit_0", wave))
    task_data <- get(paste0("task_data_0", wave))
    model_dat <- get(paste0("dd_model_dat_0", wave))

    process_model(model_fit = model_fit, task_data = task_data, model_dat = model_dat, wave = wave)
  }

  ### Get model fits --------------------------------------------------------------------------------------------------------
  dd_hyperbo_fits <- list(dd_hyperbo_fit_02, dd_hyperbo_fit_03, dd_hyperbo_fit_04, dd_hyperbo_fit_05, dd_hyperbo_fit_06)

  # read csv files for each model
  model_fits_tot <- lapply(dd_hyperbo_fits, function(fit) {
    cmdstanr::read_cmdstan_csv(
      fit$output_files(),
      variables = c("mu_k", "mu_beta")
    )
  }) # outputs to environment in form of 'model_fits_tot' list

  ### Visualisation ---------------------------------------------------------------------------------------------------------

  # create and save trace plots
  trace_plots <- purrr::imap(model_fits_tot, create_trace_plot)

  # Create and save density plots
  density_plots <- purrr::imap(model_fits_tot, create_density_plot)

} else {

  print("DID NOT RUN MODELS, LOADING FIT INDICES RESULTS FROM FILE")

  dd_hyperbo_check_02 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_check_02.RDS"))
  dd_hyperbo_check_03 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_check_03.RDS"))
  dd_hyperbo_check_04 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_check_04.RDS"))
  dd_hyperbo_check_05 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_check_05.RDS"))
  dd_hyperbo_check_06 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_check_06.RDS"))
}

### Load Model Fit Indices --------------------------------------------------------------------------------------------------

dd_hyperbo_check_02 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_check_02.RDS"))
dd_hyperbo_check_03 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_check_03.RDS"))
dd_hyperbo_check_04 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_check_04.RDS"))
dd_hyperbo_check_05 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_check_05.RDS"))
dd_hyperbo_check_06 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_check_06.RDS"))

### Examine Model Fit -------------------------------------------------------------------------------------------------------

# RHAT extreme across waves

rhat_values <- c(
  dd_hyperbo_check_02$Rhat[[6]], dd_hyperbo_check_03$Rhat[[6]], dd_hyperbo_check_04$Rhat[[6]],
  dd_hyperbo_check_05$Rhat[[6]], dd_hyperbo_check_06$Rhat[[6]]
)

rhat_values[which.max(abs(rhat_values))]

# ESS minimum across waves
min(dd_hyperbo_check_02$ess[[1]], dd_hyperbo_check_03$ess[[1]], dd_hyperbo_check_04$ess[[1]],
    dd_hyperbo_check_05$ess[[1]], dd_hyperbo_check_06$ess[[1]])

### Examine Posterior Predictives -------------------------------------------------------------------------------------------
ppc02 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_ppc_02.RDS"))
ppc03 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_ppc_03.RDS"))
ppc04 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_ppc_04.RDS"))
ppc05 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_ppc_05.RDS"))
ppc06 <- readRDS(here::here("output", "modelfit", "dd_hyperbo_ppc_06.RDS"))

cor(ppc02$mean_pred_choice, ppc02$mean_real_choice);
cor(ppc03$mean_pred_choice, ppc03$mean_real_choice);
cor(ppc04$mean_pred_choice, ppc04$mean_real_choice);
cor(ppc05$mean_pred_choice, ppc05$mean_real_choice);
cor(ppc06$mean_pred_choice, ppc06$mean_real_choice);

ppc02 %>%
  dplyr::group_by(real_delay_later) %>%
  dplyr::summarise(correlation = cor(mean_pred_choice, mean_real_choice))

ppc03 %>%
  dplyr::group_by(real_delay_later) %>%
  dplyr::summarise(correlation = cor(mean_pred_choice, mean_real_choice))

ppc04 %>%
  dplyr::group_by(real_delay_later) %>%
  dplyr::summarise(correlation = cor(mean_pred_choice, mean_real_choice))

ppc05 %>%
  dplyr::group_by(real_delay_later) %>%
  dplyr::summarise(correlation = cor(mean_pred_choice, mean_real_choice))

ppc06 %>%
  dplyr::group_by(real_delay_later) %>%
  dplyr::summarise(correlation = cor(mean_pred_choice, mean_real_choice))

## DESCRIPTIVES =============================================================================================================

data_list <- list(task_data_02, task_data_03, task_data_04, task_data_05, task_data_06)

# get the acceptance proportion for entire task (average of participants)
prop_accept <- map(data_list, ~ .x %>%
      group_by(subjID) %>%
      dplyr::summarise(
        prop_accept = mean(choice)
      ) %>%
      dplyr::summarise(
        mean_accept = format(mean(prop_accept), nsmall = 5),
        sd_accept = format(sd(prop_accept), nsmall = 5)
      ))

# get the acceptance proportion for bins within task
prop_accept_bins <- map(data_list, ~ .x %>%
  ungroup() %>%
  dplyr::mutate(v_diff = amount_later - amount_sooner,
         v_diff_bin = ntile(v_diff, 4)) %>%
  dplyr::group_by(v_diff_bin) %>%
  dplyr::summarise(
    prop_accept = format(mean(choice), nsmall = 5)
  ))

# above trend can be explained by negative association between sooner offer size and delay later
purrr::map(data_list, ~ .x %>%
      ungroup() %>%
      dplyr::summarise(v_corr = format(cor(x = amount_sooner, y = delay_later), nsmall = 5)))
